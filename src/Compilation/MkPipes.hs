{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}

module Compilation.MkPipes where
-- External
import           Control.Concurrent.Async           (mapConcurrently)
import           Control.Monad                      (unless)
import           Data.List                          (nub)
import           Data.Map.Strict                    (elems, size)
import qualified Data.Map.Strict                    as Map (map, null)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text, unpack)
import           Database.MySQL.Simple              (Connection, Only (..),
                                                     execute, executeMany,
                                                     query_)
import           Debug.Trace                        (traceM)

import           Language.Haskell.TH                (DecsQ, Exp, Q, listE,
                                                     mkName, varE)

-- Internal
import           Compilation.InputFile              (mkInputFile)
import           Compilation.Utilities              (concatMapM, mQuery,
                                                     makeIOMapEither, mergeMaps,
                                                     mkErr, mkErrQuery,
                                                     mkInsQuery, mkNullQuery,
                                                     mkParser, mkPrinter,
                                                     mkUpdateQuery,
                                                     mkUpsertQuery, nullRows,
                                                     removeRepeats, setName,
                                                     splitEitherArgs)
import           PreCompilation.DataTypes.Misc      (ExitStatus (..))
import           PreCompilation.DataTypes.Transform (TType (..), Transform (..))
import           PreCompilation.TH                  (transforms')
import           PreCompilation.Utilities           (getExitStatus, liftText,
                                                     shell')
------------------------------------------------------------------
-- |
mkMainPipeline :: DecsQ
mkMainPipeline =  [d| pipes :: [Connection -> IO ExitStatus]
                      pipes = $mainNames|]

  where mainNames = listE $ map (varE . mkName . makeName) transforms'
        makeName t = (unpack $ tHandle t) ++ "_main"

createPipes :: DecsQ
createPipes = concatMapM f transforms'
  where f t  =  concatMapM ($ t) [ mkPrinter ,mkParser,mkMainFunc]

-- |
mkMainFunc ::  Transform -> DecsQ
mkMainFunc t = case tType t of
  Insert -> mkInsertFunc t
  Update -> mkUpdateFunc t
  Upsert -> mkUpsertFunc t

----
--
mkPrintParseNames :: Transform -> (Q Exp,Q Exp)
mkPrintParseNames  =  f . unpack . tHandle
  where f n = (varE $ mkName (n++"_printer"),varE $ mkName (n++"_parser"))

mkTupUntups :: Transform -> (Q Exp,Q Exp)
mkTupUntups t= (makeExp ("unTuple" ++ show nI)
               ,makeExp ("app"++show nO))
  where [nI,nO] = map (\f-> length $ f t) [inType,outType]
        makeExp = varE . mkName . (++) "Compilation.TH."
--
getTexts :: Transform -> ((Q Exp,Q Exp),(Q Exp,Q Exp),Q Exp,Q Exp,Q Exp,Q Exp,Q Exp,Q Exp,Q Exp)
getTexts t =  (mkPrintParseNames t
                ,mkTupUntups t
               ,liftText $ mkInputFile t
               ,liftText $ mkInsQuery t
               ,liftText $ mkErrQuery t
               ,liftText $ fromMaybe "" $ query t
               ,liftText $ mkNullQuery  t
               ,liftText $ mkUpdateQuery t
               ,liftText $ mkUpsertQuery t
              )

--------------------------------------------------------------------------------
-- | Produce a function of type Conn -> IO ()
--   when called, the function will execute a query to retrieve info from
--    current DB state, transform those inputs with a program, then insert the
--    results into a specified table

mkInsertFunc :: Transform -> DecsQ
mkInsertFunc tr = setName (mkName (unpack (tHandle tr) ++ "_main"))
    -- First, handle transform-specific stuff outside of the quasiquotes
    (do
        let mkQuery = [|query_ conn $ mQuery $q|]

        let getInputs = case inType tr of
              []  -> [|pure [""]|]
              [_] -> [|map ($printr . fromOnly) <$> $mkQuery |]
              _   -> [|map $printr  <$> $mkQuery |]

        [d| xxx :: Connection -> IO ExitStatus
            xxx = \conn ->
              do inputs <-  $getInputs

                 -- write executable to "tempscript"
                 writeFile "tempscript" $ unpack $ $inFile

                 traceM ("inputs "++show inputs)

                 -- compute outputs concurrently
                 rawOutputs <- mapConcurrently exec inputs

                 -- separate errors and outputs
                 let (errors,outputs) = splitEitherArgs rawOutputs

                 -- for the successes, insert into database
                 _ <- executeMany conn (mQuery $insQ)
                        $ map ($parsr . unpack) outputs

                 -- if any errors, edit the "error" column of META_transform
                 unless (null errors)
                      $ do _ <- execute conn (mQuery $errQ) $ Only (mkErr errors)
                           return ()

                 -- report exitstatus
                 return $ getExitStatus (length inputs)
                                        (length inputs)
                                        (length errors)

               where exec input = shell' "python3" ["tempscript",input] |])
  where ((printr,parsr),_,inFile,insQ,errQ,q,_,_,_) = getTexts tr

--
mkUpdateFunc ::  Transform -> DecsQ
mkUpdateFunc tr = setName (mkName (unpack (tHandle tr) ++ "_main"))
    (do let mkQuery = [|query_ conn $ mQuery  $q|]
        [d| xxx ::  Connection -> IO ExitStatus
            xxx = \ conn ->
              do mapIOinit <- $(unTuplr)  <$> $mkQuery -- :: Map Text <INTYPE>

                 putStrLn $ "mapIOinit "++show mapIOinit
                 nullrows  <- nullRows $nullQ conn         :: IO [Text]

                 let mapIO = removeRepeats mapIOinit nullrows

                 putStrLn $ "mapIO "++show mapIO

                 let inputs = nub $ map $printr (elems mapIO) :: [String]

                 putStrLn $ "inputs "++show inputs

                 writeFile "tempscript" $ unpack $ $inFile :: IO ()

                 (mapF,mapS) <- makeIOMapEither exec inputs

                 mapM_ (execute conn (mQuery $upQ))
                       $ $(tuplr)
                       $ Map.map $parsr
                       $ mergeMaps mapS
                                   (Map.map $printr mapIO)

                 unless (Map.null mapF)
                      $ do _ <- execute conn (mQuery $errQ) (Only (mkErr (elems mapF)))
                           return ()
                 return $ getExitStatus
                                (length mapIOinit)
                                (length mapIO)
                                (size mapF)

               where exec input = shell' "python3" ["tempscript",input] |]
        )
  where ((printr,parsr),(unTuplr,tuplr),inFile,_,errQ,q,nullQ,upQ,_) = getTexts tr

--
mkUpsertFunc ::  Transform -> DecsQ
mkUpsertFunc tr = setName (mkName (unpack (tHandle tr) ++ "_main"))
  $
        [d| xxx ::  Connection -> IO ExitStatus
            xxx = \conn ->
              do putStrLn "starting upsert"

                 print $q
                 putStrLn "test"
                 rawOut <- query_ conn $ mQuery $q

                 let mapIOinit = $unTuplr rawOut

                 putStrLn $ "mapIOinit "++show mapIOinit

                 nullrows  <- nullRows $nullQ conn         :: IO [Text]

                 let mapIO = removeRepeats mapIOinit nullrows

                 let inputs = nub $ map $printr (elems mapIO) :: [String]

                 writeFile "tempscript" $ unpack $ $inFile :: IO ()

                 -- Return failures and successes
                 (mapF,mapS) <- makeIOMapEither exec inputs

                 -- Insert new data into table
                 mapM_  (execute conn (mQuery $insQ))
                              $ map $parsr (elems mapS)

                 -- Insert foreign keys
                 mapM_ (execute conn (mQuery $upsQ))
                  $ $(tuplr)
                    $ Map.map  $parsr
                      $ mergeMaps mapS
                        $ Map.map $printr mapIO

                 unless (Map.null mapF)
                    $ do _ <- execute conn (mQuery $errQ) $ Only $ mkErr $ elems mapF
                         return ()

                 return $ getExitStatus
                                (length mapIOinit)
                                (length mapIO)
                                (size mapF)

               where exec input = shell' "python3" ["tempscript",input] |]
    where ((printr,parsr),(unTuplr,tuplr),inFile,insQ,errQ,q,nullQ,_,upsQ) = getTexts tr
