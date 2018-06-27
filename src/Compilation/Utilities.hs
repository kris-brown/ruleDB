{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}

module Compilation.Utilities
  (mQuery
  ,concatMapM
  ,mExecute_
  ,removeRepeats
  ,mergeMaps
  ,splitArgs
  ,setName
  ,mkErr
  ,mkUpsertQuery
  ,mkUpdateQuery
  ,mkNullQuery
  ,splitEitherArgs
  ,mkInsQuery
  ,nullRows
  ,mkErrQuery
  ,makeIOMapEither
  ,mkPrinter
  ,mkParser
  )where
-- External Modules]
import           Control.Concurrent.Async           (mapConcurrently)
import           Data.Either                        (partitionEithers)
import           Data.Map.Strict                    (Map, filterWithKey,
                                                     fromList, lookup,
                                                     mapEither, mapMaybe)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text, concat, intercalate,
                                                     intersperse, pack,
                                                     replicate, splitOn, unpack,
                                                     unwords)
import           Data.Text.Encoding                 (encodeUtf8)
import           Database.MySQL.Simple              (Connection,
                                                     Only (Only, fromOnly),
                                                     execute_, query_)
import           Database.MySQL.Simple.Types        (Query (Query))
import           Language.Haskell.TH                (Dec (SigD, ValD), DecsQ,
                                                     Name, Pat (VarP), Q, Type,
                                                     appE, appT, conE, conT,
                                                     listE, listP, mkName, tupE,
                                                     tupP, tupleT, varE, varP,
                                                     wildP)
import           Prelude                            hiding (concat, lookup,
                                                     replicate, unwords)
import qualified Prelude                            as P (concat)

-- Internal Modules
import           PreCompilation.DataTypes.DataTypes (SQLType (..))
import           PreCompilation.DataTypes.Misc      (Config (retry),
                                                     Error (Error))
import           PreCompilation.DataTypes.Table     (Col (colName, colPK),
                                                     Table (cols, tableName))
import           PreCompilation.DataTypes.Transform (TType (..), Transform (..))
import           PreCompilation.ParsingUtils        (fParseMB, iParseMB,
                                                     sParseMB)
import           PreCompilation.TH                  (config', tables')
import           PreCompilation.Utilities           (comma, if', tShow,
                                                     unsafeLookup)
--------------------------------------------------------------------------------

mQuery :: Text -> Query
mQuery = Query . encodeUtf8

mExecute_ ::  Connection -> Text -> IO ()
mExecute_ c q = do {_<-execute_ c $ mQuery q;return ()}

--------------------------------------------------------------------------------
-- | If an insert program's output is meant to generate multiple outputs,
--    then these will be delimited by "&&&"
splitEitherArgs :: [Either a String] ->  ([a],[Text])
splitEitherArgs = partitionEithers . concatMapEither (splitOn "&&&" . pack)

--------------------------------------------------------------------------------
-- | If input element isRight, then possible multiple outputs are generated
-- is throwing away null strings a HACK?
--   introduced because get_dft_jobs was returning empty list,
--   which lead to creating a single row in job (with empty string for stordir)
--   only downside is if we want to do an insert that inserts a single null
concatMapEither :: (String->[c]) -> [Either a String] -> [Either a c]
concatMapEither _ []           = []
concatMapEither f (Left x:xs)  = Left x : concatMapEither f xs
concatMapEither f (Right x:xs)
 | null x    = concatMapEither f xs
 | otherwise = map Right (f x) ++ concatMapEither f xs


concatMapM :: (Monad f, Traversable t) => (a1 -> f [a2]) -> t a1 -> f [a2]
concatMapM = fmap (fmap P.concat) . mapM
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | ex: INSERT INTO job (stordir,user,timestamp) VALUE (?,?,?)
mkInsQuery :: Transform -> Text
mkInsQuery t  = unwords strs
  where strs = ["INSERT IGNORE INTO",insTable t,"(",strcols,") VALUES (",qmarks,")"]
        strcols = intercalate "," $  insCols t
        qmarks = intersperse ',' $ replicate n "?"
        n      = length $ insCols t

---------------------------------------------------------------------
-- | Checks for repeat jobs for UPDATE and INSERT transforms
mkNullQuery :: Transform -> Text
mkNullQuery t = unwords ["select",pkStr,"from",tab,"where",cond]
  where pkStr   = tabQueryPK $ unsafeLookup tableName tables' $ tab
        tab     = fromMaybe (insTable t) (fkTab t) -- iff fkTab, then Upsert, we assume
        cond    = if tType t == Upsert then upsCond else upCond
        upsCond = concat [fromMaybe "" (fkCol t)," is null"]
        upCond  = intercalate " AND " $ map f (insCols t)
        f c     = concat [ c," is null"]
-- |
nullRows :: Text -> Connection -> IO [Text]
nullRows q conn = fmap (map fromOnly) . query_ conn $ mQuery q
--
tabQueryPK :: Table -> Text
tabQueryPK t = concat ["concat_ws('_',",comma pkcs,")"]
  where pkcs = map colName $ filter colPK $ cols  t

---------------------------------------------------------------------
mkUpdateQuery :: Transform -> Text
mkUpdateQuery t = unwords strs
  where strs    = ["UPDATE ",insTable t," SET ",strcols," where ",pkStr," = ?"]
        strcols = intercalate "," $ map f (insCols t)
        f col   = concat [col," = ?"]
        pkStr   = tabQueryPK $ unsafeLookup tableName tables' $ insTable t

--
-- |
mkUpsertQuery :: Transform -> Text
mkUpsertQuery t = unwords strs
  where (Just fkt,Just fkc,insTab,iCs) = (fkTab t,fkCol t,insTable t, insCols t)
        iCQs = intercalate " AND " $ map (\x->concat ["(",x," = ? OR ",x," is null)"]) iCs
        strs = ["UPDATE",fkt,"SET",fkc,"=(SELECT id FROM", insTab,"WHERE",iCQs,") WHERE ",pkStr," = ?"]
        pkStr   = tabQueryPK $ unsafeLookup tableName tables' fkt


--
mkErrQuery :: Transform -> Text
mkErrQuery t =  concat ["UPDATE META_transform SET error = ? "
                                  ," WHERE method='", tHandle t,"'"]

mkErr :: [Error] ->  Text
mkErr (Error e:_) =  e
mkErr _ = error "mkErr should only be called on non-empty error lists"

setName :: Name -> DecsQ -> DecsQ
setName name  =  fmap (map (sn name))
  where sn n (SigD _ t)          = SigD n t
        sn n (ValD (VarP _) x y) = ValD (VarP n) x y
        sn _ d                   = d

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | For a Transform with INPUT TYPE = (a,b,c,...) we need to construct a function
--    with type (a,b,c,...) -> Str, to be consumed by the Transforms characteristic program
mkPrinter' ::  Transform -> DecsQ
mkPrinter' t = do
    let args = map (mkName . (++) "x" . show) [0..n-1]
    let tupArgs = varP <$> args
    let printList = (\(a,dt) -> appE (varE (getPrinter dt)) (varE a)) <$> zip args iT
    let pL' = listE printList
    let inTuple = foldr g (tupleT n) (reverse iT)
    let lamExp = if' (n==0) wildP (tupP tupArgs)
    [d| xxx :: $inTuple -> String
        xxx = \ $lamExp ->  unpack $ intercalate (pack "@@@")  $pL'|]
  where n    = length iT
        g newDT typ = appT typ (getType newDT)
        iT   = inType t

mkPrinter ::  Transform -> DecsQ
mkPrinter t = setName name $ mkPrinter' t
  where name = mkName $ unpack (tHandle t) ++ "_printer"

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
-- | Makes a function String -> <OUTTYPE> which processes program output
mkParser' :: Transform -> DecsQ
mkParser' t = do
    let argNames  = map (mkName . (++) "x" . show) [0..n-1]
    let listArgs  = listP $ varP <$> argNames
    let parseList = tupE $ zipWith f argNames ot
    let pL        = if' needsWrapper (appE (conE 'Only)) id $ parseList
    [d| xxx =(\ $listArgs ->  $pL) . splitArgs |]

  where ot   = outType t
        n    = length ot
        f aN dT      = appE (varE (getParser dT)) (varE aN)
        needsWrapper = (length ot == 1) && (tType t == Insert)


mkParser :: Transform -> DecsQ
mkParser t = setName name $ mkParser' t
  where name = mkName $ unpack (tHandle t) ++ "_parser"

--
getPrinter :: SQLType -> Name
getPrinter = \case
  SQLVarchar -> 'tShow
  SQLText    -> 'tShow
  SQLInt     -> 'tShow
  SQLFloat   -> 'tShow

getParser :: SQLType -> Name
getParser = \case
  SQLVarchar -> 'sParseMB
  SQLText    -> 'sParseMB
  SQLInt     -> 'iParseMB
  SQLFloat   -> 'fParseMB

getType :: SQLType -> Q Type
getType = \case
  SQLVarchar  -> conT ''Text
  SQLText     -> conT ''Text
  SQLInt      -> conT ''Int
  SQLFloat    -> conT ''Double

splitArgs :: String -> [Text]
splitArgs = splitOn "@@@" . pack
---------------------------------------------------------------------
-- | Map related functions
--------------------------
makeIOPair :: (a->IO b) -> a ->  IO (a,b)
makeIOPair f x = do y <- f x
                    return (x,y)

makeIOPairs :: (a->IO b) -> [a] ->  IO [(a,b)]
makeIOPairs f = mapConcurrently (makeIOPair f)

makeIOMap :: Ord a => (a->IO b) -> [a] -> IO (Map a b)
makeIOMap f = fmap fromList . makeIOPairs f

-- | Merges two maps. Result map has all possible links.
mergeMaps ::  (Show a, Show b, Show c ,Ord b) => Map b c -> Map a b -> Map a c
mergeMaps a =  mapMaybe (`lookup` a)

makeIOMapEither :: Ord a => (a -> IO (Either b c))
                              -> [a]
                              -> IO (Map a b,Map a c)
makeIOMapEither f xs = mapEither id <$>  makeIOMap f xs
---------------------------------------------------------------------
-- | If the (RETRY) flag is false, then remove all entries in the map whose key
--   is not found in the list (of job_IDs that were flagged as having NULL)
removeRepeats ::  Map Text a -> [Text] -> Map Text a
removeRepeats  m ints
 | retry config'    = m -- don't filter
 | otherwise = filterWithKey (\k _ -> k `elem` ints) m
--

--
