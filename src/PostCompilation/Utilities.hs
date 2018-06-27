{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module PostCompilation.Utilities
  (printV
  ,removeIfExists
  ,connectInfo
  ,setTfield
  ,renderType
  ,update
  ,queryInt
  ,roundFloat) where
import           Compilation.Utilities              (mExecute_, mQuery)
import           Control.Exception                  (catch, throwIO)
import           Data.Text                          (Text, concat)
import           Database.MySQL.Simple              (ConnectInfo (..),
                                                     Connection, fromOnly,
                                                     query_)
import           PreCompilation.DataTypes.DataTypes (DataType (..), isBase,
                                                     isDict, isFun, isList,
                                                     isTuple, isUnion)
import           PreCompilation.DataTypes.Misc      (Config (database, host, password, port, user, verbose))
import           PreCompilation.DataTypes.Transform (Transform (tHandle))
import           PreCompilation.TH                  (config')
import           PreCompilation.Utilities           (comma, if', tShow)
import           Prelude                            hiding (concat)
import           System.Directory                   (removeFile)
import           System.IO.Error                    (isDoesNotExistError)
--
-- | This can't go in utilities because utilities is compiled before TH
printV :: String -> IO ()
printV = if' (verbose config') putStrLn (\_->pure ())

-- | Delete file from path
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

connectInfo ::  ConnectInfo
connectInfo = ConnectInfo (host config') (port config') (user config')
                              (password config') (database config') [] "" Nothing


-- | A version of the "update" function with some degrees of freedom removed
--    (also, note that it automatically executes the update)
setTfield :: Connection -> Transform -> Text -> Text -> IO ()
setTfield conn t col value = mExecute_ conn $ update "META_transform"
                                                    col value "method"
                                                    (tHandle t) True True
--

-- | Prettyprint a Datatype
renderType :: DataType -> Text
renderType dt
 | dt == AnyType  = "Any"
 | dt == NoneType = "None"
 | isBase dt     =  tShow $ unBase dt
 | isUnion dt    = concat ["Union [",comma $ map renderType $ uArgs dt,"]"]
 | isList dt     = concat ["[",renderType $ unList dt,"]"]
 | isTuple dt    = concat ["(",comma  $ map renderType $ tArgs dt,")"]
 | isFun dt = concat ["{",comma $ map renderType (cArgs dt),"} -> ",renderType $ out dt]
 | isDict dt = concat ["Dict{",renderType $ key dt," : ",renderType $ val dt,"}"]
 | otherwise = error $ "Haven't yet written a renderType instance for "++show dt


--------------------------------------------------------------------------------
-- | Wrapper for UPDATE statement where we want to change a particular cell
--
update :: Text -> Text -> Text -> Text -> Text -> Bool -> Bool -> Text
update tab col value condCol  condVal valIsStr condIsStr =   concat strs
  where strs = ["update ",tab," set ",col," = ",vQuote,value,vQuote
               ," where ",condCol," = ",cQuote,condVal,cQuote]
        vQuote = if' valIsStr "'" ""
        cQuote = if' condIsStr "'" ""
--
queryInt :: Connection -> Text -> IO [Int]
queryInt  conn q = fmap fromOnly <$> query_ conn (mQuery q)


roundFloat ::  Int -> Rational  -> Double
roundFloat n f = fromRational (num / denom )
  where num = fromInteger (round $ f * (10^n))
        denom = 10.0^^(1::Int)
