{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module PreCompilation.Initialize (loadMetaDB) where

import           Data.Aeson                         (FromJSON, eitherDecode)
import           Data.List                          (zipWith5)
import           Data.Maybe                         (fromMaybe, isJust)
import           Data.Text                          (Text, drop, dropEnd, pack,
                                                     unpack)
import           Data.Text.Lazy                     (fromStrict)
import           Data.Text.Lazy.Encoding            (encodeUtf8)
import           Database.SQLite.Simple             (Connection, Only (..),
                                                     Query (Query), query,
                                                     query_, withConnection)
import           Prelude                            hiding (drop)
import           Text.Read                          (readMaybe)

--
import           PreCompilation.DataTypes.DataTypes (SQLType, parseSQLType)
import           PreCompilation.DataTypes.Table     (Col (Col), FK (FK),
                                                     Table (Table))

import           PreCompilation.DataTypes.Misc      (Config (Config),
                                                     Func (Func), parseLanguage)
import           PreCompilation.DataTypes.Transform (TFA (FuncArg, StdIn, TFC, TempFuncArg),
                                                     TempFunc (TempFunc),
                                                     Template (Template),
                                                     Transform (TF), parseTType)
import           PreCompilation.Utilities           (unComma)
{-
WE NEED TO USE THE "ONLY" "EXCEPT" CONFIG OPTIONS TO PRUNE THE TRANSFORMS HERE,
RATHER THAN AT RUNTIME!
-}
--------------------------------------------------------------------------------

-- |
loadMetaDB :: FilePath                              -- location of meta database
         -> IO (Config,[Func],[Table],[Transform])  -- Left if any error parsing the JSON data
loadMetaDB pth = do funs <- exec getFuns
                    tabs <- exec getTabs
                    conf  <- exec getConfig
                    putStrLn "\t\tNOT using config to filter out transforms"
                    trans <- exec getTrans
                    return (conf,  funs,  tabs,trans)
  where exec = withConnection pth ::  (Connection -> IO a) -> IO a
--------------------------------------------------------------------------------

int2bool :: Text -> Bool
int2bool = (==) "1"

-- | unsafe parser that will throw runtime error
decodeTxt :: FromJSON a => Text -> a
decodeTxt t = case eitherDecode $ encodeUtf8 $ fromStrict t of
  Right x -> x
  Left e  -> error e

-- | Extract functions from func table
getFuns :: Connection -> IO [Func]
getFuns c = do fs <- query_ c q
               return $ mkFunc <$> fs
  where q = Query "SELECT name,pth,source,docstring,language,inTypes_json,outType_json from func"
        mkFunc (name,path,source,docstring,language,inTypes,outType)
          =  Func name path source docstring
            (parseLanguage language)
            (decodeTxt $ pack inTypes)
            (decodeTxt $ pack outType)


getTabs :: Connection -> IO [Table]
getTabs conn = do qOut <- query_ conn colQ :: IO [(Int,Text,Text,Text,Text,Text,Text,Text)]
                  fks <- mapM (mkFKs conn .fstOf8) qOut :: IO [[FK]]
                  return $ zipWith mkTab (restOf8 <$> qOut) fks
  where colQ = Query "SELECT T.id \
                            \,T.name \
                    				\,T.desc \
                    				\,group_concat(C.name) \
                    				\,group_concat(C.type) \
                    				\,group_concat(C.pk) \
                    				\,group_concat(C.nnull) \
                    				\,group_concat(C.uniq) \
                    \FROM tab T \
                      \JOIN col C           ON C.tab_id   = T.id \
                      \JOIN table_order TBO ON TBO.tab_id = T.id \
                    \GROUP BY T.id \
                    \ORDER BY TBO.id"
        fstOf8 (a,_,_,_,_,_,_,_) = a
        restOf8 (_,b,c,d,e,f,g,h) = (b,c,d,e,f,g,h)
        mkCol n t p nn u = Col n (parseSQLType t) (int2bool p) (int2bool nn) (int2bool u)
        mkTab (nam,desc,cNames,cTyps,cPks,cNNs,cUs)
          = Table nam desc (zipWith5 mkCol (unComma cNames)
                                           (unComma cTyps)
                                           (unComma cPks)
                                           (unComma cNNs)
                                           (unComma cUs))
mkFKs :: Connection -> Int -> IO [FK]
mkFKs c i = map processFKdata <$> (query c q (Only i) :: IO [(Text,Text,Text)])
  where q =  Query "SELECT group_concat(FC.name),TT.name,group_concat(TC.name) \
              \from  fk \
              \join col FC on FC.tab_id = fk.tab_id and FC.id = fk.col_id \
              \join col TC on TC.tab_id = fk.to_tab and TC.id = fk.to_col \
              \join tab TT on TT.id=fk.to_tab \
              \where fk.tab_id = ? \
              \group by fk.to_tab"
        processFKdata (fcols,tabname,tcols) = FK (unComma fcols) tabname (unComma tcols)
-- | For a given Transform (specified by name) and DB cnxn, make a Template
getTF :: Connection -> Text -> IO Template
getTF c trName = do tfs    <- query c tfQ [trName] :: IO [(Text,Int,Int,String)] -- one element for each TempFunc
                    tfas   <- mapM (getTFA c . drpFst) tfs  :: IO [[TFA]] -- one list for each temp func
                    stdout <- getStdOut c trName :: IO [TFA]-- one element for each element of stdout
                    return $ Template (zipWith mkTempFunc tfs tfas) stdout

  where tfQ = Query "SELECT F.name,T.id,F.id,TF.id \
                	\FROM tempfunc TF \
                	\JOIN transform T ON TF.transform_id = T.id \
                  \JOIN func F ON F.id = TF.func_id \
                  \JOIN tempfunc_order TFO ON TFO.transform_id = TF.transform_id AND TFO.func_id = TF.func_id AND TFO.tempfunc_id=TF.id \
                	\WHERE T.name = ? \
                	\GROUP BY TF.func_id,TF.id \
                  \ORDER BY TFO.id"
        drpFst (_,x,y,z) = (x,y,z)
        mkTempFunc (fName,_,_,tfID)  = TempFunc fName (pack tfID)

getTFA ::  Connection -> (Int,Int,String) -> IO [TFA]
getTFA c ids = map mkTFA <$> query c q ids
  where q = Query "SELECT TFA.stdin \
                				\,TFA.const \
                			  \,F.name \
                        \,TFUN.name \
                			  \,TFA.tfo_tfid \
                				\,TFA.tfo_id \
                  \FROM tempfuncArg TFA \
                  \LEFT JOIN func F ON F.id = TFA.func_arg \
                  \LEFT JOIN func TFUN ON TFUN.id = TFA.tfo_fid \
                \WHERE TFA.transform_id=? and TFA.func_id=? and TFA.tempfunc_id=? \
                  \ORDER BY TFA.id"
        mkTFA (mbStdIn,mbConst,mbFuncArg,mbTFuncName,mbTFID,mbOutID)
          | isJust mbStdIn = StdIn $ fromMaybe 0 mbStdIn
          | isJust mbConst = TFC $ fromMaybe "" mbConst
          | isJust mbFuncArg = FuncArg  (fromMaybe "" mbFuncArg)
          | otherwise = TempFuncArg (fromMaybe "" mbTFuncName)
                                    (fromMaybe "" mbTFID)
                                    mbOutID

getStdOut ::  Connection -> Text -> IO [TFA]
getStdOut c trName = do result <- query c q $ Only trName :: IO [(Maybe Int,Maybe Text,Maybe Int,Maybe Text,Maybe Text,Maybe Int)]
                        return $ map mkTFA result
  where q = Query "SELECT SO.stdin \
                  				\,SO.const \
                  				\,SO.tfo_tid \
                  				\,TFUN.name \
                  				\,SO.tfo_tfid \
                  				\,SO.tfo_id \
                  \FROM stdout SO  \
                  \JOIN transform T ON T.id = SO.transform_id \
                  \LEFT JOIN func TFUN ON TFUN.id = SO.tfo_fid \
                  \WHERE T.name=? \
                  \ORDER BY SO.id"
        mkTFA (mbStdIn,mbConst,_,mbTFuncName,mbTFID,mbOutID)
            | isJust mbStdIn = StdIn $ fromMaybe 0 mbStdIn
            | isJust mbConst = TFC $ fromMaybe "" mbConst
            | otherwise = TempFuncArg (fromMaybe "" mbTFuncName)
                                      (fromMaybe "" mbTFID)
                                      mbOutID

getTrans ::  Connection -> IO [Transform]
getTrans c = do tResult <- query_ c q -- :: IO [(Text,Text,Text,Text,Maybe Text,Text,Text,Maybe Text,Maybe Text)]
                depResult <- fmap fromOnly <$> query_ c depsQ :: IO [Maybe Text]
                tfs <- mapM (getTF c . fstOf9) tResult
                return $ mkT <$> zip3 tResult tfs depResult
  where q = Query "SELECT transform.name AS tHandle \
                      	\,transform.type AS tType  \
                      	\,InsT.name AS insTable  \
                      	\,group_concat(InsC.name) AS insCols \
                      	\,transform.query AS query  \
                      	\,transform.inType AS inType  \
                      	\,transform.outType AS outType \
                      	\,FKT.name AS fkTab \
                      	\,FKC.name AS fkCol \
                      \FROM transform  \
                      	\JOIN transform_order ON transform.id = transform_order.transform_id  \
                      	\LEFT JOIN transform_cols ON transform_cols.transform_id = transform.id  \
                      	\LEFT JOIN col InsC ON InsC.id=transform_cols.col_id and InsC.tab_id=transform_cols.tab_id  \
                      	\LEFT JOIN tab InsT ON InsT.id = transform_cols.tab_id \
                      	\LEFT JOIN col FKC ON FKC.tab_id=transform.fkTab and FKC.id=transform.fkCol \
                      	\LEFT JOIN tab FKT ON FKT.id=transform.fkTab \
                      	\GROUP BY transform.id \
                      	\ORDER BY transform_order.id"
        depsQ = Query "SELECT group_concat(DepT.name) AS deps  \
                        \FROM transform  \
                        	\JOIN transform_order ON transform.id = transform_order.transform_id  \
                        	\LEFT JOIN transformDeps ON transformDeps.child = transform.id \
                        	\LEFT JOIN transform DepT ON DepT.id = transformDeps.parent \
                        	\GROUP BY transform.id \
                        	\ORDER BY transform_order.id"
        mkT ((tHandl,tType,tab,insCols,quer,inT,outT,fkT,fkC),temp,depsStr)
          =  TF tHandl (parseTType tType) tab (unComma insCols)
                quer  temp
                (getT inT) (getT outT) fkT fkC
                (maybe [] unComma depsStr)
        fstOf9 (x,_,_,_,_,_,_,_,_) = x
        getT = map parseSQLType . unComma . drop 1 . dropEnd 1  :: Text -> [SQLType]

getConfig :: Connection -> IO Config
getConfig c = do (x:_) <-query_ c $ Query q :: IO [(Int,Int,Int,String,Int,String,String,String)]
                 return $ mkConfig x
  where q =  "SELECT verbose,retry,simulate,hostname,port,user,password,database \
              \from config join connection on connection_id=connection.id"
        mkConfig (v,r,s,h,p,u,pw,d) = Config ((== 1) v) ((== 1) r) ((== 1) s) h (fromIntegral p) u pw d
