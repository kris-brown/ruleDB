{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module PostCompilation.MetaTable
  (initTables
  ,fillMetaData
  ,dropMetatables
  )where


import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text, concat, intercalate,
                                                     unwords)
import           Database.MySQL.Simple              (Connection, execute)

import           Compilation.InputFile              (getLangFromTemp,
                                                     mkInputFile)
import           Compilation.Utilities              (mExecute_, mQuery)
import           PostCompilation.Utilities          (renderType)
import           PreCompilation.DataTypes.DataTypes (SQLType (..))
import           PreCompilation.DataTypes.Misc      (Func (..))
import           PreCompilation.DataTypes.Table     (Col (..), FK (..),
                                                     MetaTable (..),
                                                     Table (Table, cols, tableName))
import           PreCompilation.DataTypes.Transform (Transform (..))
import           PreCompilation.TH                  (funcs', tables',
                                                     transforms')
import           PreCompilation.Utilities           (comma, if', tShow)
import           Prelude                            hiding (concat, unwords)

------

--------------------------------------------------------------------------------
-- | Create all tables specified in DBG (if they don't exist), including meta-tables
initTables :: Connection -> IO ()
initTables  conn = mapM_ (mExecute_ conn . mkTableQ) (map mTable metatables ++ tables')

-- | SQL statement to create a table
mkTableQ :: Table -> Text
mkTableQ (Table t_n _ cs fks) =  unwords strs
  where strs        = ["CREATE TABLE IF NOT EXISTS ",t_n
                      ,"(",strcols,pkConst cs
                      ,concat $ map fkConst fks
                      ,uniqConst cs
                      ,")"]
        strcols     = comma $ map col2Str cs

-- | Render a column for the SQL CREATE TABLE statement
col2Str :: Col -> Text
col2Str c = unwords [colName c,typ,nnstr ,autoInc]
  where nnstr = if' (colNN c) " NOT NULL " ""
        typ   = tShow $ colType c
        autoInc = if' (colName c /= "id") ""  " AUTO_INCREMENT "
-- | Writes foreign key constraints
fkConst :: FK -> Text
fkConst (FK c t x) = concat [",FOREIGN KEY (",comma c,") REFERENCES ",t,"(",comma x,")"]

-- | Writes primary key constraints
pkConst :: [Col] -> Text
pkConst cs = concat [",PRIMARY KEY (",comma pkcols,")"]
  where pkcols = map colName $ filter colPK cs

-- | Writes UNIQUE constraints
uniqConst :: [Col] -> Text
uniqConst cs = case filter colUniq cs of
  [] -> ""
  xs ->  concat [",UNIQUE (",comma (map colName xs)
                  ,")"]

--------------------------------------------------------------------------------
-- |
dropMetatables :: Connection -> IO ()
dropMetatables conn = mapM_ (dropMeta conn) (map mTable metatables ++ (reverse tables'))

-- | Apply metatable functions to populate their tables
fillMetaData :: Connection -> IO ()
fillMetaData conn = mapM_ (populateMeta conn) metatables

-- | Drop meta table (should be done at start of program)
dropMeta :: Connection -> Table -> IO ()
dropMeta conn t = mExecute_ conn quer
  where quer = concat ["DROP TABLE IF EXISTS ",tableName t]


-- | A metatable comes equipped with a function that takes a DBG instance
--   and returns BINDS that get used in this function.
populateMeta ::  Connection -> MetaTable ->  IO ()
populateMeta  conn (MetaTable t strs)  =  mapM_ xQ strs
  where cs       = comma $ map colName $ tail $ cols t
        qmarks   = comma $ replicate (length ( cols t) - 1) "?"
        q        = unwords ["INSERT INTO ",tableName t,"(",cs,") VALUES (",qmarks,")"]
        xQ       =  mExecute conn q
        mExecute c quer r = do {_<-execute c  (mQuery quer) r;return ()}

--------------------------------------------------------------------------------
-- | All metatables
metatables :: [MetaTable]
metatables = [metaTransform,metaDeps,metaTabs,metaFunc] -- these ones are functional, do others later

-- | Shortcut for making a metatable that takes advantage of some assumptions:
--  1. Every column in every metatable is VARCHAR (except for meta.current)
--  2. There are no SQL constraints / FKs
mkMetaTable :: Text -> Text -> [Text] -> [[Text]] -> MetaTable
mkMetaTable mName mDesc mCols  = MetaTable (Table mName mDesc (idCol:mCols') [])
  where mCols'  = map (\c->Col c SQLText False True False) mCols
        idCol   = Col "id" SQLInt True True False


-- | Table storing everything known about every transform (in order)
metaTransform :: MetaTable
metaTransform = mkMetaTable "META_transform" "List of possible transformation to database in the order they are scheduled to run"
                        ["method","method_type","status","error","time"
                        ,"query","template","target_table"
                        ,"insert_cols","input_type","output_type"
                        ,"fk_table","fk_col","language","deps"]
                    mkMetaTransform

-- | Unpack and render all fields of a Transform instance
mkMetaTransform :: [[Text]]
mkMetaTransform = map f transforms'
  where f t@(TF tH tT tN iCs q temp iT oT fkT fkC ds) =
               [tH,tShow tT,"initialized","",""
               ,fromMaybe "" q , mkInputFile t, tN
               ,comma  iCs
               ,tShow iT ,tShow oT
               ,fromMaybe "" fkT, fromMaybe "" fkC
               , tShow $ getLangFromTemp temp,comma ds]

-- | Dependency info in a queryable form (gets used in FillTables.hs) so that
--    if a dependency fails (or is ignored), the dependent Transform will
--    not be executed
metaDeps :: MetaTable
metaDeps = mkMetaTable "META_dependencies" "Summary of transform dependencies"
                        ["parent","child"]
                       mkMetaDeps

-- | Unpack dependencies from list of all Transforms
mkMetaDeps :: [[Text]]
mkMetaDeps = concatMap f  transforms'
  where f tf = map (\d -> [d,tHandle tf]) (deps tf)

-- | List of all functions in DBG instance
metaFunc :: MetaTable
metaFunc = mkMetaTable "META_func" "List of external functions"
                        ["func_handle","funcPath","docstring","sourcecode"
                        ,"language","fromType","toType"]
                   mkMetaFunc

-- | Collect all the funcs from DataTables,Rows, and Columns
mkMetaFunc ::  [[Text]]
mkMetaFunc = map xtract funcs'
  where xtract (Func fh pth ds sc lang ft tt) = [fh, pth, ds, sc
                                                  ,tShow lang
                                                  ,comma $ map renderType ft
                                                  ,renderType tt]

metaTabs :: MetaTable
metaTabs = mkMetaTable "META_tables" "Summary of tables"
                        ["name","description","not_nulls","fks","uniques"]
                       mkMetaTabs

-- | Unpack dependencies from list of all Transforms
mkMetaTabs ::  [[Text]]
mkMetaTabs = map f tables'
  where f (Table n d cs fs) = [n,d,getNN cs
                                 ,intercalate "\n" $ map getFK fs
                                 ,getU cs]

        getFK (FK c t x) = concat [comma c," -> ",t,"(",comma x,")"]
        getNN cs            = listNames $ filter colNN cs
        getU  cs            = listNames $ filter colUniq cs
        listNames  = comma . map colName
