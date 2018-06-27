{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Strict             #-}

module PreCompilation.DataTypes.Transform
 (Transform (..)
 ,TType (..)
 ,Template (..)
 ,TempFunc (..)
 ,TFA (..)
 ,parseTType
 ,getTFAfunc)
where

import           Data.Data                          (Data)
import           Data.Text                          (Text)
import           PreCompilation.DataTypes.DataTypes (SQLType)
--------------------------------------------------------------------------------
--Transform Related Datatypes
-------------------
data Transform = TF {tHandle  :: Text
                    ,tType    :: TType
                    ,insTable :: Text
                    ,insCols  :: [Text]
                    ,query    :: Maybe Text
                    ,template :: Template
                    ,inType   :: [SQLType]
                    ,outType  :: [SQLType]
                    ,fkTab    :: Maybe Text
                    ,fkCol    :: Maybe Text
                    ,deps     :: [Text]
                    } deriving (Show,Data)

data TType =  Update | Upsert | Insert deriving (Read,Show,Data,Eq)
parseTType :: Text -> TType
parseTType = \case
  "Update" -> Update
  "Upsert" -> Upsert
  "Insert" -> Insert
  x -> error $ "parseTType failed with input "++show x
--
data Template = Template {tempFuncs :: [TempFunc] -- create DAG and sort into reasonable order
                         ,stdout    :: [TFA]
                         } deriving (Show,Data)

--
data TempFunc  = TempFunc {func   :: Text
                          ,tfID   :: Text
                          ,tfArgs :: [TFA]} deriving (Show,Data)

--
data TFA =  FuncArg {funcAsArg:: Text}
             | TFC {tfcVal :: Text}
             | StdIn {stdInIndex :: Int}
             |  TempFuncArg {tfaSource   :: Text   --
                               ,tfaID    :: Text       -- OUGHT distinguish multiple calls to same TfAsource
                               ,tfaIndex :: Maybe Int    -- nothing => source is not a tuple
            } deriving (Show,Data)

-- | Docstring
getTFAfunc :: TFA -> Maybe Text
getTFAfunc = \case
  TempFuncArg x _ _ -> Just x
  FuncArg x -> Just x
  _ -> Nothing
