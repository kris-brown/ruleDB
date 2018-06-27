{-# LANGUAGE DeriveAnyClass     #-}
-- So that separate lines aren't needed to deriving A.FromJSON/A.ToJSON
{-# LANGUAGE DeriveGeneric      #-}
-- To derive A.FromJSON/A.ToJSON
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Strict             #-}

-- {-# LANGUAGE DeriveLift         #-}

module PreCompilation.DataTypes.DataTypes
  (SQLType (..)
  ,parseSQLType
  ,DataType (..)
  ,isNoneType
  ,isList
  ,isBase
  ,isUnion
  ,isTuple
  ,isFun
  ,isDict)

where

-- External Modules
import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Data    (Data)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- import           Language.Haskell.TH.Syntax (Lift)
-- import qualified Data.Data    as D

--------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Type to model SQL types
--------------------------
data SQLType = SQLInt | SQLVarchar | SQLFloat | SQLText  deriving (Data,Eq)

instance Show SQLType where
    show SQLInt     = "INTEGER"
    show SQLText    = "TEXT"
    show SQLFloat   = "DECIMAL"
    show SQLVarchar = "VARCHAR(255)"

parseSQLType :: Text -> SQLType
parseSQLType = \case
  "Int"     -> SQLInt
  "Text"    -> SQLText
  "Str"     -> SQLVarchar
  "Varchar" -> SQLVarchar
  "Float"   -> SQLFloat
  "Numeric" -> SQLFloat
  x         ->  error $ "parseSQLType couldn't parse " ++ show x

--------------------------------------------------------------------------------
-- Type to model more general Types
-----------------------------------
data DataType = NoneType | AnyType | Base {unBase :: String}
                | Callable {cArgs:: [DataType],out:: DataType}
                | Union {uArgs:: [DataType]} | List {unList :: DataType}
                | Tuple {tArgs :: [DataType]}
                | Dict {key :: DataType, val :: DataType}
                deriving (Show,Generic,FromJSON,ToJSON,Data,Eq) --Data, ,Lift)

-- -- | This will be helpful when we want to autoderive the FROM TYPE of transforms
-- dt2SQLtype :: DataType -> SQLType
-- dt2SQLtype  = \case
--   Base "Int"   -> SQLInt
--   Base "Float" -> SQLFloat
--   Base "Str"   -> SQLVarchar
--   x -> error ("bad datatype in dt2sqltype "++show x)

isNoneType :: DataType -> Bool
isNoneType NoneType = True
isNoneType _        = False
isBase :: DataType -> Bool
isBase (Base _) = True
isBase _        = False
isUnion :: DataType -> Bool
isUnion (Union _) = True
isUnion _         = False
isList :: DataType -> Bool
isList (List _) = True
isList _        = False
isTuple :: DataType -> Bool
isTuple (Tuple _) = True
isTuple _         = False
isFun :: DataType -> Bool
isFun (Callable _ _) = True
isFun _              = False
isDict :: DataType -> Bool
isDict (Dict _ _) = True
isDict _          = False
