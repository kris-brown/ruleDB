{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Strict             #-}

module PreCompilation.DataTypes.Misc
  (Config (..)
  ,Func (..)
  ,Language (..)
  ,Error (..)
  ,ExitStatus (..)
  ,parseLanguage
  ) where

-- External Modules
import           Data.Data                          (Data)
import           Data.Text                          (Text)
import           Data.Word                          (Word16)

-- Internal Modules
import           PreCompilation.DataTypes.DataTypes (DataType)

{- DataTypes that are shared across a variety of contexts -}
--------------------------------------------------------------------------------
-- Func Related
---------------
data Func = Func {funcHandle :: Text
                 ,funcPath   :: Text
                 ,docString  :: Text
                 ,sourceCode :: Text
                 ,language   :: Language
                 ,fromType   :: [DataType]
                 ,toType     :: DataType
                 }  deriving (Show,Data)
--
data Language = Python | Haskell deriving (Eq,Show,Data)

parseLanguage ::  Text -> Language
parseLanguage = \case
   "Python" -> Python
   "Haskell" -> Haskell
   x -> error $ "parse language failed with input "++ show x
--------------------------------------------------------------------------------
-- Config Related
-----------------
data Config  = Config {verbose  :: Bool
                      ,retry    :: Bool
                      ,simulate :: Bool
                      ,host     ::  String
                      ,port     :: Word16
                      ,user     :: String
                      ,password :: String
                      ,database :: String
                      } deriving (Data,Show) --,Eq,Ord,,G.Generic)

-------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Secondary DataType
------------------------------------------------------------------------------------
--------------------------------------------------------------------------
newtype Error = Error {unError :: Text} -- deriving (Show,Eq)
data ExitStatus = Completed | NoInput | AlreadyComputed | Failed deriving (Show)--,Eq,Ord)
