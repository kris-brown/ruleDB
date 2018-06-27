{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Strict             #-}

module PreCompilation.DataTypes.Table
  (MetaTable (..)
  ,Table (..)
  ,Col (..)
  ,FK (..))
where

-- External Modules
import           Data.Data                          (Data)
import           Data.Text                          (Text)

-- Internal Modules
import           PreCompilation.DataTypes.DataTypes (SQLType)

data MetaTable = MetaTable {mTable :: Table                -- everything needed to instantiate table
                         ,mBinds   :: [[Text]]}    -- means for populating table


-- | restricted such that only one UNIQUE constraint can be made per table
data Table = Table {tableName :: Text
                 ,tableDesc   :: Text
                 ,cols        :: [Col]
                 ,fks         :: [FK]
                 } deriving (Data,Show) --,Show,Ord,Eq,G.Generic,Lift)
-------------------------------------------------------------
data Col = Col {colName :: Text
              ,colType  :: SQLType
              ,colPK    :: Bool
              ,colNN    :: Bool
              ,colUniq  :: Bool
              }  deriving (Data,Show) -- ,Ord,Eq,G.Generic,Lift)

--
data FK = FK {fkColumn :: [Text]
            ,fkTable   :: Text
            ,fkTarget  :: [Text]
            }  deriving (Data,Show) -- ,Eq,Ord,G.Generic,Lift)
