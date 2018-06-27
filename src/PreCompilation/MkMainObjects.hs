{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TemplateHaskell     #-}

module PreCompilation.MkMainObjects
  (createMainObjects) where

-- External Modules
import           Data.Typeable                      (cast)
import           Language.Haskell.TH                (DecsQ, conT)
import           System.Environment                 (getEnv)

import           Language.Haskell.TH.Syntax         (addDependentFile,
                                                     dataToExpQ, runIO)
-- Internal Modules
import           PreCompilation.DataTypes.Misc      (Config, Func)
import           PreCompilation.DataTypes.Table     (Table)
import           PreCompilation.DataTypes.Transform (Transform)
import           PreCompilation.Initialize          (loadMetaDB)
import           PreCompilation.Utilities           (liftText)
{-
Create global (compiled) objects from the results of parsing the database
-}
--

-- |
createMainObjects :: DecsQ
createMainObjects =  do
    root <- runIO getRoot
    addDependentFile (dbFile root) -- changes in meta.db will cause the program to recompile from here
    (conf,fs,tabs,transforms) <- runIO (mainIO root)
    confDecs <- mkConfig conf
    funcDecs <- mkFuncs  fs
    tabDecs  <- mkTabs tabs
    trDecs   <- mkTrans  transforms
    return $ concat [confDecs,funcDecs,tabDecs,trDecs]

  where mainIO rt = loadMetaDB $ rt ++ "/data/meta.db"
        dbFile rt = rt ++ "/data/meta.db"
        getRoot   = getEnv "DBGEN_ROOT"

--------------------------------------------------------------------------------

mkConfig :: Config -> DecsQ
mkConfig conf = [d| config' :: $(conT ''Config)
                    config' = $e
                 |]
  where e = dataToExpQ (fmap liftText . cast) conf

mkFuncs :: [Func] -> DecsQ
mkFuncs fs = [d|funcs' :: [$(conT ''Func)]
                funcs' = $e
             |]
  where e = dataToExpQ (fmap liftText . cast) fs

mkTabs :: [Table] -> DecsQ
mkTabs ts = [d|tables' :: [$(conT ''Table)]
               tables' = $e
            |]
  where e = dataToExpQ (fmap liftText . cast) ts

mkTrans :: [Transform] -> DecsQ
mkTrans ts = [d|transforms' :: [$(conT ''Transform)]
                transforms' = $e
             |]
  where e = dataToExpQ (fmap liftText . cast) ts
