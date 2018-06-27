{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module PostCompilation.FillTables (fillTables) where

import           Control.Monad                      (unless, when)
import           Control.Monad.Loops                (whileM_)
import           Data.Map.Strict                    ((!))
import           Data.Text                          (Text, concat, unwords)
import           Data.Time                          as TI
import           Database.MySQL.Simple              (Connection, Only (..),
                                                     query_)
import           Prelude                            hiding (concat, unwords)

import           Compilation.TH                     (pipes)
import           PostCompilation.Utilities          (printV, queryInt,
                                                     roundFloat, setTfield)
import           PreCompilation.DataTypes.Misc      (Config (verbose),
                                                     ExitStatus)
import           PreCompilation.TH                  (config', tables',
                                                     transforms')

import           Compilation.Utilities              (mExecute_, mQuery)
import           PreCompilation.DataTypes.Table     (Col (colName, colType),
                                                     Table (cols, tableName))
import           PreCompilation.DataTypes.Transform (Transform (..))
import           PreCompilation.Utilities           (if', tShow, unsafeLookup)
{-
This module exports one function which manages the process of applying the
sequence of transformations to the database
-}
----------------------------------------------------------------

-- | Apply all the transformation methods of the database
fillTables :: Connection -> IO ()
fillTables conn = do mapM_ (processT conn) $ zip transforms' pipes
                     putStrLn "\t\tdone with fill fillTables"
                     return ()


-- Process a given transformation method from start to finish
processT ::  Connection -> (Transform,Connection -> IO ExitStatus) -> IO ()
processT conn (t,pipe) = do printV $ "\n\t\tNext is " ++ show (tHandle t)
                            printV "\t\t\tNOT adding columns if they don't exist (it's slow for some reason)"
                            when (False) $ addCols conn t
                            printV "\t\t\tCheck if any dependencies have failed"
                            failed <- failedDeps conn $ tHandle t
                            if failed
                                then
                                  do printV "\t\t\tThis transform depends on something that failed"
                                     setTfield conn t "status" "not executed"
                                else
                                  do printV "\t\t\tSet rows status to running"
                                     setTfield conn t "status" "running"
                                     start      <- TI.getCurrentTime
                                     exitStatus <- pipe conn
                                     setTfield conn t "status" $ tShow exitStatus
                                     end <- TI.getCurrentTime
                                     setTfield conn t "time" $ concat [rounder (TI.diffUTCTime end start)," s"]

                            printV "\t\t\tdone"

  where rounder  = tShow . roundFloat 1 . toRational -- round to one decimal


-- |
failedDeps :: Connection -> Text -> IO Bool
failedDeps conn n = do out <- queryInt conn q
                       case out of
                         [] -> return False
                         _  -> return True
  where q = concat ["select 1 from META_dependencies "
                               ,"join META_transform on parent = method "
                               ,"where child ='",n,"'and status in ('failed','not executed')"]

--------------------------------------------------------------------------------
-- | COLUMN STUFF
-- (If updating an already-existing database with a new schema,
--  we need to add columns)
---------------------------------------------------------------

-- | Adds columns if not already present
addCols :: Connection -> Transform -> IO ()
addCols conn tr = sequence_ $ addCol conn tab <$> iCs
  where tab = unsafeLookup tableName tables' (insTable tr)
        iCs = filter (\c->colName c `elem` insCols tr) $ cols tab

-- | add column in case it doesn't exist yet
addCol :: Connection -> Table -> Col -> IO ()
addCol conn tab col =  do check <- colExists conn t c
                          unless check (mExecute_ conn q)
                          return ()

  where q = concat ["ALTER TABLE ",t," ADD COLUMN ",c," ",ct]
        (c,ct,t) = (colName col,tShow (colType col),tableName tab)
--
-- | checks whether or not a column exists in a table
colExists :: Connection -> Text -> Text -> IO Bool
colExists  conn tName cName = null <$> (queryInt conn q)
  where q = unwords ["select 1 from information_schema.columns where table_name='"
                    ,tName,"' AND column_name = '",cName,"'"]
