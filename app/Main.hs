{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Database.MySQL.Simple      (close, connect)

import           PostCompilation.FillTables (fillTables)
import           PostCompilation.MetaTable  (dropMetatables, fillMetaData,
                                             initTables)
import           PostCompilation.Utilities  (connectInfo, removeIfExists)
import           PreCompilation.TH          (config')

-------------------------------------------------------------------------------

--
main :: IO ()
main = do putStrLn ("\t-config is: "++show config')

          putStrLn "\tMaking connection to DB "
          conn <- connect $ connectInfo

          putStrLn "\t-Clearing MetaTable"
          dropMetatables conn

          putStrLn "\t-Initializing tables if they do not exist"
          initTables conn

          putStrLn "\t-Filling metadata"
          fillMetaData conn

          putStrLn "\t-Expanding Knowledge Base"
          fillTables conn

          putStrLn "\t-Removing temp file"
          removeIfExists "tempscript"

          putStrLn "\t-Closing DB connection"
          close conn
