{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}

module PreCompilation.Utilities

  (
  if'
  ,liftText
  ,tShow
  ,trim
  ,comma
  ,unComma
  ,unsafeLookup
  ,shell'
  ,getExitStatus
  )
  where

-- External Modules
import           Data.Char                     (isSpace)
import           Data.List                     (dropWhileEnd, find)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text, concat, intercalate, pack,
                                                splitOn, unpack)
import           Debug.Trace                   (trace)
import           Language.Haskell.TH           (Exp (AppE, VarE), Q)
import           Language.Haskell.TH.Syntax    (lift)
import           Prelude                       hiding (concat)
import           System.Process                (readProcessWithExitCode)

-- Internal Modules
import           PreCompilation.DataTypes.Misc (Error (Error), ExitStatus (..))
{-
This module contains some general functions that everyone needs, so it
usually gets imported (unqualified) into everything else
-}

-- ----------------------------------------------------------------------------
-- BASIC HASKELL
----------------

-- | Functional implementation of
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
-----------------------------------------------------------------------------------
-- TEXT RELATED

--
-- | Allows one to use a Text inside a quasiquote
liftText :: Text -> Q Exp
liftText txt = AppE (VarE 'pack) <$> lift (unpack txt)



-- | Render as Text
tShow :: Show a => a -> Text
tShow = pack . show

-- | Remove leading and trailing spaces
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


comma :: [Text]->Text
comma = intercalate ","
unComma :: Text -> [Text]
unComma "" = []
unComma x  = splitOn "," x

-- | Set the status of a given transform function in the meta table
unsafeLookup :: (a->Text)->[a] -> Text -> a
unsafeLookup getName fs n = fromMaybe (error $ "Cannot find "++show n)
                                      (find predicate fs)
  where predicate f = getName f == n

--------------------------------------------------------------------------------


-- | Execute shell command, trim stdout or return an error
-- | what does the last argument for P.rPWEC (called STDIN) do?
shell' :: String -> [String] -> IO (Either Error String)
shell' ex args = do (_,stdO,stdE) <- readProcessWithExitCode ex args ""
                    case stdE of
                      "" -> return $ Right $ trim stdO
                      _  -> return $ Left $ Error $ pack stdE


---------------------------------------------------------------
-- |
getExitStatus :: Int -> Int -> Int -> ExitStatus
getExitStatus numInput numFiltered numErr
 | trace (show (concat ["numInput ",tShow numInput,"\nnumFiltered ",tShow numFiltered,"\nnumErr ",tShow numErr])) False = undefined
 | numInput == 0    = NoInput
 | numFiltered == 0 = AlreadyComputed
 | numErr > 0       = Failed
 | otherwise        = Completed
