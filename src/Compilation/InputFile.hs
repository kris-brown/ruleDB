{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Compilation.InputFile (mkInputFile,getLangFromTemp) where

import           Data.List                          (nub)
import           Data.Maybe                         (mapMaybe)
import           Data.Text                          (Text, concat, intercalate)
import           PreCompilation.DataTypes.DataTypes (SQLType (..))
import           PreCompilation.DataTypes.Misc      (Func (..), Language (..))
import           PreCompilation.DataTypes.Transform (TFA (..), TType (..),
                                                     TempFunc (..),
                                                     Template (..),
                                                     Transform (..), getTFAfunc)
import           PreCompilation.TH                  (funcs')
import           PreCompilation.Utilities           (if', tShow, unsafeLookup)
import           Prelude                            hiding (concat)
-- | Construct a file that describes a CLI program that, when called with a
--   line from the Transform's query, returns a value for insertion back into
--   the database.

mkInputFile :: Transform -> Text
mkInputFile t = intercalate (barrier l) [header l
                                        ,printParseDef l t
                                        ,fDefs l t
                                        ,parseCall l
                                        ,body l t]
  where l = getLangFromTemp $ template t
-- | Business that belongs at the top any any such program
header :: Language  -> Text
header l
 | l == Python = "import sys\nimport ast\nimport dbgenPython.support.function_analyzer as fa"
 | otherwise = error "badlang"

-- | For organization purposes, a delimiter for sections of code
barrier ::Language  -> Text
barrier l
 | l == Python = "\n######################\n"
 | otherwise = error "badlang"

-- | Docstring
printParseDef ::Language -> Transform -> Text
printParseDef l
 | l == Python = mkPyPPDef
 | otherwise = error "badlang"

-- | Docstring
fDefs :: Language -> Transform -> Text
fDefs l t
 | l == Python = intercalate "\n" $ map mkPyDef fs
 | otherwise = error "badlang"
   where fs = allFuncs $ tempFuncs $ template t


-- | Need all tempfunc functions but also any functions that may be pure arguments
allFuncs :: [TempFunc] -> [Text]
allFuncs tfs = nub $ fs ++ mapMaybe getTFAfunc tfas
  where fs = map func tfs
        tfas = concatMap tfArgs tfs

-- | docstring
mkPyDef :: Text -> Text
mkPyDef f = concat [f," = fa.path_to_func('",p,"')"]
  where p = funcPath $ unsafeLookup funcHandle funcs' f

-- curently fixed at Str input and [Str] output...needs to be parameterized
mkPyPPDef :: Transform -> Text
mkPyPPDef t = concat ["def printer(",mbStar,"x):\n\tprint(",selectPrinter t,")"
                        ,"\ndef parser(x=None):\n\t",selectParser t]
  where listOutput = (tType t == Insert) && not (null tfs)
        oneOutput  = (tType t /= Insert) && (length (insCols t) == 1) -- is there a better TType constraint on this?
        tfs       = tempFuncs $ template t
        mbStar     = if' (listOutput || oneOutput) "" "*"

-- | Template
selectPrinter :: Transform -> Text
selectPrinter t
  | listOutput     && oneOutput     = "'&&&'.join(map(str,x))"
  | listOutput     && not oneOutput = "'&&&'.join(['@@@'.join(map(str,y)) for y in x])"
  | not listOutput && oneOutput     = "x"
  | not listOutput && not oneOutput = "'@@@'.join(map(str,x))"
  | otherwise = error "The Impossible Happened"
  where oneOutput  = length (insCols t) == 1
        listOutput = tType t == Insert

-- | docstring
selectParser :: Transform -> Text
selectParser t
 | n == 0    = "return None"
 | n == 1    = concat ["return (",head its,"(x),)"]
 | otherwise = concat [intercalate "," [concat ["x",tShow i] | i <- [1..n] ]
                        ," = x.split('@@@')"
                        ,"\n\treturn (",intercalate "," $ zipWith f its [1..n],")"]
  where its = map dtRenderer $ inType t
        n = length its
        f it i = concat [it,"(x",tShow i,")"]

-- | This could get complicated later, but simple for python?
dtRenderer :: SQLType -> Text
dtRenderer dt
 | dt `elem` [SQLVarchar,SQLInt,SQLFloat,SQLText] = "ast.literal_eval"
 | otherwise = error $ "need primitive type for python input, not "++show dt

-- | Store the results of parsing in a variable
parseCall :: Language  -> Text
parseCall l
 | l == Python =  "\nstdin = parser(sys.argv[1])"
 | otherwise = error "badlang"

-- | The body of the program
body :: Language -> Transform -> Text
body l t
 | l == Python = mkPyBody (template t)
 | otherwise = error "badlang"

-- | body of a python program (a sequence of Template Function calls)
mkPyBody :: Template -> Text
mkPyBody (Template tfs stdOut) = intercalate "\n" strs
  where strs = map renderPyTF tfs ++ [renderPyStdOut stdOut]

-- | Render a template function call (in Python)
renderPyTF :: TempFunc -> Text
renderPyTF (TempFunc f i xs)= concat [ f, "_",i,"="
                                       , f,"(",renderedXs,")"]
  where renderedXs = intercalate "," $ map renderPyTFA xs

-- | Render the argument for a template function call (in Python)
renderPyTFA :: TFA -> Text
renderPyTFA (FuncArg fun) =  fun
renderPyTFA (TFC v)       = v
renderPyTFA (StdIn n)     = concat ["stdin[",tShow n ,"]"]
renderPyTFA (TempFuncArg src i ind) = concat [  src
                                                 ,"_", i
                                                 ,maybe "" f ind]
    where f x = concat ["[",tShow x,"]"]

-- | Collect the results for STDOUT from the results of template function calls
renderPyStdOut :: [TFA] -> Text
renderPyStdOut tfas = concat ["stdout = printer(",outargs,")"]
  where outargs = intercalate "," $ map renderPyTFA tfas

--------------------------------------------------------------------------------
getLangFromTemp  :: Template -> Language
getLangFromTemp t
  | null tfs  = Python -- should there be an 'identity language'
  | otherwise = language $ unsafeLookup funcHandle funcs' $ func $ head tfs
  where tfs = tempFuncs t
