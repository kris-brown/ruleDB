{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}

module Compilation.TH where
import           Compilation.MkBoilerPlate (genApps, genRenders, genShows,
                                            genUnTuples)
import           Compilation.MkPipes       (createPipes, mkMainPipeline)
{-

Generate compile time objects needed to make pipesa

config' :: Config
funcs'  :: [Func]
tables'  :: [Table]
transforms' ::  [Transform] (in the right order!)
-}

-- Boilerplate can be generated at the same step
$(genApps 1 50)
$(genUnTuples 1  50)
$(genRenders 25 50)
$(genShows 16 50)

$(createPipes)
$(mkMainPipeline)
