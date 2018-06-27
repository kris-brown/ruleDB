{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}

module PreCompilation.TH where
import           PreCompilation.MkMainObjects (createMainObjects)

$(createMainObjects)
