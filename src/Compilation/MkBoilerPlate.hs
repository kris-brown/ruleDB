{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}

module Compilation.MkBoilerPlate
  (genUnTuples
    ,genApps
    ,genRenders
    ,genShows)

  where
import           Compilation.Utilities             (concatMapM)
import           Control.Monad                     (replicateM)
import           Data.Map.Strict                   (Map, foldrWithKey, fromList)
import           Database.MySQL.Simple.Param       (Param (render))
import           Database.MySQL.Simple.QueryParams (QueryParams (renderParams))
import           Language.Haskell.TH               (Body (NormalB),
                                                    Clause (Clause),
                                                    Dec (FunD, InstanceD, SigD),
                                                    DecsQ,
                                                    Exp (AppE, ConE, InfixE, LamE, ListE, TupE, VarE),
                                                    Pat (TupP, VarP), Q,
                                                    Type (AppT, ArrowT, ConT, ForallT, ListT, TupleT, VarT),
                                                    mkName, newName)
--------------------------------------------------------------------------------

-- | Make a function like so:
-- app3 :: Map Text (a,b,c) -> [(a,b,c,Text)]
-- app3 = foldrWithKey (\txt (a,b,c) -> (:) (a,b,c,txt)) []
appN :: Int -> Q (Exp,Maybe Type)
appN n = do
    xs <- replicateM (n+1) (newName "x")
    let lamExp = AppE (ConE '(:)) $ TupE $ map VarE xs
        rest   = TupP $ map VarP $ init xs
        lam    = LamE [VarP $ last xs, rest] lamExp
    return $ (AppE (fold lam) list, Nothing)
  where fold = AppE (VarE 'foldrWithKey)
        list = ListE []

-- | Construct a function like so:
-- unTuple4 :: [(Text,a,b,c,d)] -> Map Text (a,b,c,d)
-- unTuple4 = fromList . map (\(txt,a,b,c,d)->(txt,(a,b,c,d)))
unTupleN :: Int -> Q (Exp,Maybe Type)
unTupleN n = do
    xs <- replicateM (n+1) (newName "x")
    let lam     = LamE lamArgs lamExp
        lamArgs = [TupP $ map VarP xs]
        lamExp  = TupE [VarE (head xs),TupE $ map VarE (tail xs)]
        expr    = InfixE fList (VarE '(.)) (Just $ mapE lam)

        typ     = ForallT [] [AppT (ConT ''Ord) (VarT (head xs))] typBody
        typBody = AppT (AppT ArrowT lisT) mapT
        lisT    = AppT ListT (tupTyp $ tail xs ++ [head xs])
        mapT    = AppT (AppT (ConT ''Map) (VarT $ head xs)) (tupTyp (tail xs))
    return $ (expr,Just typ)
  where mapE   = AppE (VarE 'map)
        fList  = Just (VarE 'fromList)
        tupTyp xs = case length xs of
          1 -> VarT $ head xs
          m -> foldr (\next acc -> AppT acc (VarT next)) (TupleT m) xs
-- |
renderN :: Int -> Q Dec
renderN n = do
    xs <- replicateM n (newName "x")
    let tup   = AppT (ConT ''QueryParams) $ tupTyp xs
        tuplT = map tfTyp xs
        fun   = FunD 'renderParams [Clause [TupP $ map VarP xs] normB []]
        normB = NormalB (ListE $ map tfExp xs)
    return $ InstanceD Nothing tuplT tup [fun]
  where tfTyp = AppT (ConT ''Param) . VarT
        tfExp = AppE (VarE 'render) . VarE
        tupTyp = foldr (\next acc -> AppT acc (VarT next)) (TupleT n)

-- |
showN :: Int -> Q Dec
showN n = do
    xs <- replicateM n (newName "x")

    let tup = AppT (ConT ''Show) $ tupTyp xs
        tupT =  map tfTyp xs
        fun = FunD 'show [Clause [TupP $ map VarP xs] normB []]
        normB = NormalB (AppE (VarE 'concat) $ ListE $ map tfExp xs)
    return $ InstanceD Nothing tupT  tup [fun]
  where tfTyp  x= AppT (ConT ''Show) $ VarT x
        tfExp x = AppE (VarE 'show) $ VarE x
        tupTyp = foldr (\next acc -> AppT acc (VarT next)) (TupleT n)

--------------------------------------------------------------------------------
-- Top level functions
-----------------------
genBoilerPlate :: String -> (Int -> Q (Exp,Maybe Type)) -> Int -> Int -> DecsQ
genBoilerPlate n f i1 i2 = concatMapM mkF [i1..i2]
  where mkF ith = do (x,mT) <- f ith
                     let name = mkName $ n ++ show ith
                     let typ = maybe [] (\t->[SigD name t]) mT
                     let fun = FunD name [Clause [] (NormalB x) []]
                     return $ fun : typ
-------------------------------------------------------
genUnTuples :: Int -> Int -> DecsQ
genUnTuples = genBoilerPlate "unTuple" unTupleN

genApps :: Int -> Int -> DecsQ
genApps = genBoilerPlate "app" appN

genRenders :: Int -> Int -> DecsQ
genRenders i1 i2 = mapM renderN [i1..i2]

genShows :: Int -> Int -> DecsQ
genShows i1 i2 = mapM showN [i1..i2]
