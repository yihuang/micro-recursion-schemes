{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.Functor.Identity
import           Data.List                (foldl')
import           Language.Haskell.TH
import           Test.HUnit

data Expr a
    = Lit a
    | Add (Expr a) (Expr a)
    | Expr a :* [Expr a]
  deriving (Show)

makeBaseFunctor ''Expr

data Expr2 a
    = Lit2 a
    | Add2 (Expr2 a) (Expr2 a)
  deriving (Show)

makeBaseFunctorWith (runIdentity $ baseRulesCon (\_-> Identity $ mkName . (++ "'") . nameBase) baseRules
    >>= baseRulesType (\_ -> Identity $ mkName . (++ "_") . nameBase)
    ) ''Expr2

expr1 :: Expr Int
expr1 = Add (Lit 2) (Lit 3 :* [Lit 4])

-- This is to test newtype derivation
--
-- Kind of a list
newtype L a = L { getL :: Maybe (a, L a) }
  deriving (Show, Eq)

makeBaseFunctor ''L

cons :: a -> L a -> L a
cons x xs = L (Just (x, xs))

nil :: L a
nil = L Nothing

-- Test #33
data Tree a = Node {rootLabel :: a, subForest :: Forest a}
  deriving (Show)
type Forest a = [Tree a]

makeBaseFunctor ''Tree

main :: IO ()
main = do
    let expr2 = ana divCoalg 55 :: Expr Int
    14 @=? cata evalAlg expr1
    55 @=? cata evalAlg expr2

    let lBar = cons 'b' $ cons 'a' $ cons 'r' nil
    "bar" @=? cata lAlg lBar
    lBar @=? ana lCoalg "bar"

    let expr3 = Add2 (Lit2 21) $ Add2 (Lit2 11) (Lit2 10)
    42 @=? cata evalAlg2 expr3

    let expr4 = Node 5 [Node 6 [Node 7 []], Node 8 [Node 9 []]]
    35 @=? cata treeAlg expr4
  where
    -- Type signatures to test name generation
    evalAlg :: ExprF Int Int -> Int
    evalAlg (LitF x)   = x
    evalAlg (AddF x y) = x + y
    evalAlg (x :*$ y)  = foldl' (*) x y

    evalAlg2 :: Expr2_ Int Int -> Int
    evalAlg2 (Lit2' x)   = x
    evalAlg2 (Add2' x y) = x + y

    divCoalg x
        | x < 5     = LitF x
        | even x    = 2 :*$ [x']
        | otherwise = AddF x' (x - x')
      where
        x' = x `div` 2

    lAlg (LF Nothing)        = []
    lAlg (LF (Just (x, xs))) = x : xs

    lCoalg []       = LF { getLF = Nothing } -- to test field renamer
    lCoalg (x : xs) = LF { getLF = Just (x, xs) }

    treeAlg :: TreeF Int Int -> Int
    treeAlg (NodeF r f) = r + sum f
