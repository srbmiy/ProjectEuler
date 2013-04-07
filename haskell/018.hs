---------------------------------------------------------------
-- Project Euler, Problem 018
---------------------------------------------------------------

module Problem018 where

import Data.List

data BinTree a = EmpTree | Node {node:: a,
                                 leftTree:: BinTree a,
                                 rightTree:: BinTree a}

instance Eq a => Eq (BinTree a) where
  (==) EmpTree EmpTree = True
  (==) EmpTree (Node _ _ _) = False
  (==) (Node _ _ _) EmpTree = False
  (==) (Node n t0 t1) (Node m s0 s1) = (n == m)&&(t0 == s0)&&(t1 == s1)

type Weight a = [a] -> Int

findPath:: (Eq a) => BinTree a -> Weight a -> [a]
findPath t weight = if t == EmpTree then [] 
                    else let n = node t
                             lt = leftTree t
                             rt = rightTree t
                             lpath = findPath lt weight
                             rpath = findPath rt weight
                         in if (weight lpath <= weight rpath) then n:lpath else n:rpath



testTree:: BinTree Int
testTree = Node {node = 1,
                 leftTree = Node 2 EmpTree EmpTree,
                 rightTree = Node 3 EmpTree EmpTree}

myweight::Weight Int
myweight xs = sum xs

main = do
  let weight xs = sum xs
  let p = findPath testTree myweight
  mapM_ print p