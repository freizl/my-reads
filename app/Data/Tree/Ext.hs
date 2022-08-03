-- |
module Data.Tree.Ext where

import Data.Tree (Forest, Tree (..))

drawForest :: Show a => Forest a -> String
drawForest = unlines . map drawTree

drawTree :: Show a => Tree a -> String
drawTree = unlines . draw

draw :: Show a => Tree a -> [String]
draw (Node x ts0) = lines (show x) ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
      "|" : shift "`- " "   " (draw t)
    drawSubTrees (t : ts) =
      "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)
