-- |
module Data.Tree.Ext where

import Data.Tree (Forest, Tree (..))

drawForest :: Show a => Forest a -> String
drawForest = unlines . map drawTree

drawTree :: Show a => Tree a -> String
drawTree = unlines . draw

-- | TODO: instead of always doing `show x`
-- try to use `Text.Pretty.Simple.pShow` for `Content Text`.
-- it may solve the unicode display problem.
-- other case are unlikely to have unicode. e.g. `TagOpen name attrs`
--
draw :: Show a => Tree a -> [String]
draw (Node x ts0) = lines (show x) ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
      "|" : shift "`- " "   " (draw t)
    drawSubTrees (t : ts) =
      "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)
