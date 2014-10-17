import FPPrac
import RoseTree

-- a)
data Tree1a = Leaf1a Number 
            | Node1a Number Tree1a Tree1a
            deriving Show

pp1a :: Tree1a -> RoseTree
pp1a (Leaf1a n) = RoseNode (show n) []
pp1a (Node1a n t1 t2) = RoseNode (show n) [pp1a t1, pp1a t2]