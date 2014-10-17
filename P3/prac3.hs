{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module ModuleName where
import FPPrac
import Data.Char
import Data.List
import FPPrac.Trees.RoseTree
data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a
data Tree1b = Leaf1b (Number,Number) | Node1b (Number,Number) Tree1b Tree1b
data Tree1c = Leaf1c  | Node1c Number Tree1c Tree1c
data Tree1d = Leaf1d (Number,Number) | Node1d  [Tree1d]


pp1a:: Tree1a ->RoseTree
pp1a (Leaf1a x) = RoseNode (show x) []

pp1a (Node1a x t1 t2) = RoseNode (show x) [pp1a t1, pp1a t2] 

pp1b:: Tree1b ->RoseTree
pp1b (Leaf1b (x,y)) = RoseNode (show (x,y)) []
pp1b (Node1b (x,y) t1 t2) = RoseNode (show (x,y)) [pp1b t1, pp1b t2] 

pp1c:: Tree1c ->RoseTree
pp1c (Leaf1c ) = RoseNode "" []
pp1c (Node1c x t1 t2) = RoseNode (show x) [pp1c t1, pp1c t2] 


pp1d (Leaf1d (x,y)) = RoseNode (show (x,y)) []
pp1d (Node1d (ts)) = RoseNode "" [pp1d t |t<-ts]

plus x (Leaf1a y) = Leaf1a (y+x)
plus x (Node1a y t1 t2) = Node1a (y+x) (plus x t1) (plus x t2) 

kwadraat (Leaf1a y) = Leaf1a (y^2)
kwadraat (Node1a y t1 t2) = Node1a (y^2) (kwadraat t1) (kwadraat t2) 

mapTree f (Leaf1a x) = Leaf1a (f x)
mapTree f (Node1a x t1 t2) = Node1a (f x) (mapTree f t1) (mapTree f t2)

plus2 x t1 = mapTree (+x) t1
kwadraat2 t1 = mapTree (^2) t1

telopNode (Leaf1b (x,y)) = Leaf1a (x+y)
telopNode (Node1b (x,y) t1 t2) = Node1a (x+y) (telopNode t1) (telopNode t2)

mapTree2 f (Leaf1b (x,y)) = Leaf1a (f (x,y)) 
mapTree2 f (Node1b (x,y) t1 t2) = Node1a (f (x,y)) (mapTree2 f t1) (mapTree2 f t2)

binspiegel (Leaf1a x) = Leaf1a x
binspiegel (Node1a x t1 t2) = Node1a x t2 t1

binspiegel2 (Leaf1d (x,y)) = Leaf1d (y,x)
binspiegel2 (Node1d ts) = Node1d (map binspiegel2 (reverse ts))


t_insert x Leaf1c = (Node1c x Leaf1c Leaf1c)
t_insert x (Node1c y t1 t2)	| y<x 	= Node1c y t1 (t_insert x t2)
							| otherwise = Node1c y (t_insert x t1) t2

makeTree  = foldr t_insert Leaf1c 


makeTreeRec xs = makeTreeRec2 xs Leaf1c
makeTreeRec2 [] t1 = t1

makeTreeRec2 (x:xs) t1 = makeTreeRec2 xs (t_insert x t1)

makeList Leaf1c = []
makeList (Node1c x t1 t2)= x:(makeList t1)++(makeList t2)

sortListByTree = makeList . makeTreeRec

sortTreeByList = makeTreeRec . makeList

zoek n Leaf1c = error "getal niet in boom"
zoek n (Node1c x t1 t2) 	| x==n = (Node1c x t1 t2)
							| n<x = zoek n t1
							| otherwise = zoek n t2

totDiepte 0 (Node1a x t1 t2) = Leaf1a x		
totDiepte m (Leaf1a x) = Leaf1a x					
totDiepte m (Node1a x t1 t2) = Node1a x (totDiepte (m-1) t1) (totDiepte (m-1) t2)

vervang (Leaf1a x) [] n = Leaf1a n
vervang (Node1a x t1 t2) [] n = Node1a n t1 t2
vervang (Node1a x t1 t2) (c:cs) n 	|c=='l' = Node1a x (vervang t1 cs n) t2
									|c=='r' = Node1a x t2 (vervang t2 cs n)

subboom (Leaf1a x) [] = Leaf1a x
subboom (Leaf1a x) cs = error "pad te lang"
subboom (Node1a x t1 t2) [] = Leaf1a x									
subboom (Node1a x t1 t2) (c:cs) | c=='l' = subboom t1 cs
								| c=='r' = subboom t2 cs



