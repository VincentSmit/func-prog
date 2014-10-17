{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module ModuleName where
import FPPrac

import RoseTree
import FPPrac.Trees.RedBlackTree

data Col = Red | Black | Grey
data RBTree = Tree Col NodeType
data NodeType = Leaf | Node Number RBTree RBTree



rbZetomC Red = RedG
rbZetomC Black = BlackG
rbZetomC Grey = GreyG
rbZetom (Tree c Leaf) = RBNodeG (rbZetomC c) "" []
rbZetom (Tree c (Node x t1 t2)) = (RBNodeG (rbZetomC c) (show x) ((rbZetom t1):[rbZetom t2]))

rbinsert x (Tree c Leaf) = (Tree Red (Node x (Tree Black Leaf) (Tree Black Leaf)))
rbinsert x (Tree c (Node y t1 t2))	| (x<y) = (Tree c (Node y (rbinsert x t1) t2))
									| otherwise = (Tree c (Node y t1 (rbinsert x t2)))
									
rTB (Tree _ dc) = (Tree Black dc)


cF (Tree Black (Node x 	(Tree Red (Node y (Tree Red dc) dc2)) (Tree Red (Node z dc3 dc4))))			=  (Tree Red (Node x 	(Tree Black (Node y (Tree Red dc) dc2))
																						(Tree Black (Node z dc3 dc4))))
cF (Tree Black (Node x 	(Tree Red (Node y dc2 (Tree Red dc)))(Tree Red (Node z dc3 dc4))))			=  (Tree Red (Node x 	(Tree Black (Node y dc2 (Tree Red dc)))
																						(Tree Black (Node z dc3 dc4))))
cF (Tree Black (Node x 	(Tree Red (Node y dc2 dc3))	(Tree Red (Node z (Tree Red dc) dc4))))			=  (Tree Red (Node x 	(Tree Black (Node y dc2 dc3))
																						(Tree Black (Node z (Tree Red dc) dc4))))
cF (Tree Black (Node x 	(Tree Red (Node y dc2 dc4))	(Tree Red (Node z dc3 (Tree Red dc)))))			=  (Tree Red (Node x 	(Tree Black (Node y dc2 dc4))
																								(Tree Black (Node z dc3 (Tree Red dc)))))
cF t1 = t1
																								
rebalance (Tree Black (Node	x (Tree Red (Node y (Tree Red (Node z lll llr)) lr) ) r)) = (Tree Black (Node y (Tree Red (Node z lll llr)) (Tree Red (Node x lr r))))																							
																								
rebalance (Tree Black (Node	x (Tree Red (Node y ll (Tree Red (Node z lrl lrr))) ) r)) = (Tree Black (Node z (Tree Red (Node y ll lrl)) (Tree Red (Node x lrr r))))																							
																								
rebalance (Tree Black (Node	x  l (Tree Red (Node y (Tree Red (Node z rll rlr)) rr) ))) = (Tree Black (Node z (Tree Red (Node x l rll)) (Tree Red (Node y rlr rr))))																							
																								
rebalance (Tree Black (Node	x  l (Tree Red (Node y  rl (Tree Red (Node z rrl rrr))) ))) = (Tree Black (Node y (Tree Red (Node x l rl)) (Tree Red (Node z rrl rrr))))

rebalance t1 = t1
bI x t1 = rTB (balance  (rbinsert x t1))






balance (Tree Black (Node x 	(Tree Red (Node y (Tree Red dc) dc2)) (Tree Red (Node z dc3 dc4)))) = cF (Tree Black (Node x 	(Tree Red (Node y (Tree Red dc) dc2)) (Tree Red (Node z dc3 dc4))))
balance (Tree Black (Node x 	(Tree Red (Node y dc2 (Tree Red dc)))(Tree Red (Node z dc3 dc4))))  = cF (Tree Black (Node x 	(Tree Red (Node y dc2 (Tree Red dc)))(Tree Red (Node z dc3 dc4))))
balance (Tree Black (Node x 	(Tree Red (Node y dc2 dc3))	(Tree Red (Node z (Tree Red dc) dc4))))	= cF (Tree Black (Node x 	(Tree Red (Node y dc2 dc3))	(Tree Red (Node z (Tree Red dc) dc4))))	
balance (Tree Black (Node x 	(Tree Red (Node y dc2 dc4))	(Tree Red (Node z dc3 (Tree Red dc))))) = cF (Tree Black (Node x 	(Tree Red (Node y dc2 dc4))	(Tree Red (Node z dc3 (Tree Red dc)))))

balance (Tree Black (Node	x (Tree Red (Node y (Tree Red (Node z lll llr)) lr) ) r)) = rebalance (Tree Black (Node	x (Tree Red (Node y (Tree Red (Node z lll llr)) lr) ) r))
balance (Tree Black (Node	x (Tree Red (Node y ll (Tree Red (Node z lrl lrr))) ) r)) = rebalance (Tree Black (Node	x (Tree Red (Node y ll (Tree Red (Node z lrl lrr))) ) r))
balance (Tree Black (Node	x  l (Tree Red (Node y (Tree Red (Node z rll rlr)) rr) ))) = rebalance (Tree Black (Node	x  l (Tree Red (Node y (Tree Red (Node z rll rlr)) rr) )))
balance (Tree Black (Node	x  l (Tree Red (Node y  rl (Tree Red (Node z rrl rrr))) ))) = rebalance (Tree Black (Node	x  l (Tree Red (Node y  rl (Tree Red (Node z rrl rrr))) )))


balance (Tree Black (Node x t1 t2)) =rebalance (cF(Tree Black (Node x (balance t1) (balance t2))))
balance (Tree Red (Node x t1 t2)) =  rebalance (cF(Tree Red (Node x (balance t1) (balance t2))))

balance ourLeaf = ourLeaf


leftmostvalue (Tree c (Node x (Tree Black Leaf) t2)) = x
leftmostvalue (Tree c (Node x t1 t2)) = leftmostvalue t1

removeLeftmostNode (Tree Black (Node x (Tree Black Leaf) (Tree Black Leaf))) = (Tree Grey Leaf)
removeLeftmostNode (Tree c (Node x (Tree Black Leaf) t2)) = t2
removeLeftmostNode (Tree c (Node x t1 t2)) = Tree c (Node x (removeLeftmostNode t1) t2)

greyColourFlip (Tree Black (Node x (Tree Grey nt) (Tree Black (Node y (Tree Black nt2) (Tree Black nt3))))) = (Tree Grey (Node x (Tree Black nt) (Tree Red (Node y (Tree Black nt2) (Tree Black nt3)))))
greyColourFlip (Tree Black (Node x (Tree Black  (Node y (Tree Black nt2) (Tree Black nt3)))  (Tree Grey nt))) = (Tree Grey (Node x(Tree Red (Node y (Tree Black nt2) (Tree Black nt3)))  (Tree Black nt)))

greyColourFlip (Tree c (Node p (Tree Grey g) (Tree Black (Node s (Tree Red (Node l (Tree Black a) (Tree Black b))) (Tree c2 r))))) = Tree c (Node l (Tree Black (Node p (Tree Black g) (Tree Black a))) (Tree Black (Node s (Tree Black b) (Tree c2 r))))
greyColourFlip (Tree c (Node p (Tree Black (Node s (Tree c2 r) (Tree Red (Node l (Tree Black a) (Tree Black b))))) (Tree Grey g))) = Tree c (Node l (Tree Black (Node s (Tree c2 r) (Tree Black a))) (Tree Black (Node p (Tree Black b) (Tree Black g))))

greyColourFlip (Tree Red (Node p (Tree Grey g) (Tree Black (Node s (Tree Black l) (Tree c r))))) = Tree Black (Node s (Tree Red (Node p (Tree Black g) (Tree Black l))) (Tree c r))
greyColourFlip (Tree Red (Node p (Tree Black (Node s (Tree c r) (Tree Black l))) (Tree Grey g))) = Tree Black (Node s (Tree c r) (Tree Red (Node p (Tree Black l) (Tree Black g))))

greyColourFlip (Tree Black (Node p (Tree Grey g) (Tree Black (Node s (Tree Black l) (Tree Red r))))) = Tree Black (Node s (Tree Black (Node p (Tree Black g) (Tree Black l))) (Tree Black r))
greyColourFlip (Tree Black (Node p (Tree Black (Node s (Tree Red r) (Tree Black l))) (Tree Grey g))) = Tree Black (Node s (Tree Black r) (Tree Black (Node p (Tree Black l) (Tree Black g))))

greyColourFlip (Tree Black (Node p (Tree Grey g) (Tree Red (Node s (Tree Black l) (Tree Black r))))) = Tree Black (Node s (greyColourFlip (Tree Red (Node p (Tree Grey g) (Tree Black l)))) (Tree Black r))
greyColourFlip (Tree Black (Node p (Tree Red (Node s (Tree Black r) (Tree Black l))) (Tree Grey g))) = Tree Black (Node s (Tree Black r) (greyColourFlip (Tree Red (Node p (Tree Black l) (Tree Grey g)))))
greyColourFlip t1 = t1
 
greyRebalance (Tree c (Node x t2 (Tree Grey Leaf))) = greyColourFlip (Tree c (Node x t2 (Tree Grey Leaf)))
greyRebalance (Tree c (Node x (Tree Grey Leaf) t2)) = greyColourFlip (Tree c (Node x (Tree Grey Leaf) t2))
greyRebalance (Tree c (Node x (Tree Black Leaf) (Tree Black Leaf))) =greyColourFlip(Tree c (Node x (Tree Black Leaf) (Tree Black Leaf)))

greyRebalance (Tree c Leaf) = Tree c Leaf
greyRebalance (Tree c (Node x t1 t2)) =greyColourFlip(Tree c (Node x (greyRebalance t1) (greyRebalance t2)))

valueOf (Tree c (Node x t1 t2)) = x

delete x t1 = balance (greyRebalance (delete2 x t1))
delete2 x (Tree c (Node y t1 (Tree Black Leaf)))	|x==y = t1
delete2 x (Tree c (Node y t1 t2)) 	| x == y 	= Tree c (Node (leftmostvalue t2) t1 (removeLeftmostNode t2)) 
									| x < y 	= Tree c (Node y (delete2 x t1) t2)
									| x > y		= Tree c (Node y t1 (delete2 x t2))

	
ourLeaf = Tree Black Leaf																					
																						
rbexampleTree = Tree Black 
						(Node 4 
								(Tree Red (Node 2 ourLeaf ourLeaf))
								(Tree Red (Node 7 ourLeaf ourLeaf))
																						)

fouteBoomRoot = Tree Red (Node 6 (Tree Red (Node 4 ourLeaf ourLeaf)) ourLeaf)

fouteBoom2 = Tree Black (Node 6 (Tree Red (Node 4 (Tree Red (Node 2 ourLeaf ourLeaf)) ourLeaf)) (Tree Red (Node 7 ourLeaf ourLeaf)))

fouteBoom3 = Tree Black (Node 6 (Tree Red (Node 4 (Tree Red (Node 3 ourLeaf ourLeaf) ) ourLeaf)) ourLeaf)

fouteBoom4 = Tree Black (Node 6 (Tree Red (Node 3 ourLeaf (Tree Red (Node 4 ourLeaf ourLeaf) ) )) ourLeaf)

boomZZZ = Tree Black (Node 4 ourLeaf ourLeaf)

boomex1 = Tree Black (Node 5 (Tree Grey Leaf) (Tree Black (Node 6 ourLeaf ourLeaf)))
boomex1sym = Tree Black (Node 5 (Tree Black (Node 6 ourLeaf ourLeaf)) (Tree Grey Leaf))

boomex2 = Tree Black (Node 6 (Tree Grey Leaf) (Tree Black (Node 10 (Tree Red (Node 8 ourLeaf ourLeaf)) ourLeaf))) 
boomex2sym = Tree Red (Node 6 (Tree Black (Node 4 ourLeaf (Tree Red (Node 5 ourLeaf ourLeaf)))) (Tree Grey Leaf))

boomex3 = Tree Red (Node 6 (Tree Grey Leaf) (Tree Black (Node 10 ourLeaf ourLeaf)))
boomex3sym = Tree Red (Node 8 (Tree Black (Node 6 ourLeaf ourLeaf)) (Tree Grey Leaf)) 

boomex4 = Tree Black (Node 6 (Tree Grey Leaf) (Tree Black (Node 10 ourLeaf (Tree Red (Node 11 ourLeaf ourLeaf)))))
boomex4sym = Tree Black (Node 8 (Tree Black (Node 6 (Tree Red (Node 5 ourLeaf ourLeaf)) ourLeaf)) (Tree Grey Leaf))

boomex5 = Tree Black (Node 6 (Tree Grey Leaf) (Tree Red (Node 10 (Tree Black (Node 8 ourLeaf ourLeaf)) ourLeaf)))
boomex5sym = Tree Black (Node 8 (Tree Red (Node 6 ourLeaf (Tree Black (Node 7 ourLeaf ourLeaf)))) (Tree Grey Leaf))

aftekenBoom = Tree Black (Node 15
                        (Tree Black (Node 7
                            (Tree Red (Node 3
                                (Tree Black (Node 1
                                    (Tree Red (Node 1
                                        (Tree Black Leaf)
                                        (Tree Black Leaf)
                                    ))
                                    (Tree Red (Node 2
                                        (Tree Black Leaf)
                                        (Tree Black Leaf)
                                    ))
                                ))
                                (Tree Black (Node 5
                                    (Tree Red (Node 4
                                        (Tree Black Leaf)
                                        (Tree Black Leaf)
                                    ))
                                    (Tree Red (Node 6
                                        (Tree Black Leaf)
                                        (Tree Black Leaf)
                                    ))
                                ))
                            ))
                            (Tree Black (Node 10
                                (Tree Red (Node 8
                                    (Tree Black Leaf)
                                    (Tree Black Leaf)
                                ))
                                (Tree Red (Node 12
                                    (Tree Black Leaf)
                                    (Tree Black Leaf)
                                ))
                            ))
                        ))
                        (Tree Black (Node 25
                            (Tree Black (Node 20
                                (Tree Black Leaf)
                                (Tree Black Leaf)
                            ))
                            (Tree Red (Node 30
                                (Tree Black (Node 28
                                    (Tree Black Leaf)
                                    (Tree Black Leaf)
                                ))
                                (Tree Black (Node 60
                                    (Tree Black Leaf)
                                    (Tree Black Leaf)
                                ))
                            ))
                        ))
                    )