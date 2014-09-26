import FPPrac
import FPPrac.Trees.ParseTree
import Data.Char

data BinTree a b = LeafBT b
                | NodeBT a (BinTree a b) (BinTree a b)
                deriving (Show, Eq)

data Unit = Nothing

type Tree1a = BinTree Number Number
type Tree1b = BinTree (Number, Number) (Number, Number)
type Tree1c = BinTree Number Unit

pp :: (Show a, Show b) => BinTree a b -> ParseTree
pp (LeafBT x) = ParseNode (show x) []
pp (NodeBT x t1 t2) = ParseNode (show x) [pp t1, pp t2]

-- 2 ------------------------------------------

