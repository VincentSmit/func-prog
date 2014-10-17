import FPPrac
import FPPrac.Graphs
import Data.Maybe
import Data.Char (isDigit)
import Data.List 
import Debug.Trace
    

-- | Store datatype
--   Hiermee worden alle variabelen van het
--   programmer gedefinieerd
data Store = Store 
             { pressedN :: Bool
             , pressedR :: Bool
             , pressedE :: Bool
             , pressedD :: Bool
             , pressedW :: Bool
             , pressedF :: Bool
             , pressedX :: Bool
             , pressedZ :: Bool
             , pressedP :: Bool
             , pressedS :: Bool
             , node1Select :: Maybe Node
             , node2Select :: Maybe Node
             , graph :: Graph
             }                

             
-- | Begingraph
--   Dit is de begintoestand van de graaf             
beginGraph = Graph [('a', (50,50), Orange), ('b', (100, 100), Black)] [('a', 'b', Black, 5)] Undirected Weighted

-- | BeginStore
--   Dit is de begintoestand van de store
beginStore = Store  { pressedN = False
                    , pressedR = False
                    , pressedE = False
                    , pressedD = False
                    , pressedW = False
                    , pressedF = False
                    , pressedX = False
                    , pressedZ = False
                    , pressedP = False
                    , pressedS = False
                    , node1Select = Nothing
                    , node2Select = Nothing
                    , graph   = beginGraph
                    }
 
-- | Instructions
--   Dit is een lijst van alle mogelijke 
--   instructies 
instructions = Instructions [ "Instructions",
                              "Press 'n' and click on the screen to create a new node",
                              "Press 'r', click on a node and press a letter to rename the node",
                              "Press 'e', click on two nodes to create an edge",
                              "Press 'd', click on a node to delete the node",
                              "Press 'w', click on two nodes and press a number to weight the edge in between",
                              "Press 'f', click on two nodes to delete an edge"
                            ]                             

-- | Resetcommands
--   Deze functie reset alle mogelijke commando variabelen van de store                            
resetCommands :: Store -> Store
resetCommands s = s { pressedN = False
                    , pressedR = False
                    , pressedE = False
                    , pressedD = False
                    , pressedW = False
                    , pressedF = False
                    , pressedX = False
                    , pressedZ = False
                    , pressedP = False
                    , pressedS = False
                    , node1Select = Nothing
                    , node2Select = Nothing
                    }

-- | Start
--   Deze functie start het programma
--   De functie preEventloop start de server
--   en neemt de functie eventloop mee als handler
--   Ook wordt de begintoestand van de store meegegeven                    
start = preEventloop eventloop beginStore
                
-- | AddNode
--  Deze functie voegt de node toe aan de graaf
addNode :: Node -> Graph -> Graph
addNode n g@(Graph{nodes=ns}) = g {nodes=(n:ns)}

-- | AddEdge
-- Deze functie voegt de edge toe aan de graaf
addEdge :: Edge -> Graph -> Graph
addEdge e g@(Graph{edges=es}) = g {edges=(e:es)}

-- | Geeft de nodes terug die horen bij de edge
findNodesByEdge :: Edge -> Graph -> (Node, Node)
findNodesByEdge (l1, l2, _, _) graph = (n1, n2)
                                    where
                                        Just n1 = findNode l1 graph
                                        Just n2 = findNode l2 graph

-- | Geeft de node terug horend bij het label
findNode :: Label -> Graph -> Maybe Node
findNode l (Graph {nodes=nodes}) | null n = Nothing
                                 | otherwise = Just (head n)
                                where
                                    n = filter (\(nl, _, _) -> l == nl) nodes

-- | Geeft de edge terug horend bij de twee labels
findEdge :: Label -> Label -> Graph -> Maybe Edge
findEdge l1 l2 (Graph {edges=es}) | null e    = Nothing
                                  | otherwise = Just (head e)
                                  where
                                    e = filter (\(el1, el2, _, _) -> l1 == el1 && l2 == el2) es

-- | Geeft alle edges terug die van of naar de node gaan
--   die bijbehorend label heeft                                    
findEdgesSingleLabel :: Label -> Graph -> [Edge]
findEdgesSingleLabel l (Graph{edges=es}) = filter (\(el1, el2, _, _) -> el1 == l || el2 == l) es

-- | Verwijdert de node horend bij het label uit de graaf
removeNode :: Label -> Graph -> Graph
removeNode l g@(Graph{nodes=ns}) | nM == Nothing = g 
                                 | otherwise     = g {nodes = ns'}
                                where
                                    nM = findNode l g
                                    n  = fromJust nM
                                    ns' = delete n ns

-- | Verwijdert de edge horend bij de twee labels uit
--   de graaf
removeEdge :: Label -> Label -> Graph -> Graph
removeEdge l1 l2 g@(Graph{edges=es}) | eM == Nothing = g 
                                     | otherwise     = g {edges = es'}
                                    where
                                        eM  = findEdge l1 l2 g
                                        e   = fromJust eM
                                        es' = delete e es

-- | Verwijdert alle edges die van of naar de node gaan
--   horend bij het gegeven label
removeEdgesSingleLabel :: Label -> Graph -> Graph
removeEdgesSingleLabel l g@(Graph{edges=es}) = g {edges = es'}
                            where
                                es' = es \\ (findEdgesSingleLabel l g)

-- | Geeft de grafische output om de node te maken
nodeToOutput :: Node -> GraphOutput
nodeToOutput (l, p, c) = NodeG l p c


-- | Geeft de grafische output om de edge te maken
--  Deze functie houdt rekening met de eigenschappen
--  van de graaf zoals gewicht en directie
edgeToOutput :: Graph -> Edge -> GraphOutput
edgeToOutput graph@(Graph {weighted=weighted, directed=directed}) e@(l1, l2, c, w) | weighted == Weighted  = WeightedLineG n1 n2 w c Thin directed
                                                                                   | otherwise             = LineG n1 n2 c Thin directed 
                                                                                   where
                                                                                       (n1, n2) = findNodesByEdge e graph
graphToOutput :: Graph -> [GraphOutput]
graphToOutput graph@(Graph{nodes=n,edges=e,weighted=w,directed=d}) = map nodeToOutput n ++ map (edgeToOutput graph) e

-- | De Eventloop
--   Dit is het hart van het grafische IO programma.
--   Elk input dat van belang is wordt gemapt naar
--   de bijbehorende uitvoer.
eventloop :: Store -> GraphInput -> ([GraphOutput], Store)

-- | Deze functie hernoemt een node wanneer alle 
--   benodigde variabelen zijn gezet
eventloop s@(Store { pressedR = True
                   , node1Select = Just n
                   , graph = g
                   }) (KeyPress l) = (output, s'{graph=graph'})
                                  where
                                      (output, graph') = renameNode (head l) n g
                                      s'               = resetCommands s

-- | Deze functie geeft een gewicht aan een edge
--   wanneer alle benodigde variabelen zijn gezet
eventloop s@(Store { pressedW    = True
                   , node1Select = Just n1
                   , node2Select = Just n2
                   , graph       = g
                   }) (KeyPress d) | isDigit (head d) = (output, s'{graph=g'})
                                   | otherwise        = ([], s)
                                   where
                                        s' = resetCommands s
                                        i  = read d
                                        (output, g') = weightEdge n1 n2 i g

eventloop s@(Store {graph = bg}) Start = (graphToOutput bg, s)

-- | Zet het bijbehorende commando bij de toetsaanslag                            
eventloop s (KeyPress "n") = ([], s' {pressedN = True})
                      where
                          s' = resetCommands s

-- | Zet het bijbehorende commando bij de toetsaanslag                          
eventloop s (KeyPress "r") = ([], s' {pressedR = True})
                      where
                          s' = resetCommands s
                          
-- | Zet het bijbehorende commando bij de toetsaanslag                          
eventloop s (KeyPress "e") = ([], s' {pressedE = True})
                      where
                          s' = resetCommands s
                          
-- | Zet het bijbehorende commando bij de toetsaanslag                          
eventloop s (KeyPress "d") = ([], s' {pressedD = True})
                      where
                          s' = resetCommands s
                          
-- | Zet het bijbehorende commando bij de toetsaanslag                          
eventloop s (KeyPress "w") = ([], s' {pressedW = True})
                      where
                          s' = resetCommands s
                          
-- | Zet het bijbehorende commando bij de toetsaanslag                          
eventloop s (KeyPress "f") = ([], s' {pressedF = True})
                      where
                          s' = resetCommands s                                          

-- | Zet het bijbehorende commando bij de toetsaanslag
eventloop s (KeyPress "x") = ([], s' {pressedX = True})
                      where
                          s' = resetCommands s   

-- | Zet het bijbehorende commando bij de toetsaanslag
eventloop s (KeyPress "z") = ([], s' {pressedZ = True})
                      where
                          s' = resetCommands s   

-- | Zet het bijbehorende commando bij de toetsaanslag
eventloop s@(Store{graph=g}) (KeyPress "p") = (o, s' {pressedP = True})
                      where
                          s' = resetCommands s   
                          (o,_) = blackWhite g

-- | Zet het bijbehorende commando bij de toetsaanslag
eventloop s@(Store{graph=g}) (KeyPress "s") = (o, s' {pressedS = True})
                      where 
                        s' = resetCommands s
                        (o,_) = colourSubGraph g

-- | Deze functie geeft het correcte gedrag bij een muisklik.
--   Afhankelijk van welke toetsaanslag al is ingedrukt en of er
--   op een nodige is geklikt, wordt een variabele gezet of de complete
--   Store gezet met resetCommands.
eventloop s@(Store pn pr pe pd pw pf px pz pp ps n1s n2s g) (MouseUp MLeft pos)  | pn && node == Nothing              = (output1, s' {graph=graph1})
                                                                        | pd && node /= Nothing                    = (output2, s' {graph=graph2})
                                                                        | pr && n1s  == Nothing                    = ([], s{node1Select = node})
                                                                        | pe && n1s  == Nothing                    = ([], s{node1Select = node})
                                                                        | pe && n1s  /= Nothing && node /= Nothing = (output3, s' {graph=graph3})
                                                                        | pw && n1s == Nothing                     = ([], s{node1Select = node})
                                                                        | pw && n1s /= Nothing                     = ([], s{node2Select = node})
                                                                        | pf && n1s == Nothing                     = ([], s{node1Select = node})
                                                                        | pf && n1s /= Nothing                     = (output4, s' {graph=graph4})
                                                                        | px && node /= Nothing                    = (output5, s' {graph=graph5})
                                                                        | pz && node /= Nothing                    = (output6, s' {graph=graph6})
                                                                        | ps && node /= Nothing                    = (output7, s'{graph=graph7})
                                                                        | otherwise                                = ([], s)
                                                                              where
                                                                                (output1, graph1) = insertNode pos g
                                                                                (output2, graph2) = deleteNode (fromJust node) g
                                                                                (output3, graph3) = insertEdge (fromJust n1s) (fromJust node) g
                                                                                (output4, graph4) = deleteEdge (fromJust n1s) (fromJust node) g
                                                                                (output5, graph5) = colourNeighbour (fromJust node) g
                                                                                (output6, graph6) = colourNode (fromJust node) Red g
                                                                                (output7, graph7) = colourSubGraph g
                                                                                node              = onNode (nodes g) pos
                                                                                s' = resetCommands s

-- | Voor alle andere uitvoer hoeft er niks te gebeuren                                                                                
eventloop s _ = ([], s)  

-- | Deze functie insert een node in de graaf op de bijbehorende
--   positie en geeft het een lege label. Ook wordt de grafische
--   uitvoer gegeven.
insertNode :: Pos -> Graph -> ([GraphOutput], Graph)
insertNode pos g@(Graph {nodes=ns}) | nM == Nothing = (output, g')
                                    | otherwise     = ([], g)
                                    where
                                        l = ' '
                                        nM = findNode l g
                                        n = (l, pos, Black)
                                        g' = addNode n g
                                        output = [RemoveNodeG l, nodeToOutput n]

-- | Deze functie verwijdert een node uit de graaf. Ook wordt de
--   grafische uitvoer gegeven.
deleteNode :: Node -> Graph -> ([GraphOutput], Graph)
deleteNode (l, _, _) g = (output, g'')
                    where
                        g' = removeNode l g
                        edgesFromNode = findEdgesSingleLabel l g'
                        g'' = removeEdgesSingleLabel l g'
                        output = RemoveNodeG l:(map (\(el1, el2, _, _)-> RemoveEdgeG el1 el2) edgesFromNode)

-- | Deze functie insert een edge in de graaf.
--   Ook wordt de grafische uitvoer gegeven.
insertEdge :: Node -> Node -> Graph -> ([GraphOutput], Graph)
insertEdge n1@(l1,_,_) n2@(l2,_,_) g@(Graph {directed=d}) | eM == Nothing = ([output], g')
                                                          | otherwise     = ([], g)
                                                          where
                                                              eM = findEdge l1 l2 g
                                                              e = (l1, l2, Black, -1)
                                                              g' = addEdge e g
                                                              output = LineG n1 n2 Black Thin d

-- | Hernoemt een node en update alle bijbehorende edges
--   Geeft ook de bijbehorende grafische uitvoer
renameNode :: Label -> Node -> Graph -> ([GraphOutput], Graph)
renameNode l' (l, p, c) g = (outN ++ outEs, g''')
                        where
                            g'           = removeNode l g
                            n'           = (l', p, c)
                            g''          = addNode n' g'
                            outN         = [RemoveNodeG l, nodeToOutput n']
                            (outEs, g''') = renameEdges l l' g''

-- | Hernoemt alle edges horend tot de oldL. Alle oldL wordt
--   vervangen door l. Ook geeft deze functie de bijbehorende
--   grafische uitvoer.
renameEdges :: Label -> Label -> Graph -> ([GraphOutput], Graph)
renameEdges oldL l g = (output, g')
                    where
                        es = findEdgesSingleLabel oldL g
                        (output, g') = foldl renEs ([],g) es
                        renEs (o, graph) e = let
                                              (o', graph') = renE e graph
                                              in (o ++ o', graph')
                        renE e graph = renameEdge oldL l e graph
 

-- | Hernoemt een enkele edge 
renameEdge :: Label -> Label -> Edge -> Graph -> ([GraphOutput], Graph)
renameEdge oldL l e@(el1, el2, c, w) g@(Graph{weighted=wei, directed = d}) | el1 == oldL && el2 == oldL = ([rem, edgeToOutput g' e1], addEdge e1 g')
                                                                           | el1 == oldL                = ([rem, edgeToOutput g' e2], addEdge e2 g')
                                                                           | el2 == oldL                = ([rem, edgeToOutput g' e3], addEdge e3 g')
                                                                    where
                                                                        (n1, n2)  = findNodesByEdge e g
                                                                        rem       = RemoveEdgeG el1 el2
                                                                        g'        = removeEdge el1 el2 g
                                                                        e1        = (l, l, c, w)
                                                                        e2        = (l, el2, c, w)
                                                                        e3        = (el1, l, c, w)                               

-- | Verandert het gewicht van de edge van node 1 naar node 2.                               
weightEdge :: Node -> Node -> Int -> Graph -> ([GraphOutput], Graph)
weightEdge n1@(l1, _, _) n2@(l2, _, _) w g | eM == Nothing = ([], g)
                                           | eM /= Nothing = ([rem, edgeToOutput g'' e'], g'')
                                          where
                                             rem          = RemoveEdgeG l1 l2
                                             eM           = findEdge l1 l2 g
                                             (_, _, c, _) = fromJust eM
                                             e'           = (l1, l2, c, w)
                                             g'           = removeEdge l1 l2 g
                                             g''          = addEdge e' g'

-- | Verwijdert de edge van node 1 naar node 2.
deleteEdge :: Node -> Node -> Graph -> ([GraphOutput], Graph)
deleteEdge (l1, _, _) (l2, _, _) g = ([RemoveEdgeG l1 l2], g')
                                    where
                                        g' = removeEdge l1 l2 g

colourNodes :: [Node] -> ColorG -> Graph -> ([GraphOutput], Graph)
colourNodes nodes col g = (output, g')
                        where
                          (output, g') = foldl colourNs ([], g) nodes
                          colourNs (o,g) n = let (o', g') = colourN n g
                                              in (o++o', g')
                          colourN n g = colourNode n col g

colourNode :: Node -> ColorG -> Graph -> ([GraphOutput], Graph)
colourNode node@(l,p,c) col g = (outN ++ outEs, g''')
                        where
                            g'           = removeNode l g
                            n'           = (l, p, col)
                            g''          = addNode n' g'
                            outN         = [RemoveNodeG l, nodeToOutput n']
                            (outEs, g''') = renameEdges l l g''

colourSubGraph :: Graph -> ([GraphOutput], Graph)
colourSubGraph g@(Graph{nodes=nodes}) =  (output,g')
            where
              sgn = allSubGraphNodes g nodes
              cls = cycle [Red, Blue, Green, Purple, Grey, Yellow, Orange, Black, White]
              nodecol = zip sgn cls
              (output, g') = foldl colourNs ([], g) nodecol
              colourNs (o,g) nc = let (o', g') = colourN nc g
                                  in (o++o', g')
              colourN (n,c) g = colourNodes n c g

findNeighbours :: Node -> Graph -> [Node]
findNeighbours node@(l,_,_) g@(Graph{directed=d}) | d==Undirected = map (\n -> if node == (fst n) then (snd n) else (fst n)) ns
                                                  | otherwise = map snd (filter (\n -> fst n == node) ns)
                  where 
                    es = findEdgesSingleLabel l g
                    ns = if es /= [] then map (\e-> findNodesByEdge e g) es else []

colourNeighbour :: Node -> Graph -> ([GraphOutput], Graph)
colourNeighbour node@(l,p,c) g = colourNodes nbs Blue g 
                        where 
                            nbs = findNeighbours node g

blackWhite :: Graph -> ([GraphOutput], Graph)
blackWhite g@(Graph{nodes=nodes}) = colourNodes nodes Black g

isComplete :: Graph -> Bool
isComplete g@(Graph{nodes=nodes}) = and $ map (\n-> nodes \\ (n:(findNeighbours n g)) ==[]) nodes

isCoherent :: Graph -> Bool
isCoherent g@(Graph{nodes=nodes}) = nodes \\ (subGraphNodes g (head nodes)) == []
       

subGraphs :: Graph -> [Graph]
subGraphs g@(Graph{nodes=nodes,directed=d,weighted=w}) = map (\(n,e) -> Graph{nodes=n,edges=e,directed=d,weighted=w} ) (zip asgn sasge)
                  where
                    asgn = allSubGraphNodes g nodes
                    asge = map (subGraphEdges g) asgn
                    sasge = map (\es -> setifyEdge es []) asge

subGraphEdges :: Graph -> [Node] -> [Edge]
subGraphEdges _ [] = []
subGraphEdges g nodes = concat $ map (\(l,p,c) -> findEdgesSingleLabel l g) nodes

allSubGraphNodes :: Graph -> [Node] -> [[Node]]
allSubGraphNodes _ [] = []
allSubGraphNodes g nodes = sgn : (allSubGraphNodes g (nodes\\sgn))
                    where 
                      sgn = trace (show $ setifyNodes (subGraphNodes g (head nodes))) $ setifyNodes (subGraphNodes g (head nodes)) 

subGraphNodes :: Graph -> Node -> [Node]
subGraphNodes g n@(l,_,_) | nbs /= [] = n : (nbs ++ concat (map (subGraphNodes g'') nbs)) 
                          | otherwise = [n]
                  where
                    g' = removeNode l g
                    g'' = removeEdgesSingleLabel l g 
                    nbs = findNeighbours n g


setifyNodes :: [Node] -> [Node]
setifyNodes [] = []
setifyNodes xs = x : map snd (filter (uncurry (FPPrac./=)) (zip sxs (tail sxs)))
      where
        sxs = sortBy sortNode xs
        x = head xs

sortNode (l1,p1,c1) (l2,p2,c2)
    | l1 > l2 = GT 
    | l1 < l2 = LT 
    | l1 == l2 = EQ


setifyEdge :: [Edge] -> [Edge] -> [Edge]
setifyEdge [] _ = []
setifyEdge (e:es) res = if elem e res then setifyEdge es res else setifyEdge es (e:res)