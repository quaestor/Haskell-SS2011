1. Ergänzen Sie die in der Vorlesung bei der Definition der Typklasse Tree
   weggelassenen Default-Implementierungen für height und preOrder.

> import Data.List
>
> class Tree h where
>    isEmptyTree    :: h a -> Bool
>    root           :: h a -> a
>    height         :: h a -> Integer 
>    weight         :: h a -> Integer
>    successors     :: h a -> [h a]
>    preOrder       :: h a -> [a]
>    showTree       :: (Show a) => h a -> String
>
>    weight t = if isEmptyTree t then 0
>               else 1 + sum (map weight (successors t))
>
>    height t = if isEmptyTree t then 0
>               else 1 + maximum (map height (successors t))
>
>    showTree t = if isEmptyTree t then "-"
>                 else if length (successors t) == 0
>                      then "(" ++ show (root t) ++ ")"
>                 else "(" ++ show (root t) ++ " "
>                          ++ sh (successors t) ++ ")"
>                 where sh [] = ""
>                       sh (hd:[]) = showTree hd
>                       sh (hd:tl) = showTree hd ++ " "
>                                    ++ sh tl
>
>    preOrder t = if isEmptyTree t then []
>                 else root t : concatMap preOrder (successors t)

2. Implementieren Sie eine Alternative zur Standardimplementierung der binären
   Bäume:

> data AltBinTree a = EmptyAltBinTree
>                   | Leaf a
>                   | InnerNode (a, AltBinTree a, AltBinTree a)
>
> class (Tree h) => BinTree h where
>    consLeaf   :: a -> h a
>    consTree   :: (a, h a, h a) -> h a 
>    left       :: h a -> h a
>    right      :: h a -> h a
>    inOrder    :: h a -> [a]
>
>    inOrder t = if isEmptyTree t then []
>                else inOrder (left t) ++ [root t] ++ inOrder (right t)
>
>    left t  = (successors t)!!0 -- Fehlermeldung fehlt!
>    right t = (successors t)!!1
>
> instance Tree AltBinTree where
>
>    isEmptyTree EmptyAltBinTree = True
>    isEmptyTree _ = False
>
>    root EmptyAltBinTree = error "root of empty tree"
>    root (Leaf r) = r
>    root (InnerNode (r, _, _)) = r
>
>    preOrder EmptyAltBinTree = []
>    preOrder (Leaf l) = [l]
>    preOrder (InnerNode (root, left, right))
>        = root : preOrder left ++ preOrder right
>
>    successors EmptyAltBinTree = []
>    successors (Leaf _) = []
>    successors (InnerNode(_, l, r)) = [l, r]
>
>    showTree EmptyAltBinTree = "()"
>    showTree (Leaf root) = "(" ++ show root ++ ")"
>    showTree (InnerNode (root, l, r)) = "(" ++ show root
>                                     ++ " " ++ show(l)
>                                     ++ " " ++ show(r) ++ ")" 
>
> instance BinTree AltBinTree where
>    consLeaf = Leaf
>    consTree = InnerNode
>
>    left (InnerNode (_, l, _)) = l
>    left EmptyAltBinTree = error "left of empty tree"
>    left (Leaf _) = error "left of leaf"
>
>    right (InnerNode (_, _, r)) = r
>    right EmptyAltBinTree = error "right of empty tree"
>    right (Leaf _) = error "right of leaf"
>
> instance (Show a) => Show (AltBinTree a) where
>    show = showTree

3. Man kann beliebige Bäume mit einem Trick als binäre Bäume implementieren.
   Dabei werden die Nachfolger eines Knotens als verkettete Liste dargestellt
   (⊥ bezeichnet die leere Liste). Jeden Knoten verlassen zwei Zeiger:
   firstSucc, der auf den ersten Nachfolger zeigt, und neighbour, der den
   rechten Nachbar bestimmt. Betrachten Sie die Klasse
     ArbTree: class (Tree h) => ArbTree h
   und machen Sie StandBinTree zu einer Instanz dieser Klasse!

> data StandBinTree a = EmptyBinTree 
>                     | Node (a, StandBinTree a, StandBinTree a)
>   deriving (Show, Eq)
>
> class (Tree h) => ArbTree h where
>   firstSucc :: h a -> h a
>   neighbour :: h a -> h a
>
>   showArbTree :: (Show a) => h a -> String
>   showArbTree' :: (Show a) => [h a] -> String
>
> instance Tree StandBinTree where
>    isEmptyTree(EmptyBinTree) = True
>    isEmptyTree(Node(_,_,_)) = False
>
>    root(EmptyBinTree) = error "root of empty tree"
>    root(Node(r, _, _)) = r
>
>    successors(EmptyBinTree) = []
>    successors(Node(_, l, r)) = [l, r]
>
> instance ArbTree StandBinTree where
>   firstSucc EmptyBinTree = EmptyBinTree
>   firstSucc (Node (root, l, r)) = l
>
>   neighbour EmptyBinTree = EmptyBinTree
>   neighbour (Node (root, l, r)) = r
>
>   showArbTree a = showArbTree' [a]
>   showArbTree' [] = ""
>   showArbTree' fs = (concat $ intersperse " " $ 
>                     map (\x -> if isEmptyTree x then (\y -> "|") x else show $ root x) neighbours) ++ "\n" ++
>                     (showArbTree' firstsuccs)
>      where neighbours = concat $ intersperse [EmptyBinTree] [takeWhile (\x -> not $ isEmptyTree x) (iterate neighbour n) | n <- fs]
>            firstsuccs = filter (\x -> not $ isEmptyTree x) (map firstSucc neighbours)
