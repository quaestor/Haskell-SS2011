1. Ergänzen Sie die in der Vorlesung bei der Deﬁnition der Typklasse Tree
   weggelassenen Default-Implementierungen für height und preOrder.

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
>
>
> data StandBinTree a = EmptyBinTree 
>                          | Node(a, StandBinTree a, StandBinTree a)
>
> instance (Show a) => Show (StandBinTree a) where
>    show(EmptyBinTree) = "()"
>    show(Node(root, EmptyBinTree, EmptyBinTree))
>                       = "(" ++ show root ++ ")"
>    show(Node(root, l, r)) = "(" ++ show root
>                                 ++ " " ++ show(l)
>                                 ++ " " ++ show(r) ++ ")"
>
> instance Tree StandBinTree where
>
>    isEmptyTree(EmptyBinTree) = True
>    isEmptyTree(Node(_,_,_)) = False
>
>    root(EmptyBinTree) = error "root of empty tree"
>    root(Node(r, _, _)) = r
>
>    successors(EmptyBinTree) = []
>    successors(Node(_, l, r)) = [l, r]
