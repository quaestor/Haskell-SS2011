Übung 3 -- Christoph Rauch

1. Schreiben Sie für den Datentyp Nat' die Funktionen mult' zur Multiplikation
   und power zur Potenzierung

> data Nat' = Zero' | Succ' Nat' deriving Show
>
> add' Zero' y     = y
> add' (Succ' x) y = Succ' $ add' x y
>
> mult' Zero' _     = Zero'
> mult' (Succ' x) y = add' y $ mult' x y
>
> power _ Zero'     = Succ' Zero'
> power (Succ' x) y = mult' y $ power x y

2. Schreiben Sie ein Programm, das die Höhe eines binären Baumes ausrechnet.

> data StandBinTree a = EmptyBinTree | Node(a, StandBinTree a,
>                                              StandBinTree a)
>
> height :: StandBinTree a -> Int
> height EmptyBinTree     = 0
> height (Node (_, a, b)) = 1 + max (height a) (height b)

3. Schreiben Sie ein Programm, das jeden Knoten eines Baumes jeweils in eine
   Zeile für sich schreibt und die Unterknoten um drei Anschläge gegenüber dem
   übergeordneten einrückt.

> showTree :: StandBinTree a -> String
> showTree n = showTree' 0 n
>    where showTree' i EmptyBinTree = "( )"
>          showTree' i (Node (v, l, r)) =
>            "\n" ++ replicate i ' ' -- Whitespace
>            ++ "(" ++ show v ++ ")" -- Knoten ausgeben
>            ++ showTree' (i + 3) l -- linken Teilbaum rekursiv darstellen
>            ++ showTree' (i + 3) r -- rechten Teilbaum rekursiv darstellen

   oder mit Knotenverbindungen:

> instance (Show a) => Show (StandBinTree a)
>     where
>     show t = shows' id t ""
>
> shows' :: (Show a) => (String -> String) -> StandBinTree a -> (String -> String)
> shows' _  EmptyBinTree     = ("-( )\n"++)
> shows' ss (Node (p, l, r)) =
>     let s'  = ("-("++) . shows p . (")+"++) -- Knoten ausgeben
>         ss' = ss . (tail (spaces s') ++)    -- Whitespace
>     in s'  . ("\n"++)
>        . ss' . ("|"++) . shows' (ss' . ("|"++)) l -- linker Teilbaum
>        . ss' . ("`"++) . shows' (ss' . (" "++)) r -- rechter Teilbaum
>     where spaces f = map (const ' ') (f "")

4. Man kann die data-Konstruktion zur Definition von Vereinigungstypen
   verwenden:

> data ListElement = IntElem Integer | CharElem Char deriving (Show)

    (a) Wie muss eine konkrete Liste, die sowohl Integer- als auch
        Char-Elemente enthält, hingeschrieben werden?

> concreteList :: [ListElement]
> concreteList = [IntElem 42, CharElem 'q']

    (b) Schreiben Sie Funktionen, die die Teilliste der Integer-Elemente bzw.
        die Teilliste der Char-Elemente heraussuchen.

> intElems :: [ListElement] -> [Integer]
> intElems [] = []
> intElems ((IntElem x):xs) = x : intElems xs
> intElems (_:xs) = intElems xs

> charElems :: [ListElement] -> [Char]
> charElems [] = []
> charElems ((CharElem x):xs) = x : charElems xs
> charElems (_:xs) = charElems xs

    (c) Welcher (unerwartete) Effekt entsteht beim Ausdrucken der
        Char-Teilliste?

        Die Liste wird als String ausgegeben.

5. Folgendes Programm ordnet eine Sequenz von Zeichenketten rechtsbündig an:

> ralign :: [[Char]] -> [[Char]]
> ralign xs = [rjustify (m,x) | x <- xs]
>     where m = maximum [length x | x <- xs]

   Es wird die maximale Länge eines Strings in der Liste bestimmt und für jeden
   der Strings daraufhin die Funktion 'rjustify' aufgerufen.

> rjustify :: (Int, [Char]) -> [Char]
> rjustify (n,s) = spaces (n - length s) ++ s

   'n - length s' ist die Anzahl der voranzustellenden Leerzeichen. Diese
   werden mit 's' konkateniert. Einen String von 'n' Leerzeichen generiert
   folgende Funktion:

> spaces :: Int -> [Char]
> spaces n = take n (repeat ' ')

6. Die zweite Lösung der gleichen Aufgabe ist etwas trickreicher. Sie bestimmt
   nicht vorab die maximale Zeichenkettenlänge:

> ralign' xs = ys
>    where (ys,m) = pass xs
>          pass [] = ([],0)
>          pass (q:qs) = (rjustify (m,q) : zs, max (length q) n)
>             where (zs,n) = pass qs

   Die Funktion 'pass' steigt zunächst rekursiv von der anfänglichen Liste von
   Strings bis zur leeren Liste ab, wobei immer genau ein String aus der Liste
   pro rekursivem Aufruf (hier 'q') weggenommen wird. Ist die leere Liste
   erreicht, wird zum ersten Mal die zweite Hälfte des Rückgabewertes mit einem
   Wert (nämlich '0') belegt. In jeder Rekursionsstufe wird dieser Wert dann
   mit der Länge von 'q' verglichen und das Maximum an die nächsthöhere
   Rekursionsstufe übergeben. Zuletzt führt dies zur Belegung von 'm' mit dem
   Maximum der Stringlängen, woraufhin alle Variablen wertbehaftet sind und in
   jeder Rekursionsstufe 'rustify (m, q)' ausgeführt werden kann.

7. Implementieren Sie das Unifikationsverfahren von Robinson!
    (a) Definieren Sie zunächst einen Datentyp Term und darauf die Funktionen
        substitute und occurs.

> data Term = VarT String | FuncT String [Term] deriving Eq
> instance Show Term where
>     show (VarT s)     = s
>     show (FuncT s xs) = s ++ show xs

Terme sind also Variablen VarT oder Funktionen FuncT, jeweils mit einem Namen,
der als String übergeben wird.

> substitute :: Term -> Term -> Term -> Term
> substitute (VarT x) t (VarT f) | x == f    = t
>                                | otherwise = (VarT f)
> substitute x t (FuncT f fs) = FuncT f $ map (substitute x t) fs

Die Funktion substitute ersetzt alle Vorkommen der Variablen 'x' im Term mit
der Bezeichnung 'f' durch den Term 't'.

> occurs :: Term -> Term -> Bool
> occurs (VarT x) (FuncT f fs) = or $ map (occurs (VarT x)) fs
> occurs (VarT x) (VarT y) = x == y
> occurs _ _ = False

Mit dieser Funktion wird überprüft, ob eine Variable 'x' im Inneren der
Funktion 'f' vorkommt.

> differencePair :: Term -> Term -> Maybe (Term, Term)
> differencePair (VarT x) (VarT y)
>     | x == y    = Nothing
>     | otherwise = Just (VarT x, VarT y)
> differencePair (FuncT f fs) (VarT x) = Just (VarT x, FuncT f fs)
> differencePair (VarT x) (FuncT f fs) = Just (VarT x, FuncT f fs)
> differencePair t1@(FuncT f fs) t2@(FuncT g gs)
>     | t1 == t2  = Nothing
>     | f == g    = head $ filter (/= Nothing) $ zipWith differencePair fs gs
>     | otherwise = error "unification impossible (different symbols at root)"

Mit der Funktion 'differencePair' wird das erste Unterscheidungspaar von zwei
Termen bestimmt. Dieses wird als Paar (Variable, Term) zurückgegeben, gekapselt
im Maybe-Monad, d.h. falls keine Unterscheidunsgpaare existieren, so ist der
Rückgabewert 'Nothing'. Die Funktion liefert außerdem einen Fehler, falls zwei
Terme mit unterschiedlichen Symbolen an der Wurzel verglichen werden. Dies ist
einer der beiden Überprüfungsschritte, ob die Terme unifizierbar sind.

> unify :: Term -> Term -> Term
> unify t1 t2 =
>     case differencePair t1 t2 of
>         Nothing      -> t1
>         Just (x, t)  -> if occurs x t
>             then error "unification impossible (occurs check)"
>             else unify (substitute x t t1) (substitute x t t2)

Schließlich führt 'unify' die Unifikation nach Robinson-Algorithmus durch. Es
wird ein Unterscheidungspaar gesucht und der Occurs-Check durchgeführt (zweiter
Überprüfungsschritt). Daraufhin wird auf beiden Input-Termen die Substitution
durchgeführt und auf die damit entstandenen Terme wiederum 'unify' rekursiv
angewendet, bis kein Unterscheidungspaar mehr gefunden wird.
