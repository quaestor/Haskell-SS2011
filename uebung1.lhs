Übung 1 -- Christoph Rauch

1. Implementieren Sie die in der Vorlesung bereits als Infixoperator (++) benutzte Funktion
   concat' :: ([a], [a]) -> [a]
   die zwei Listen aneinanderhängt.

> concat' :: ([a], [a]) -> [a]
> concat' ([], x) = x
> concat' (x, []) = x
> concat' (x:xs, y) = x : concat' (xs, y)

2. Implementieren Sie die Funktion
   merge :: ([a], [a]) -> [a]
   die zwei aufsteigend geordnete Listen zu einer geordneten verschmilzt.

> merge :: Ord a => ([a], [a]) -> [a]
> merge ([], x) = x
> merge (x, []) = x
> merge (x:xs, y:ys)
>		| x < y     = x : y : merge (xs, ys)
> 		| otherwise = y : x : merge (xs, ys)

3. Implementieren Sie eine Funkton
   filter' :: (a -> Bool) -> [a] -> [a]
   die alle Elemente aus einer Liste entfernt, die die Bedingung nicht
   erfüllen, die durch den ersten Parameter angegeben wird. Beispielsweise soll
       filter' (>0) liste
   alle nichtpositiven Elemente aus der Liste streichen.

> filter' :: (a -> Bool) -> [a] -> [a]
> filter' _ [] = []
> filter' f (x:xs)
>		| f x == True = x : filter' f xs
>		| otherwise   = filter' f xs

4. Implementieren Sie eine Funktion
   remove_mults :: [a] -> [a]
   die aus einer Liste eventuelle Wiederholungen von Elementen streicht. Geben
   Sie zwei Lösungen an: eine ohne die Filter-Funktion, eine unter deren
   Verwendung.

  Ohne filter:

> remove_mults :: Eq a => [a] -> [a]
> remove_mults xs = rm xs []
>	where	rm [] _ = []
>		rm (x:xs) ys
>			| x `elem` ys = rm xs ys
>			| otherwise   = x : rm xs (x:ys)


  Mit filter:

> remove_mults' :: Eq a => [a] -> [a]
> remove_mults' (x:xs) = x : remove_mults' (filter (/=x) xs)
> remove_mults' _ = []

5. Implementieren Sie die Funktion bin(n,m) zur Berechnung der Binomialkoeffizienten: 
    (a) Verwenden Sie Rekursion auf dem Pascalschen Dreieck.

> pas :: [Integer] -> [[Integer]]
> pas x = x:pas (zipWith (+) ([0] ++ x) (x ++ [0]))
>
> pascal :: [[Integer]]
> pascal = pas [1]
>
> bin :: Int -> Int -> Integer
> bin k n = pascal !! n !! k

    (b) Machen Sie die Lösung effizienter, indem Sie beachten, dass Sie eine neue Zeile des 
	Dreiecks berechnen können, wenn Sie die vorherige kennen. 

> bin' :: Integer -> Integer -> Integer
> bin' 0 _ = 1
> bin' _ 0 = 1
> bin' k n
>	| k /= n = (bin' (k-1) (n-1)) + (bin' k (n-1))
>	| otherwise = 1

    (c) Kommen Sie zu einer Lösung mit linearem Laufzeitbedarf, indem Sie das Dreieck
        gar nicht benutzen!

> bin'' :: Integer -> Integer -> Integer
> bin'' 0 _ = 1
> bin'' _ 0 = 1
> bin'' k n = ((n - k + 1) * (bin'' (k - 1) n)) `div` k
