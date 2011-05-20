Übung 2 -- Christoph Rauch

1. Schreiben Sie zwei Curry-Funktionen, die eine beliebige Funktion y = f(x)
    (a) an der x-Achse,
    (b) an der y-Achse
   spiegeln:

> mirrorx :: (Float -> Float) -> (Float -> Float)
> mirrorx f = \x -> - (f x)

> mirrory :: (Float -> Float) -> (Float -> Float)
> mirrory f = \x -> f (- x)

2. Schreiben Sie zwei Curry-Funktionen, die eine beliebige Funktion y = f(x) um
   einen gegebenen Beitrag in x-Richtung verschieben bzw. dehnen:

> shift :: Float -> (Float -> Float) -> (Float -> Float)
> shift t f = \x -> f (x - t)

> stretch :: Float -> (Float -> Float) -> (Float -> Float)
> stretch s f = \x -> f (x / s)

3. Schreiben Sie zwei Funktionen, die allgemein den Übergang zwischen
   Curry-Form und Tupelparametern erlauben:

> curry' :: ((a, b) -> c) -> (a -> b -> c)
> curry' f = \x y -> f (x, y)

> uncurry' :: (a -> b -> c) -> ((a, b) -> c)
> uncurry' f = \(x, y) -> f x y

4. Definieren Sie einen Infix-Operator `iter`, der die n-fache iterierte
   Anwendung einer Funktion in der Form f `iter` n erlaubt. Beispielsweise wäre
   inc `iter` 3 die Funktion, die um 3 erhöht, oder sqrt `iter` 2 die vierte
   Wurzel.

> iter :: Integral b => (a -> a) -> b -> (a -> a)
> f `iter` n | n < 0     = error "Can't apply function a negative amount of times"
>            | n == 0    = id
>            | otherwise = f . (f `iter` (n-1))

5. Schreiben Sie eine Funktion zur Bestimmung des Skalarproduktes zweier
   (numerischer) Vektoren
     x1*y1 + x2*y2 + x3*y3 + ... + xn*yn
   unter Verwendung der Funktion foldr.

> sprod :: Num a => [a] -> [a] -> a
> xs `sprod` ys | length xs == length ys = foldr (+) 0 $ zipWith (*) xs ys
>               | otherwise              = error "Vector sizes differ"

