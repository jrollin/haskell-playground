-- define func
doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1


-- func with no arg is called definition
conanO'Brien = "It's a-me, Conan O'Brien!"

-- list (same type)
lostNumbers = [4,8,15,16,23,42]

-- concat
addList = [1,2,3,4] ++ [9,10,11,12]

concatWords = "hello" ++ " " ++ "world"

concatLists = ['w','o'] ++ ['o','t']

-- better perf on large elements
concatWordsBis = 'A':" SMALL CAT"

-- no need to cast 5 to list
concatListBis = 5:[4,3,67]

--- 7th element
listIndice = "Steve Buscemi" !! 6


-- double les éléments de l'ensemble
formule = [x*2 | x <- [1..10]]

-- prédicat : elements plus que grand que 12 une fois doublé
formulePredicate = [x*2 | x <- [1..10], x*2 >= 12]

-- tous les nombres de 10 à 20 sauf 13, 15 19
listExcept = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]

-- boomBangs [2..15]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]


-- add elements of list 
combineList = [ x*y | x <- [2,5,10], y <- [8,10,11]]


-- combine with predicat result > 50
combineListPredicate = [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]


-- vectors tuples  (1,5)



-- probleme tous les triangles dont les côtés sont inférieurs à 10 et périmètre 24
triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

rightTrianglesB n = [ (a,b,c) | c <- [1..n], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]



-- get type 
-- :t (==)
-- :t triangles

--- declare type
circumference :: Float -> Float
circumference r = 2 * pi * r



-- factorial 
factorialA n = product [1..n]

-- with recursion
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


-- add vectors
-- addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a + snd b)

-- add vectors with explicit pattern 
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


-- extract first, seconf, third from tuple
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- better show func
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y


-- implement length
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- named pattern 
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- guards
-- bmiTell :: (RealFloat a) => a -> a -> String
-- bmiTell weight height
--     | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
--     | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
--     | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
--     | otherwise                 = "You're a whale, congratulations!"

-- guards with definitions 
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- with pattern
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname


-- case 
-- head' :: [a] -> a
-- head' [] = error "No head for empty lists!"
-- head' (x:_) = x

head' :: [a] -> a
head' xs = case xs of [] -> error "No head for emp ty lists!"
                      (x:_) -> x


-- recursion
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x


-- find elem in list (extract and check head or continue)
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs


-- sort
-- quicksort :: (Ord a) => [a] -> [a]
-- quicksort [] = []
-- quicksort (x:xs) =
--     let smallerSorted = quicksort [a | a <- xs, a <= x]
--         biggerSorted = quicksort [a | a <- xs, a > x]
--     in  smallerSorted ++ [x] ++ biggerSorted

-- same sort with fitler
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted


-- infixe fonction (partiel)
divideByTen :: (Floating a) => a -> a
divideByTen = (/10) 


-- foldl :  fonction binaire + start value + liste to fold
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- version currifiée
-- La lambda (\acc x -> acc + x) est équivalente à (+)
-- On peut omettre le xs à la fin parce que foldl (+) 0 retourne une fonction qui attend une liste.
-- sum' :: (Num a) => [a] -> a
-- sum' = foldl (+) 0
