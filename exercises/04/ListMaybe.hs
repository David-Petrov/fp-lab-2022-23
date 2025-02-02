-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

module ListMaybe where

import Prelude hiding (all, and, concat, drop, filter, length, map, null, product, reverse, subtract, sum, take, zip, zipWith, (++))

-- TODO:
-- ask about 08.12
-- -> maybe, poll

-- g :: ((Int -> Int) -> Int) -> Bool
-- g f = even (f (\x -> x * 10))
--
-- y :: Bool
-- y = g (\h -> h 20)

-- g f = ... f <> ...
--
-- Пешо вика g и дава f
-- Гошо имплементира g и вика f
--
--
--
-- g :: Int -> (Bool -> (String -> Char))
-- g :: (Int -> Int) -> Bool -> String -> Char

--
--
--
-- f :: (Int -> Int) -> Int -> Bool
-- Int
--
-- (Int -> Int)
--
-- h :: ((Int -> Int) -> Int) -> Int -> Bool

-- game/server+client semantics for HOF
-- quad x = double $ double x

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- compose :: (b -> c) -> (a -> b) -> a -> c
-- compose f g x = f (g x)

double :: Int -> Int
double x = x * x

-- 3  -> 5 -> 6 -> nil
-- 4 ----^
--
-- [3,5,6]
-- [4,5,6]

-- data List a
--  = Nil -- []
--  | Cons a (List a) -- (:)

-- sumList :: [Integer] -> Integer
-- sumList [] = 0
-- sumList (x : xs) = x + sumList xs

-- dog dogs
-- x   xs

-- sumList ((:) x xs) = x + sumList xs

-- ($), (.) (example quad?)
--
-- lists:
--   * constructor name
--   * list range
--   * list comprehension
--   * String
--      * utf8 -> Text
--      * byte array -> ByteString
--   * (optional) "nondeterminism effect"

-- matrix :: (....)
-- f -> (...)

-- maybe:
--   * like pointers kind of
--   * used for error handling and propagation
--   * (optional) "failure effect"

-- data Maybe a
--  = Nothing -- null
--  | Just a -- ...

-- addMaybes ::
--  Maybe Integer ->
--  Maybe Integer ->
--  Maybe Integer
-- addMaybes (Just x) (Just y) = Just (x + y)
-- addMaybes _ _ = Nothing

safeDiv ::
  Integer ->
  Integer ->
  Maybe Integer
safeDiv x 0 = Nothing
safeDiv x y = Just (x `div` y)

-- safeDiv

-- null

-- sum
-- x, xs

-- headMaybe

-- headMaybe :: [a] -> Maybe a
-- headMaybe [] = Nothing
-- headMaybe (x : _) = Just x

-- cartesianProd with list comprehension?

-- Maybe as "computation that can fail" Lists as "one of many"

-- EXERCISE
-- Generate all the numbers in the ("mathematical range") [n, m] in a list (inclusive).
-- EXAMPLES
-- >>> listFromRange 3 12
-- [3,4,5,6,7,8,9,10,11,12]
-- >>> listFromRange 8 6
-- []
listFromRange :: Integer -> Integer -> [Integer]
listFromRange a b
  | a > b = []
  | otherwise = a : listFromRange (a + 1) b

-- EXERCISE
-- Multiply all the elements of a list
-- EXAMPLES
-- >>> product [2,4,8]
-- 64
-- >>> product []
-- 1
product :: [Integer] -> Integer
product [] = 1
product (x : xs) = x * product xs

-- EXERCISE
-- Implement factorial with prod and listFromRange
fact :: Integer -> Integer
fact = product . listFromRange 1

-- EXERCISE
-- Return a list of the numbers that divide the given number.
-- EXAMPLES
-- >>> divisors 5
-- [1,5]
-- >>> divisors 64
-- [1,2,4,8,16,32,64]
-- >>> divisors 24
-- [1,2,3,4,6,8,12,24]
divisors :: Integer -> [Integer]
divisors n = [x | x <- listFromRange 1 n, n `mod` x == 0]

-- EXERCISE
-- Implement prime number checking using listFromRange and divisors
-- EXAMPLES
-- >>> isPrime 7
-- True
-- >>> isPrime 8
-- False
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n 
  | n < 0 = isPrime (- n)
  | otherwise = [1] == takeWhile (<= floor (sqrt $ fromIntegral n)) (divisors n)

-- EXERCISE
-- Get the last element in a list.
-- EXAMPLES
-- >>> lastMaybe []
-- Nothing
-- >>> lastMaybe [1,2,3]
-- Just 3
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [x] = Just x
lastMaybe (_ : xs) = lastMaybe xs

-- EXERCISE
-- Calculate the length of a list.
-- EXAMPLES
-- >>> length [1,2,8]
-- 3
-- >>> length []
-- 0
length :: [a] -> Integer
length [] = 0
length (x : xs) = 1 + length xs

-- EXERCISE
-- Return the nth element from a list (we count from 0).
-- If n >= length xs, return a Nothing
-- EXAMPLES
-- >>> ix 2 [1,42,69]
-- Just 69
-- >>> ix 3 [1,42,69]
-- Nothing
ix :: Integer -> [a] -> Maybe a
ix n l@(x : xs)
  | abs n >= length l = Nothing
  | n < 0             = ix (n + length l) l
  | n == 0            = Just x
  | otherwise         = ix (n - 1) xs

-- EXERCISE
-- "Drop" the first n elements of a list.
-- If n > length xs, then you should drop them all.
-- EXAMPLES
-- >>> drop 5 $ listFromRange 1 10
-- [6,7,8,9,10]
-- >>> drop 20 $ listFromRange 1 10
-- []
drop :: Integer -> [a] -> [a]
drop _ [] = []
drop n l@(_ : xs)
  | n <= 0 = l
  | otherwise = drop (n - 1) xs


-- EXERCISE
-- "Take" the first n elements of a list.
-- If n > length xs, then you should take as many as you can.
-- EXAMPLES
-- >>> take 5 $ listFromRange 1 10
-- [1,2,3,4,5]
-- >>> take 20 $ listFromRange 1 10
-- [1,2,3,4,5,6,7,8,9,10]
take :: Integer -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n l@(x : xs)
  | n < 0 = drop (n + length l) l
  | otherwise = x : take (n - 1) xs

-- EXERCISE
-- Append one list to another. append [1,2,3] [4,5,6] == [1,2,3,4,5,6]
-- This is called (++) in the base library.
-- HINT: Of course, you can think in the classic "inductive" way - I've got the result - what do I need to do at this step.
-- Or alternatively, you can think about "placing the second list at the end of the first":
-- 0. how do you get to the end of the first one?
-- 1. what do you need to do at each step to "remember" the elements of the first one?
-- EXAMPLES
-- >>> append [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]
-- >>> append [] [4,5,6]
-- [4,5,6]
append :: [a] -> [a] -> [a]
append [] l2 = l2
append (x1 : xs1) l2 = x1 : append xs1 l2

-- EXERCISE
-- Concatenate all the lists together.
-- EXAMPLES
-- >>> concat [[1,2,3], [42,69], [5,7,8,9]]
-- [1,2,3,42,69,5,7,8,9]
-- >>> concat [[1,2,3], [], [5,7,8,9]]
-- [1,2,3,5,7,8,9]
-- >>> concat []
-- []
concat :: [[a]] -> [a]
concat [] = []
concat (l : ls) = append l (concat ls)

-- EXERCISE
-- Reverse a list. It's fine to do this however you like.
-- EXAMPLES
-- >>> reverse [1,2,3]
-- [3,2,1]
-- >>> reverse []
-- []
reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = append (reverse xs) [x]

-- EXERCISE
-- Square all the numbers in a list
-- EXAMPLES
-- >>> squareList [1,2,3,5]
-- [1,4,9,25]
squareList :: [Integer] -> [Integer]
squareList [] = []
squareList (x : xs) = x * x : (squareList xs)

-- EXERCISE
-- Pair up the given element with each of the elements a list.
-- EXAMPLES
-- >>> megaPair 42 [69,7,42]
-- [(42,69),(42,7),(42,42)]
megaPair :: a -> [b] -> [(a, b)]
megaPair _ [] = []
megaPair p (x : xs) = (p, x) : megaPair p xs

-- EXERCISE
-- Both of those functions above have the same structure - apply a function to each element of a list.
-- We can abstract this and get one of the most useful functions over lists (and containers in general).
-- EXAMPLES
-- >>> map succ [1,2,3]
-- [2,3,4]
-- >>> map (\x -> x * x) [1,2,3] -- same as squareList
-- [1,4,9]
-- >>> map (\x -> (3,x)) [1,2,3] -- same as megaPair 3
-- [(3,1),(3,2),(3,3)]
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

-- EXERCISE
-- Check if all the elements in a list are True.
-- EXAMPLES
-- >>> and []
-- True
-- >>> and [False]
-- False
-- >>> and [True, True]
-- True
and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && and xs

-- EXERCISE
-- Check if all the elements of a list satisfy a predicate
-- Implement this using map and and.
-- EXAMPLES
-- >>> all isPrime [2,3,7]
-- True
-- >>> all isPrime [1,2,3,7]
-- False
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all pred (x : xs) = pred x && all pred xs

-- EXERCISE
-- Implement the cartesian product of two lists.
-- Don't use a list comprehension!
-- >>> cartesian [1,2,3] [4,5,6]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- >>> cartesian [] [4,5,6]
-- []
-- >>> cartesian [1,2,3] []
-- []
cartesian :: [a] -> [b] -> [(a, b)]
cartesian [] _ = []
cartesian (x1 : xs1) l2 =
  append (map (x1, ) l2) (cartesian xs1 l2)

-- EXERCISE
-- We can generalise cartesian to work with arbitrary functions instead of just (,),
-- taking elements "each with each"
-- This is also the generalisation of cartesian, as seen in the examples.
-- EXAMPLES
-- >>> lift2List (+) [1] [2]
-- [3]
-- >>> lift2List (+) [1,2] [2]
-- [3,4]
-- >>> lift2List (*) [1,2,3] [1,2]
-- [1,2,2,4,3,6]
-- >>> lift2List (,) [1,2,3] [4,5,6] -- same as cartesian [1,2,3] [4,5,6]
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
lift2List :: (a -> b -> c) -> [a] -> [b] -> [c]
lift2List _ [] _ = []
lift2List f (x : xs) l = append (map (f x) l) (lift2List f xs l)

-- EXERCISE
-- The "filtering" part of a list comprehension - leave only those elements, that satisfy the given predicate.
-- EXAMPLES
-- >>> even 2
-- True
-- >>> even 3
-- False
-- >>> filter even [1..10]
-- [2,4,6,8,10]
-- >>> filter isPrime [1..20]
-- [2,3,5,7,11,13,17,19]
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs) = 
  (if p x then (x : ) else id) $ filter p xs


-- EXERCISE
-- Parse a character into a digit.
-- EXAMPLES
-- >>> parseDigit '6'
-- Just 6
-- >>> parseDigit '9'
-- Just 9
-- >>> parseDigit 'c'
-- Nothing
parseDigit :: Char -> Maybe Integer
parseDigit c
  | c `elem` ['0' .. '9'] = Just $ read $ pure c
  | otherwise = Nothing

-- EXERCISE
-- See if all the values in a list xs are Just, returning Just xs only if they are.
-- We can think of this as all the computations in a list "succeeding",
-- and therefore the entire "computation list" has "succeeded.
-- Note that it is vacuously that all the elements in the empty list are Just.
-- EXAMPLES
-- >>> validateList []
-- Just []
-- >>> validateList [Just 42, Just 6, Just 9]
-- Just [42,6,9]
-- >>> validateList [Nothing, Just 6, Just 9]
-- Nothing
-- >>> validateList [Just 42, Nothing, Just 9]
-- Nothing
-- >>> validateList [Just 42, Just 6, Nothing]
-- Nothing
-- validateList :: [Maybe a] -> Maybe [a]
-- validateList l 
--   | all isJust l = Just $ map fromJust l
--   | otherwise = Nothing
--     where
--       fromJust :: Maybe a -> a
--       fromJust (Just x) = x
--       fromJust Nothing = error "Should never happen, you have validated!"

--       isJust :: Maybe a -> Bool
--       isJust (Just _) = True
--       isJust _ = False

-- Alternatively (totally not stolen from the standard lib)

validateList :: [Maybe a] -> Maybe [a]
validateList = traverse' id
  where
    traverse' f = foldr cons_f (pure [])
      where cons_f x = liftA2 (:) (f x)
    liftA2 f x = (<*>) (fmap f x)

-- EXERCISE
-- You often have a collection (list) of things, for each of which you want to
-- perform some computation, that might fail (returning Maybe).
-- Let's implement a function to do exactly this -
-- execute a "failing computation" for all the items in a list,
-- immediately "aborting" upon a failure.
-- Think about how to reuse validateList.
-- This is called traverseListMaybe, because it's a special case of a generic function called traverse
-- that performs "actions" for each element of a "collection", specialised to List and Maybe
-- EXAMPLES
-- >>> traverseListMaybe (\x -> if even x then Just x else Nothing) [2,4,6]
-- Just [2,4,6]
-- >>> traverseListMaybe (\x -> if even x then Just x else Nothing) [1,2,3]
-- Nothing
-- >>> traverseListMaybe (5 `safeDiv`) [0,2]
-- Nothing
-- >>> traverseListMaybe (8 `safeDiv`) [3,2]
-- Just [2,4]
traverseListMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseListMaybe f = foldr cons_f (pure [])
  where 
    cons_f x = liftA2 (:) (f x)
    liftA2 g x = (<*>) $ fmap g x

-- EXERCISE
-- Convert a list of digits to a number. Assume that the input list only has Integers
-- in the range [0,9]. Let's assume the empty list converts to 0.
-- HINT: It might be easier to first reverse the list and then operate on it with a helper.
-- EXAMPLES
-- >>> digitsToNumber [6,9]
-- 69
-- >>> digitsToNumber [1,2,0]
-- 120
-- >>> digitsToNumber [0,1,2,0]
-- 120
digitsToNumber :: [Integer] -> Integer
digitsToNumber = foldl (\acc x -> 10 * acc + x) 0

-- EXERCISE
-- Combine the previous functions to parse a number.
-- EXAMPLES
-- >>> parseNumber "0"
-- Just 0
-- >>> parseNumber "3"
-- Just 3
-- >>> parseNumber "69"
-- Just 69
-- >>> parseNumber "0123"
-- Just 123
-- >>> parseNumber "blabla"
-- Nothing
-- >>> parseNumber "133t"
-- Nothing
parseNumber :: String -> Maybe Integer
parseNumber = fmap digitsToNumber . traverseListMaybe parseDigit

-- EXERCISE
-- Notice how in parseNumber, in the Nothing case we returned Nothing,
-- and in the Just case, we returned Just again, with a "non-maybe" function inside.
-- This turns out to be very useful, and if you compare it to the map for lists, it's almost the same.
-- Let's write it now, so we don't have to do that pattern match again in the future.
-- Afterwards, you can reuse this function in parseNumber.
-- EXAMPLES
-- >>> maybeMap succ $ Just 5
-- Just 6
-- >>> maybeMap succ Nothing
-- Nothing
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just $ f x

-- EXERCISE
-- Another way to combine lists
-- Instead of "taking all possible combinations" we group the lists "pointwise"
-- If one list is shorter than the other, you can stop there.
-- EXAMPLES
-- >>> zip [1,2,3] [4,5,6]
-- [(1,4),(2,5),(3,6)]
-- >>> zip [1,2] []
-- []
-- >>> zip [1] [4,5,6]
-- [(1,4)]
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

-- EXERCISE
-- And the generalised version of zip.
-- EXAMPLES
-- >>> zipWith (,) [1,2,3] [4,5]
-- [(1,4),(2,5)]
-- >>> zipWith (+) [1,2,3] [4,5,6]
-- [5,7,9]
-- >>> zipWith (:) [1,2,3] [[4],[5,7],[]]
-- [[1,4],[2,5,7],[3]]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f as bs = map (uncurry f) $ zip as bs

-- EXERCISE
-- Transpose a matrix. Assume all the inner lists have the same length.
-- HINT: zipWith and map might be useful here.
-- EXAMPLES
-- >>> transpose [[1]]
-- [[1]]
-- >>> transpose [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]
-- >>> transpose [[1],[2]]
-- [[1,2]]
-- >>> transpose [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]

-- -- Variant one
-- transpose :: [[a]] -> [[a]]
-- transpose ([] : _) = []
-- transpose ls = map head ls : transpose (map tail ls)

-- Variant two
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose [lastRow] = map (: []) lastRow
transpose (xs : xss) = zipWith (:) xs $ transpose xss
-- pire
-- EXERCISE
-- Reverse a list, but in linear time (so if the input list has n elements, you should only be doing at most ~n operations, not n^2)
-- You will need a helper local definition.
-- EXAMPLES
-- >>> reverse [1,2,3]
-- [3,2,1]
-- >>> reverse []
-- []
reverseLinear :: [a] -> [a]
reverseLinear = foldl' (flip (:)) []
  where
    foldl' :: (b -> a -> b) -> b -> [a] -> b
    foldl' _ nv [] = nv
    foldl' op nv (x : xs) = foldl' op (op nv x) xs