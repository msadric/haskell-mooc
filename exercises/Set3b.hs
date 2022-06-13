-- Exercise set 3b
--
-- This is a special exercise set. The exercises are about
-- implementing list functions using recursion and pattern matching,
-- without using any standard library functions. For this reason,
-- you'll be working in a limited environment where almost none of the
-- standard library is available.
--
-- At least the following standard library functions are missing:
--  * martinAppend
--  * head
--  * tail
--  * map
--  * filter
--  * concat
--  * (!!)
--
-- The (:) operator is available, as is list literal syntax [a,b,c].
--
-- Feel free to use if-then-else, guards, and ordering functions (< and > etc.).
--
-- The tests will check that you haven't added imports :)

{-# LANGUAGE NoImplicitPrelude #-}

module Set3b where

import Mooc.LimitedPrelude
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: given numbers start, count and end, build a list that starts
-- with count copies of start and ends with end.
--
-- Use recursion and the : operator to build the list.
--
-- Examples:
--   buildList 1 5 2 ==> [1,1,1,1,1,2]
--   buildList 7 0 3 ==> [3]

buildList :: Int -> Int -> Int -> [Int]
buildList start 0 end = [end]
buildList start count end = start : buildList start (count-1) end

------------------------------------------------------------------------------
-- Ex 2: given i, build the list of sums [1, 1+2, 1+2+3, .., 1+2+..+i]
--
-- Use recursion and the : operator to build the list.
--
-- Ps. you'll probably need a recursive helper function

sums :: Int -> [Int]
sums 1 = [1]
sums i = martinAppend (sums (i-1)) [sums_ i]


martinAppend :: [Int] -> [Int] -> [Int]
martinAppend [] ls = ls
martinAppend (x:xs) ls = x : (martinAppend xs ls)


sums_ :: Int -> Int
sums_ 1 = 1
sums_ i = i + sums_ (i-1)


------------------------------------------------------------------------------
-- Ex 3: define a function mylast that returns the last value of the
-- given list. For an empty list, a provided default value is
-- returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   mylast 0 [] ==> 0
--   mylast 0 [1,2,3] ==> 3

mylast :: a -> [a] -> a
mylast def [] = def
mylast def [x] = x
mylast def (x:xs) = mylast def xs

------------------------------------------------------------------------------
-- Ex 4: safe list indexing. Define a function indexDefault so that
--   indexDefault xs i def
-- gets the element at index i in the list xs. If i is not a valid
-- index, def is returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   indexDefault [True] 1 False         ==>  False
--   indexDefault [10,20,30] 0 7         ==>  10
--   indexDefault [10,20,30] 2 7         ==>  30
--   indexDefault [10,20,30] 3 7         ==>  7
--   indexDefault ["a","b","c"] (-1) "d" ==> "d"

indexDefault :: [a] -> Int -> a -> a
indexDefault ls i def
        | i > martinLength ls = def
        | i < 0 = def
        | otherwise = indexDefault_ ls i def


indexDefault_ ::  [a] -> Int -> a -> a
indexDefault_  [] i def = def
indexDefault_ (x:xs) 0 def = x
indexDefault_ (x:xs) i def = indexDefault_ xs (i-1) def

martinLength :: [a] -> Int
martinLength [] = 0
martinLength (x:xs) = 1 + martinLength xs



------------------------------------------------------------------------------
-- Ex 5: define a function that checks if the given list is in
-- increasing order.
--
-- Use pattern matching and recursion to iterate through the list.
--
-- Examples:
--   sorted [1,2,3] ==> True
--   sorted []      ==> True
--   sorted [2,7,7] ==> True
--   sorted [1,3,2] ==> False
--   sorted [7,2,7] ==> False

sorted :: [Int] -> Bool
sorted [] = True
sorted ([x]) = True
sorted (x:(y:ys)) = (x<=y) && (sorted (y:ys))


------------------------------------------------------------------------------
-- Ex 6: compute the partial sums of the given list like this:
--
--   sumsOf [a,b,c]  ==>  [a,a+b,a+b+c]
--   sumsOf [a,b]    ==>  [a,a+b]
--   sumsOf []       ==>  []
--
-- Use pattern matching and recursion (and the list constructors : and [])

sumsOf :: [Int] -> [Int]
sumsOf [] = []
sumsOf (x:xs) = x : (addition xs x)


addition :: [Int] -> Int -> [Int]
addition [] x = []
addition (y:ys) x = y+x : addition ys (y+x)



------------------------------------------------------------------------------
-- Ex 7: implement the function merge that merges two sorted lists of
-- Ints into a sorted list
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   merge [1,3,5] [2,4,6] ==> [1,2,3,4,5,6]
--   merge [1,1,6] [1,2]   ==> [1,1,1,2,6]

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
       | (x >= y) = y : merge (x:xs) ys
        | otherwise = x : merge xs (y:ys)


------------------------------------------------------------------------------
-- Ex 8: define the function mymaximum that takes a list and a
-- function bigger :: a -> a -> Bool and returns the
-- biggest of the list, according to the comparing function.
--
-- An initial biggest value is provided to give you something to
-- return for empty lists.
--
-- Examples:
--   mymaximum (>) 3 [] ==> 3
--   mymaximum (>) 0 [1,3,2] ==> 3
--   mymaximum (>) 4 [1,3,2] ==> 4    -- initial value was biggest
--   mymaximum (<) 4 [1,3,2] ==> 1    -- note changed biggerThan
--   mymaximum (\xs ys -> length xs > lenth ys) [] [[1,2],[3]]
--     ==> [1,2]

mymaximum :: (a -> a -> Bool) -> a -> [a] -> a
mymaximum bigger initial [] = initial
mymaximum bigger initial [x]
        | bigger initial x = initial
        | otherwise = x
mymaximum bigger initial (x:[y])
        | bigger x y = mymaximum bigger initial [x]
        | otherwise = mymaximum bigger initial [y]
mymaximum bigger initial (x:y:ys)
        | bigger x y = mymaximum bigger initial (x:ys)
        | otherwise = mymaximum bigger initial (y:ys)



------------------------------------------------------------------------------
-- Ex 9: define a version of map that takes a two-argument function
-- and two lists. Example:
--
--   map2 f [x,y,z,w] [a,b,c]  ==> [f x a, f y b, f z c]
--
-- If the lists have differing lengths, ignore the trailing elements
-- of the longer list.
--
-- Use recursion and pattern matching. Do not use any library functions.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f [] [] = []
map2 f as [] = []
map2 f [] bs = []
map2 f (a:as) [b] = [f a b]
map2 f [a] (b:bs) = [f a b]
map2 f (a:as) (b:bs) = f a b : map2 f (as) (bs)

------------------------------------------------------------------------------
-- Ex 10: implement the function maybeMap, which works a bit like a
-- combined map & filter.
---
-- maybeMap is given a list ([a]) and a function of type a -> Maybe b.
-- This function is called for all values in the list. If the function
-- returns Just x, x will be in the result list. If the function
-- returns Nothing, no value gets added to the result list.
--
-- Examples:
--
-- let f x = if x>0 then Just (2*x) else Nothing
-- in maybeMap f [0,1,-1,4,-2,2]
--   ==> [2,8,4]
--
-- maybeMap Just [1,2,3]
--   ==> [1,2,3]
--
-- maybeMap (\x -> Nothing) [1,2,3]
--   ==> []

maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap f [] = []
maybeMap f (x:xs) =
         case f x of 
                Just x -> x : maybeMap f xs
                Nothing -> maybeMap f xs








