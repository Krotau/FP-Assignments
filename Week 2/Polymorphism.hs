module Polymorphism where

-- f1a :: Int -> Int
-- f1b :: a -> a
-- f1c :: (Int, Int) -> Int
-- f1d :: (a, a) -> a
-- f1e :: (a, b) -> a

-- How many total functions are there of type (Int -> Int)? By contrast,
-- how many total functions are there of type (a -> a)?

-- f2a :: (a, a) -> (a, a)
-- f2b :: (a, b) -> (b, a)
-- f2c :: (a -> b) -> a -> b
-- f2d :: (a, x) -> a
-- f2e :: (x -> a -> b, a, x) -> b
-- f2f :: (a -> b, x -> a, x) -> b
-- f2g :: (x -> a -> b, x -> a, x) -> b

-- f3a :: Int -> (Int -> Int)
-- f3b :: (Int -> Int) -> Int
-- f3c :: a -> (a -> a)
-- f3d :: (a -> a) -> a
