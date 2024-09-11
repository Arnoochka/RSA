module Lib
    ( 
      gcd,
      extendedGCD,
      findCoprime,
      modExp
    ) where

import Data.Char
import Prelude hiding (gcd)


-- Вспомогательные функции

-- алгоритм Евклида
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

-- Расширенный алгоритм Евклида
extendedGCD :: Int -> Int -> (Int, Int, Int)
extendedGCD a 0 = (a, 1, 0)
extendedGCD a b =
    let (gcd, x1, y1) = extendedGCD b (a `mod` b)
        y = x1 - (a `div` b) * y1
    in (gcd, y1, y) 

{- Функция для нахождения открытого ключа
(Нахождение числа взаимно простого с n)-}

findCoprime :: Int -> Int
findCoprime n = head [x | x <- [n - 2, n - 3 .. 3], gcd n x == 1]

-- алгоритм быстрого возведения по модулю в степень
toBin :: Int -> [Int] -> [Int]
toBin 0 xs = xs
toBin n xs = toBin (n`div` 2) ((n `mod` 2):xs) 

fastPow :: Int -> Int -> Int -> [Int] -> Int
fastPow m a n [x] = m^2 * a^x `mod` n
fastPow m a n (x:xs) = fastPow (m^2 * a^x `mod` n) a n xs 

modExp :: Int -> Int -> Int -> Int
modExp base exp modulus = fastPow (base `mod` modulus) base modulus xs
    where
        (x:xs) = toBin exp []
