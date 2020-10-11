module Hform.Util (
    alphabeticalToInt,
    convergingLimit,
) where

import           Prelude   (error, (-))

import           Data.Bool (otherwise)

import           Data.Char (Char, isLower, isUpper, ord)

import           Data.Eq   (Eq ((==)))

import           Data.Int  (Int)

alphabeticalToInt :: Char -> Int
alphabeticalToInt c
    | isLower c = (ord c) - (ord 'a')
    | isUpper c = (ord c) - (ord 'A')
    | otherwise = ord c

convergingLimit :: Eq a => [a] -> a
convergingLimit (a:b:as)
    | a == b = a
    | otherwise = convergingLimit (b:as)
convergingLimit [a] = a
convergingLimit [] = error "No elements"

