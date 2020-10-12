module Hform.Util (
    alphabeticalToInt,
    convergingLimit,
hasAbsoluteDuplicatesIntSet) where

import           Prelude   (negate, error, (-))

import           Data.Bool (Bool, otherwise)

import           Data.Char (Char, isLower, isUpper, ord)

import           Data.Eq   (Eq ((==)))

import           Data.Foldable (any)

import Data.Function ( (.) )

import           Data.Int  (Int)
import Data.IntSet (IntSet, elems, member)


alphabeticalToInt :: Char -> Int
alphabeticalToInt c
    | isLower c = ord c - ord 'a'
    | isUpper c = ord c - ord 'A'
    | otherwise = ord c

convergingLimit :: Eq a => [a] -> a
convergingLimit (a:b:as)
    | a == b = a
    | otherwise = convergingLimit (b:as)
convergingLimit [a] = a
convergingLimit [] = error "No elements"

hasAbsoluteDuplicatesIntSet :: IntSet -> Bool
hasAbsoluteDuplicatesIntSet s = (Data.Foldable.any ((`member` s) . negate) . elems) s