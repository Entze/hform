module Hform.Util (
    alphabeticalToInt,
    convergingLimit,
hasAbsoluteDuplicatesIntSet,prettyClauseList,countClauses,countLiterals) where

import           Prelude   (abs, negate, error, (-))

import           Data.Bool (Bool, otherwise)

import           Data.Char (Char, isLower, isUpper, ord)

import           Data.Eq   (Eq ((==)))

import           Data.Foldable (any, foldl)

import Data.Function ( (.) )


import           Data.Int  (Int)
import Data.IntSet (size, map, fromList, empty, union, IntSet, elems, member)
import Data.Text.Prettyprint.Doc (pretty, Doc, emptyDoc, (<>), line, space)
import Data.List (map, foldl, sortOn, length)


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



prettyClauseList :: [[Int]] -> Doc ann
prettyClauseList [l] = prettyClause l <> pretty (0 :: Int)
prettyClauseList (l:ls) = prettyClause (sortOn abs l) <> pretty (0 :: Int) <> line <> prettyClauseList ls
prettyClauseList [] = emptyDoc

prettyClause :: [Int] -> Doc ann
prettyClause = foldl pretty' emptyDoc
    where
        pretty' :: Doc ann -> Int -> Doc ann
        pretty' e i = e <> pretty i <> space


countClauses :: [[Int]] -> Int
countClauses = length

countLiterals :: [[Int]] -> Int
countLiterals = Data.IntSet.size . Data.List.foldl Data.IntSet.union Data.IntSet.empty . Data.List.map (Data.IntSet.map abs) . Data.List.map Data.IntSet.fromList
