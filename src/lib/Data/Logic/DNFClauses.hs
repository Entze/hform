module Data.Logic.DNFClauses (
    DNFClauseList,
    DNFClauses,
    clauseListFromFormula,clauseListToClauses,fromIntFormula,fromFormula,
    removeRedundantClauses) where

import Hform.Util (hasAbsoluteDuplicatesIntSet)

import Prelude ()
import Data.Bool (not)
import           Data.Function                   ((.))
import           Data.Eq (Eq)
import           Data.Int                        (Int)
import           Data.IntSet                     (foldl, IntSet, fromList)
import           Data.List                       (foldl, map)
import           Data.Ord                        (Ord)
import           Data.Set                        (filter, Set, fromList,foldl)
import           Text.Show                       (Show)
import           Data.Text.Prettyprint.Doc       (Doc, line, space, Pretty(pretty), emptyDoc, (<>))

import           Data.Logic.PropositionalFormula (PropositionalFormula,
                                                  renameNegationsToMinus,
                                                  renameToIntFromOrder, toDNF,
                                                  toListsOfListClausesDNF)

newtype DNFClauseList = DNFList [[Int]] deriving (Show, Eq, Ord)

newtype DNFClauses = DNF (Set IntSet) deriving (Show, Eq, Ord)

instance Pretty DNFClauseList where
    pretty (DNFList lists) = Data.List.foldl pretty' emptyDoc lists
        where
            pretty' e list = e <> Data.List.foldl pretty'' emptyDoc list <> pretty (0 :: Int) <> line
            pretty'' e i = e <> pretty i <> space

instance Pretty DNFClauses where
    pretty (DNF sets) = Data.Set.foldl pretty' emptyDoc sets
        where
            pretty' :: Doc ann -> IntSet -> Doc ann
            pretty' e set = e <> Data.IntSet.foldl pretty'' emptyDoc set <> pretty (0 :: Int) <> line
            pretty'' :: Doc ann -> Int -> Doc ann
            pretty'' e i = e <> pretty i <> space

clauseListFromFormula :: Data.Logic.PropositionalFormula.PropositionalFormula Int -> DNFClauseList
clauseListFromFormula formula = DNFList (toListsOfListClausesDNF formula)

clauseListToClauses :: DNFClauseList -> DNFClauses
clauseListToClauses (DNFList lists) = DNF ((Data.Set.fromList . Data.List.map Data.IntSet.fromList) lists)

fromIntFormula :: Data.Logic.PropositionalFormula.PropositionalFormula Int -> DNFClauses
fromIntFormula = clauseListToClauses . clauseListFromFormula

fromFormula :: Ord a => Data.Logic.PropositionalFormula.PropositionalFormula a -> DNFClauses
fromFormula = fromIntFormula . renameNegationsToMinus . toDNF . renameToIntFromOrder

removeRedundantClauses :: DNFClauses -> DNFClauses
removeRedundantClauses (DNF set) = DNF (Data.Set.filter (not . hasAbsoluteDuplicatesIntSet) set)



{-
removeRedundantLiterals :: DNFClauses -> DNFClauses
removeRedundantLiterals (DNF set) = DNF (Data.Set.map removeAbsoluteDuplicates set)
    where
        removeAbsoluteDuplicates :: IntSet -> IntSet
        removeAbsoluteDuplicates set = set \\ absoluteDuplicates
            where
                absoluteDuplicates = Data.IntSet.filter ((`Data.IntSet.member` set) . negate) set


removeRedundantClauses :: DNFClauses -> DNFClauses
removeRedundantClauses (DNF set) = DNF ()

-}