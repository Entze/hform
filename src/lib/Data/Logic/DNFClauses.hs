module Data.Logic.DNFClauses (
    DNFClauseList,
    DNFClauses,
    clauseListFromFormula,clauseListToClauses,fromIntFormula,fromFormula) where

import           Data.Eq                         (Eq)
import           Data.Function                   ((.))
import           Data.Int                        (Int)
import           Data.IntSet                     (IntSet, fromList)
import           Data.List                       (map)
import           Data.Ord                        (Ord)
import           Data.Set                        (Set, fromList)
import           Text.Show                       (Show)

import           Prelude                         ()

import           Data.Logic.PropositionalFormula (PropositionalFormula,
                                                  renameNegationsToMinus,
                                                  renameToIntFromOrder, toDNF,
                                                  toListsOfListClausesDNF)

newtype DNFClauseList = DNFList [[Int]] deriving (Show, Eq, Ord)

newtype DNFClauses = DNF (Set IntSet) deriving (Show, Eq, Ord)

clauseListFromFormula :: Data.Logic.PropositionalFormula.PropositionalFormula Int -> DNFClauseList
clauseListFromFormula formula = DNFList (toListsOfListClausesDNF formula)

clauseListToClauses :: DNFClauseList -> DNFClauses
clauseListToClauses (DNFList lists) = DNF ((Data.Set.fromList . map Data.IntSet.fromList) lists)

fromIntFormula :: Data.Logic.PropositionalFormula.PropositionalFormula Int -> DNFClauses
fromIntFormula = clauseListToClauses . clauseListFromFormula

fromFormula :: Ord a => Data.Logic.PropositionalFormula.PropositionalFormula a -> DNFClauses
fromFormula = fromIntFormula . renameNegationsToMinus . toDNF . renameToIntFromOrder
