module Data.Logic.DNFClauses (
    DNFClauseList,
    DNFClauses,
    clauseListFromFormula,clauseListToClauses,fromIntFormula,fromFormula,
    removeRedundantClauses) where

import Hform.Util (countLiterals, countClauses, hasAbsoluteDuplicatesIntSet, prettyClauseList)

import Prelude ()
import Data.Bool (not)
import           Data.Function                   ((.))
import           Data.Eq (Eq)
import           Data.Int                        (Int)
import           Data.IntSet                     (elems, IntSet, fromList)
import           Data.List                       (map)
import           Data.Ord                        (Ord)
import           Data.Set                        (elems, filter, Set, fromList)
import           Text.Show                       (Show)
import           Data.Text.Prettyprint.Doc       ((<+>), line, Pretty(pretty), (<>))

import           Data.Logic.PropositionalFormula (PropositionalFormula,
                                                  renameNegationsToMinus,
                                                  renameToIntFromOrder, toDNF,
                                                  toListsOfListClausesDNF)

newtype DNFClauseList = DNFList [[Int]] deriving (Show, Eq, Ord)

newtype DNFClauses = DNF (Set IntSet) deriving (Show, Eq, Ord)

instance Pretty DNFClauseList where
    pretty (DNFList lists) = pretty 'p' <+> pretty "dnf" <+> (pretty . countLiterals) lists <+> (pretty . countClauses) lists <> line <> prettyClauseList lists


instance Pretty DNFClauses where
    pretty (DNF sets) = pretty 'p' <+> pretty "dnf" <+> (pretty . countLiterals) lists <+> (pretty . countClauses) lists <> line <> prettyClauseList lists
        where
            lists = (Data.List.map Data.IntSet.elems .  Data.Set.elems) sets



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
