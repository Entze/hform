module Data.Logic.CNFClauses (
    CNFClauseList,
    CNFClauses,
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
import Text.Show (Show)
import           Data.Text.Prettyprint.Doc       ((<+>), line, Pretty(pretty), (<>))

import           Data.Logic.PropositionalFormula (PropositionalFormula,
                                                  renameNegationsToMinus,
                                                  renameToIntFromOrder, toCNF,
                                                  toListsOfListClausesCNF)

newtype CNFClauseList = CNFList [[Int]] deriving (Show, Eq, Ord)

newtype CNFClauses = CNF (Set IntSet) deriving (Show, Eq, Ord)

instance Pretty CNFClauseList where
    pretty (CNFList lists) = pretty 'p' <+> pretty "cnf" <+> (pretty . countLiterals) lists <+> (pretty . countClauses) lists <> line <> prettyClauseList lists


instance Pretty CNFClauses where
    pretty (CNF sets) = pretty 'p' <+> pretty "cnf" <+> (pretty . countLiterals) lists <+> (pretty . countClauses) lists <> line <> prettyClauseList lists
        where
            lists = (Data.List.map Data.IntSet.elems .  Data.Set.elems) sets



clauseListFromFormula :: Data.Logic.PropositionalFormula.PropositionalFormula Int -> CNFClauseList
clauseListFromFormula formula = CNFList (toListsOfListClausesCNF formula)

clauseListToClauses :: CNFClauseList -> CNFClauses
clauseListToClauses (CNFList lists) = CNF ((Data.Set.fromList . Data.List.map Data.IntSet.fromList) lists)

fromIntFormula :: Data.Logic.PropositionalFormula.PropositionalFormula Int -> CNFClauses
fromIntFormula = clauseListToClauses . clauseListFromFormula

fromFormula :: Ord a => Data.Logic.PropositionalFormula.PropositionalFormula a -> CNFClauses
fromFormula = fromIntFormula . renameNegationsToMinus . toCNF . renameToIntFromOrder

removeRedundantClauses :: CNFClauses -> CNFClauses
removeRedundantClauses (CNF set) = CNF (Data.Set.filter (not . hasAbsoluteDuplicatesIntSet) set)
