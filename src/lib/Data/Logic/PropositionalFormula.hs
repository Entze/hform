module Data.Logic.PropositionalFormula (
    PropositionalFormula,
    false,
    true,
    var,
    not,
    and,
    or,
    implies,
    equivalent,
    nand,
    nor,
    without,
    xor,
    converse,
    from,
    immediateSubformulas,
    rewriteTopDown,
    rewriteBottomUp,
    simplify,
    toNNF,
    toDNF,
    toCNF,
    mapAtoms,
    prettyWithTextInfix
 ) where

import Prelude (Bool (True, False), (||), Ord, 
    Int,
    Show,
    Eq,
    String,
    head,
    tail,
    iterate,
    otherwise,
    (&&),
    (==),
    (>),
    (.),
    )

import Data.List (elemIndex)

import Data.Maybe ( Maybe(Just, Nothing) )

import Data.Text.Prettyprint.Doc (
    (<>),
    Doc,
    Pretty(pretty)
    )

import Hform.Util (
    convergingLimit
    )
import Data.Tuple (fst)
import Data.List (length)
import Data.List ((++))

data PropositionalFormula a = 
    Contradiction | Tautology |
    Variable a | 
    Negation (PropositionalFormula a) |
    Conjunction (PropositionalFormula a) (PropositionalFormula a) |
    Disjunction (PropositionalFormula a) (PropositionalFormula a) |
    MaterialImplication (PropositionalFormula a) (PropositionalFormula a) |
    Biconditional (PropositionalFormula a) (PropositionalFormula a) |
    AlternativeDenial (PropositionalFormula a) (PropositionalFormula a) |
    JointDenial (PropositionalFormula a) (PropositionalFormula a) |
    MaterialNonImplication (PropositionalFormula a) (PropositionalFormula a) |
    ExclusiveDisjunction (PropositionalFormula a) (PropositionalFormula a) |
    ConverseImplication (PropositionalFormula a) (PropositionalFormula a) |
    ConverseNonImplication (PropositionalFormula a) (PropositionalFormula a)
    deriving (Eq, Show)

instance (Pretty a) => Pretty (PropositionalFormula a) where
    pretty = (prettyWithSetInfix canonicalSymbolSet canonicalPrecedence)

prettyWithTextInfix :: Pretty a => PropositionalFormula a -> Doc ann
prettyWithTextInfix = (prettyWithSetInfix canonicalTextSet canonicalPrecedence)

false :: PropositionalFormula a
false = Contradiction

true :: PropositionalFormula a
true = Tautology

var :: a -> PropositionalFormula a
var a = Variable a   

not :: PropositionalFormula a -> PropositionalFormula a
not f = Negation f

and :: PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
and f1 f2 = Conjunction f1 f2

or :: PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
or f1 f2 = Disjunction f1 f2

implies :: PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
implies f1 f2 = MaterialImplication f1 f2

equivalent :: PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
equivalent f1 f2 = Biconditional f1 f2

nand :: PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
nand f1 f2 = AlternativeDenial f1 f2

nor :: PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
nor f1 f2 = JointDenial f1 f2

without :: PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
without f1 f2 = MaterialNonImplication f1 f2

xor :: PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
xor f1 f2 = ExclusiveDisjunction f1 f2

converse :: PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
converse f1 f2 = ConverseImplication f1 f2

from :: PropositionalFormula a -> PropositionalFormula a -> PropositionalFormula a
from f1 f2 = ConverseNonImplication f1 f2

canonicalPrecedence :: PropositionalFormula a -> Int
canonicalPrecedence Contradiction = 0
canonicalPrecedence Tautology = 0
canonicalPrecedence (Variable _) = 0
canonicalPrecedence (Negation _) = 1
canonicalPrecedence (Conjunction _ _) = 2
canonicalPrecedence (Disjunction _ _) = 2
canonicalPrecedence (MaterialImplication _ _) = 3
canonicalPrecedence (Biconditional _ _) = 4
canonicalPrecedence (AlternativeDenial _ _) = 5
canonicalPrecedence (JointDenial _ _) = 5
canonicalPrecedence (MaterialNonImplication _ _) = 6
canonicalPrecedence (ExclusiveDisjunction _ _) = 4
canonicalPrecedence (ConverseImplication _ _) = 7
canonicalPrecedence (ConverseNonImplication _ _) = 7

isAssociative :: PropositionalFormula a -> Bool
isAssociative Contradiction = False
isAssociative Tautology = False
isAssociative (Variable _) = False
isAssociative (Negation _) = False
isAssociative (Conjunction _ _) = True
isAssociative (Disjunction _ _) = True
isAssociative (MaterialImplication _ _) = False
isAssociative (Biconditional _ _) = True
isAssociative (AlternativeDenial _ _) = False
isAssociative (JointDenial _ _) = False
isAssociative (MaterialNonImplication _ _) = False
isAssociative (ExclusiveDisjunction _ _) = True
isAssociative (ConverseImplication _ _) = False
isAssociative (ConverseNonImplication _ _) = False

operands :: PropositionalFormula a -> Int
operands Contradiction = 0
operands Tautology = 0
operands (Variable _) = 0
operands (Negation _) = 1
operands _ = 2

operator :: PropositionalFormula a -> PropositionalFormula Int
operator (Negation _) = (Negation (Variable 0))
operator (Conjunction _ _) = (Conjunction (Variable 0) (Variable 1))
operator (Disjunction _ _) = (Disjunction (Variable 0) (Variable 1))
operator (MaterialImplication _ _) = (MaterialImplication (Variable 0) (Variable 1))
operator (Biconditional _ _) = (Biconditional (Variable 0) (Variable 1))
operator (AlternativeDenial _ _) = (AlternativeDenial (Variable 0) (Variable 1))
operator (JointDenial _ _) = (JointDenial (Variable 0) (Variable 1))
operator (MaterialNonImplication _ _) = (MaterialNonImplication (Variable 0) (Variable 1))
operator (ExclusiveDisjunction _ _) = (ExclusiveDisjunction (Variable 0) (Variable 1))
operator (ConverseImplication _ _) = (ConverseImplication (Variable 0) (Variable 1))
operator (ConverseNonImplication _ _) = (ConverseNonImplication (Variable 0) (Variable 1))
operator (Variable _) = (Variable 0)

canonicalSymbolSet :: PropositionalFormula a -> String
canonicalSymbolSet Contradiction = "⊥"
canonicalSymbolSet Tautology = "⊤"
canonicalSymbolSet (Negation _) = "¬"
canonicalSymbolSet (Conjunction _ _) = "∧"
canonicalSymbolSet (Disjunction _ _) = "∨"
canonicalSymbolSet (MaterialImplication _ _) = "→"
canonicalSymbolSet (Biconditional _ _) = "↔"
canonicalSymbolSet (AlternativeDenial _ _) = "↑"
canonicalSymbolSet (JointDenial _ _) = "↓"
canonicalSymbolSet (MaterialNonImplication _ _) = "↛"
canonicalSymbolSet (ExclusiveDisjunction _ _) = "⨁"
canonicalSymbolSet (ConverseImplication _ _) = "←"
canonicalSymbolSet (ConverseNonImplication _ _) = "↚"
canonicalSymbolSet _ = []

canonicalTextSet :: PropositionalFormula a -> String
canonicalTextSet Contradiction = "false"
canonicalTextSet Tautology = "true"
canonicalTextSet (Negation _) = "NOT"
canonicalTextSet (Conjunction _ _) = "AND"
canonicalTextSet (Disjunction _ _) = "OR"
canonicalTextSet (MaterialImplication _ _) = "IMPLY"
canonicalTextSet (Biconditional _ _) = "IFF"
canonicalTextSet (AlternativeDenial _ _) = "NAND"
canonicalTextSet (JointDenial _ _) = "NOR"
canonicalTextSet (MaterialNonImplication _ _) = "NIMPLY"
canonicalTextSet (ExclusiveDisjunction _ _) = "XOR"
canonicalTextSet (ConverseImplication _ _) = "IF"
canonicalTextSet (ConverseNonImplication _ _) = "NIF"
canonicalTextSet _ = []


immediateSubformulas :: PropositionalFormula a -> [PropositionalFormula a]
immediateSubformulas (Negation subformula) = [subformula]
immediateSubformulas (Conjunction s1 s2) = [s1,s2]
immediateSubformulas (Disjunction s1 s2) = [s1,s2]
immediateSubformulas (MaterialImplication s1 s2) = [s1,s2]
immediateSubformulas (Biconditional s1 s2) =  [s1,s2]
immediateSubformulas (AlternativeDenial s1 s2) = [s1,s2]
immediateSubformulas (JointDenial s1 s2) = [s1,s2]
immediateSubformulas (MaterialNonImplication s1 s2) = [s1,s2]
immediateSubformulas (ExclusiveDisjunction s1 s2) = [s1,s2]
immediateSubformulas (ConverseImplication s1 s2) = [s1,s2]
immediateSubformulas (ConverseNonImplication s1 s2) = [s1,s2]
immediateSubformulas _ = []


rewriteTopDown :: Eq a => (PropositionalFormula a -> PropositionalFormula a) -> PropositionalFormula a -> PropositionalFormula a
rewriteTopDown rule = ((subrewrite (rewriteTopDown rule)) . convergingLimit . (iterate rule))

rewriteBottomUp :: Eq a => (PropositionalFormula a -> PropositionalFormula a) -> PropositionalFormula a -> PropositionalFormula a
rewriteBottomUp rule = (convergingLimit . (iterate rule) . (subrewrite (rewriteBottomUp rule)))

subrewrite :: (PropositionalFormula a -> PropositionalFormula a) -> PropositionalFormula a -> PropositionalFormula a
subrewrite rewrite (Negation f) = (Negation (rewrite f))
subrewrite rewrite (Conjunction f1 f2) = (Conjunction (rewrite f1) (rewrite f2))
subrewrite rewrite (Disjunction f1 f2) = (Disjunction (rewrite f1) (rewrite f2))
subrewrite rewrite (MaterialImplication f1 f2) = (MaterialImplication (rewrite f1) (rewrite f2))
subrewrite rewrite (Biconditional f1 f2) = (Biconditional (rewrite f1) (rewrite f2))
subrewrite rewrite (AlternativeDenial f1 f2) = (AlternativeDenial (rewrite f1) (rewrite f2))
subrewrite rewrite (JointDenial f1 f2) = (JointDenial (rewrite f1) (rewrite f2))
subrewrite rewrite (MaterialNonImplication f1 f2) = (MaterialNonImplication (rewrite f1) (rewrite f2))
subrewrite rewrite (ExclusiveDisjunction f1 f2) = (ExclusiveDisjunction (rewrite f1) (rewrite f2))
subrewrite rewrite (ConverseImplication f1 f2) = (ConverseImplication (rewrite f1) (rewrite f2))
subrewrite rewrite (ConverseNonImplication f1 f2) = (ConverseNonImplication (rewrite f1) (rewrite f2))
subrewrite _ f = f

simplify :: Eq a => PropositionalFormula a -> PropositionalFormula a
simplify = rewriteBottomUp simplifyRewrite

simplifyRewrite :: Eq a => PropositionalFormula a -> PropositionalFormula a
simplifyRewrite f@(Negation _)  = simplifyRewriteNegation f
simplifyRewrite f@(Conjunction _ _) = simplifyRewriteConjunction f
simplifyRewrite f@(Disjunction _ _) = simplifyRewriteDisjunction f
simplifyRewrite f@(MaterialImplication _ _) = simplifyRewriteMaterialImplication f
simplifyRewrite f@(Biconditional _ _) = simplifyRewriteBiconditional f
simplifyRewrite f@(AlternativeDenial _ _) = simplifyRewriteAlternativeDenial f
simplifyRewrite f@(JointDenial _ _) = simplifyRewriteJointDenial f
simplifyRewrite f@(MaterialNonImplication _ _) = simplifyRewriteMaterialNonImplication f
simplifyRewrite f@(ExclusiveDisjunction _ _) = simplifyRewriteExclusiveDisjunction f
simplifyRewrite f@(ConverseImplication _ _) = simplifyRewriteConverseImplication f
simplifyRewrite f@(ConverseNonImplication _ _) = simplifyRewriteConverseNonImplication f
simplifyRewrite f = f


simplifyRewriteNegation :: PropositionalFormula a -> PropositionalFormula a
simplifyRewriteNegation (Negation (Negation f)) = f
simplifyRewriteNegation (Negation (Contradiction)) = Tautology
simplifyRewriteNegation (Negation (Tautology)) = Contradiction
simplifyRewriteNegation (Negation (Conjunction f1 f2)) = AlternativeDenial f1 f2
simplifyRewriteNegation (Negation (Disjunction f1 f2)) = JointDenial f1 f2
simplifyRewriteNegation (Negation (MaterialImplication f1 f2)) = MaterialNonImplication f1 f2
simplifyRewriteNegation (Negation (Biconditional f1 f2)) = ExclusiveDisjunction f1 f2
simplifyRewriteNegation (Negation (AlternativeDenial f1 f2)) = Conjunction f1 f2
simplifyRewriteNegation (Negation (JointDenial f1 f2)) = Disjunction f1 f2
simplifyRewriteNegation (Negation (MaterialNonImplication f1 f2)) = MaterialImplication f1 f2
simplifyRewriteNegation (Negation (ExclusiveDisjunction f1 f2)) = Biconditional f1 f2
simplifyRewriteNegation (Negation (ConverseImplication f1 f2)) = ConverseNonImplication f1 f2
simplifyRewriteNegation (Negation (ConverseNonImplication f1 f2)) = ConverseImplication f1 f2
simplifyRewriteNegation f = f

simplifyRewriteConjunction :: Eq a => PropositionalFormula a -> PropositionalFormula a
simplifyRewriteConjunction (Conjunction f1 Tautology) = f1
simplifyRewriteConjunction (Conjunction Tautology f2) = f2
simplifyRewriteConjunction (Conjunction _ Contradiction) = Contradiction
simplifyRewriteConjunction (Conjunction Contradiction _) = Contradiction
simplifyRewriteConjunction (Conjunction f1 (Negation f2))
    | f1 == f2 = Contradiction
simplifyRewriteConjunction (Conjunction (Negation f1) f2)
    | f1 == f2 = Contradiction
simplifyRewriteConjunction (Conjunction f1 (Disjunction f2 f3))
    | f1 == f2 || f1 == f3 = f1
simplifyRewriteConjunction (Conjunction (Disjunction f1 f2) f3)
    | f3 == f2 || f3 == f1 = f3
simplifyRewriteConjunction f@(Conjunction f1 f2)
    | f1 == f2 = f1
    | otherwise = f


simplifyRewriteDisjunction :: Eq a => PropositionalFormula a -> PropositionalFormula a
simplifyRewriteDisjunction (Disjunction _ Tautology) = Tautology
simplifyRewriteDisjunction (Disjunction Tautology _) = Tautology
simplifyRewriteDisjunction (Disjunction f1 Contradiction) = f1
simplifyRewriteDisjunction (Disjunction Contradiction f2) = f2
simplifyRewriteDisjunction (Disjunction f1 (Negation f2))
    | f1 == f2 = Tautology
simplifyRewriteDisjunction (Disjunction (Negation f1) f2)
    | f1 == f2 = Tautology
simplifyRewriteDisjunction (Disjunction f1 (Conjunction f2 f3))
    | f1 == f2 || f1 == f3 = f1
simplifyRewriteDisjunction (Disjunction (Conjunction f1 f2) f3)
    | f3 == f2 || f3 == f1 = f3
simplifyRewriteDisjunction f@(Disjunction f1 f2)
    | f1 == f2 = f1
    | otherwise = f

simplifyRewriteMaterialImplication :: Eq a => PropositionalFormula a -> PropositionalFormula a
simplifyRewriteMaterialImplication (MaterialImplication _ Tautology) = Tautology
simplifyRewriteMaterialImplication (MaterialImplication Tautology f2) = f2
simplifyRewriteMaterialImplication (MaterialImplication f1 Contradiction) = f1
simplifyRewriteMaterialImplication (MaterialImplication Contradiction _) = Tautology
simplifyRewriteMaterialImplication (MaterialImplication f1 (Negation f2))
    | f1 == f2 = Negation f1
simplifyRewriteMaterialImplication (MaterialImplication (Negation f1) f2)
    | f1 == f2 = f1
simplifyRewriteMaterialImplication f@(MaterialImplication f1 f2)
    | f1 == f2 = Tautology
    | otherwise = f

simplifyRewriteBiconditional :: Eq a => PropositionalFormula a -> PropositionalFormula a
simplifyRewriteBiconditional (Biconditional f1 Tautology) = f1
simplifyRewriteBiconditional (Biconditional Tautology f2) = f2
simplifyRewriteBiconditional (Biconditional f1 Contradiction) = Negation f1
simplifyRewriteBiconditional (Biconditional Contradiction f2) = Negation f2
simplifyRewriteBiconditional (Biconditional f1 (Negation f2))
    | f1 == f2 = Contradiction
simplifyRewriteBiconditional (Biconditional (Negation f1) f2)
    | f1 == f2 = Contradiction
simplifyRewriteBiconditional f@(Biconditional f1 f2)
    | f1 == f2 = Tautology
    | otherwise = f

simplifyRewriteAlternativeDenial :: Eq a => PropositionalFormula a -> PropositionalFormula a
simplifyRewriteAlternativeDenial (AlternativeDenial f1 Tautology) = f1
simplifyRewriteAlternativeDenial (AlternativeDenial Tautology f2) = f2
simplifyRewriteAlternativeDenial (AlternativeDenial _ Contradiction) = Tautology
simplifyRewriteAlternativeDenial (AlternativeDenial Contradiction _) = Tautology
simplifyRewriteAlternativeDenial (AlternativeDenial f1 (Negation f2))
    | f1 == f2 = Tautology
simplifyRewriteAlternativeDenial (AlternativeDenial (Negation f1) f2)
    | f1 == f2 = Tautology
simplifyRewriteAlternativeDenial f@(AlternativeDenial f1 f2)
    | f1 == f2 = Negation f1
    | otherwise = f

simplifyRewriteJointDenial :: Eq a => PropositionalFormula a -> PropositionalFormula a
simplifyRewriteJointDenial (JointDenial _ Tautology) = Contradiction
simplifyRewriteJointDenial (JointDenial Tautology _) = Contradiction
simplifyRewriteJointDenial (JointDenial f1 Contradiction) = Negation f1
simplifyRewriteJointDenial (JointDenial Contradiction f2) = Negation f2
simplifyRewriteJointDenial (JointDenial f1 (Negation f2))
    | f1 == f2 = Contradiction
simplifyRewriteJointDenial (JointDenial (Negation f1) f2)
    | f1 == f2 = Contradiction
simplifyRewriteJointDenial f@(JointDenial f1 f2)
    | f1 == f2 = Negation f1
    | otherwise = f

simplifyRewriteMaterialNonImplication :: Eq a => PropositionalFormula a -> PropositionalFormula a
simplifyRewriteMaterialNonImplication (MaterialNonImplication _ Tautology) = Contradiction
simplifyRewriteMaterialNonImplication (MaterialNonImplication Tautology f2) = Negation f2
simplifyRewriteMaterialNonImplication (MaterialNonImplication Contradiction _) = Contradiction
simplifyRewriteMaterialNonImplication (MaterialNonImplication f1 Contradiction) = f1
simplifyRewriteMaterialNonImplication (MaterialNonImplication f1 (Negation f2))
    | f1 == f2 = f1
simplifyRewriteMaterialNonImplication (MaterialNonImplication (Negation f1) f2)
    | f1 == f2 = Negation f1
simplifyRewriteMaterialNonImplication f@(MaterialNonImplication f1 f2)
    | f1 == f2 = Contradiction
    | otherwise = f

simplifyRewriteExclusiveDisjunction :: Eq a => PropositionalFormula a -> PropositionalFormula a
simplifyRewriteExclusiveDisjunction (ExclusiveDisjunction f1 Tautology) = Negation f1
simplifyRewriteExclusiveDisjunction (ExclusiveDisjunction Tautology f2) = Negation f2
simplifyRewriteExclusiveDisjunction (ExclusiveDisjunction f1 Contradiction) = f1
simplifyRewriteExclusiveDisjunction (ExclusiveDisjunction Contradiction f2) = f2
simplifyRewriteExclusiveDisjunction (ExclusiveDisjunction f1 (Negation f2))
    | f1 == f2 = Tautology
simplifyRewriteExclusiveDisjunction (ExclusiveDisjunction (Negation f1) f2)
    | f1 == f2 = Tautology
simplifyRewriteExclusiveDisjunction f@(ExclusiveDisjunction f1 f2)
    | f1 == f2 = Contradiction 
    | otherwise = f

simplifyRewriteConverseImplication :: Eq a => PropositionalFormula a -> PropositionalFormula a
simplifyRewriteConverseImplication (ConverseImplication f1 Tautology) = f1
simplifyRewriteConverseImplication (ConverseImplication Tautology _) = Tautology
simplifyRewriteConverseImplication (ConverseImplication _ Contradiction) = Tautology
simplifyRewriteConverseImplication (ConverseImplication Contradiction f2) = Negation f2
simplifyRewriteConverseImplication (ConverseImplication f1 (Negation f2))
    | f1 == f2 = f1
simplifyRewriteConverseImplication (ConverseImplication (Negation f1) f2)
    | f1 == f2 = Negation f1
simplifyRewriteConverseImplication f@(ConverseImplication f1 f2)
    | f1 == f2 = Tautology
    | otherwise = f

simplifyRewriteConverseNonImplication :: Eq a => PropositionalFormula a -> PropositionalFormula a
simplifyRewriteConverseNonImplication (ConverseNonImplication f1 Tautology) = Negation f1
simplifyRewriteConverseNonImplication (ConverseNonImplication Tautology _) = Contradiction
simplifyRewriteConverseNonImplication (ConverseNonImplication _ Contradiction) = Contradiction
simplifyRewriteConverseNonImplication (ConverseNonImplication Contradiction f2) = f2
simplifyRewriteConverseNonImplication (ConverseNonImplication f1 (Negation f2))
    | f1 == f2 = Negation f1
simplifyRewriteConverseNonImplication (ConverseNonImplication (Negation f1) f2)
    | f1 == f2 = f1
simplifyRewriteConverseNonImplication f@(ConverseNonImplication f1 f2)
    | f1 == f2 = Contradiction
    | otherwise = f

rewriteAtomToNNF :: PropositionalFormula a -> PropositionalFormula a
rewriteAtomToNNF (Negation (Negation f)) = f
rewriteAtomToNNF (Negation (Conjunction f1 f2)) = (Disjunction (Negation f1) (Negation f2))
rewriteAtomToNNF (Negation (Disjunction f1 f2)) = (Conjunction (Negation f1) (Negation f2))
rewriteAtomToNNF (MaterialImplication f1 f2) = (Disjunction (Negation f1) f2)
rewriteAtomToNNF (Biconditional f1 f2) = (Conjunction (MaterialImplication f1 f2) (ConverseImplication f1 f2))
rewriteAtomToNNF (AlternativeDenial f1 f2) = (Negation (Conjunction f1 f2))
rewriteAtomToNNF (JointDenial f1 f2) = (Negation (Disjunction f1 f2))
rewriteAtomToNNF (MaterialNonImplication f1 f2) = (Negation (MaterialImplication f1 f2))
rewriteAtomToNNF (ExclusiveDisjunction f1 f2) = (Biconditional f1 (Negation f2))
rewriteAtomToNNF (ConverseImplication f1 f2) = (MaterialImplication f2 f1)
rewriteAtomToNNF (ConverseNonImplication f1 f2) = (Negation (ConverseImplication f1 f2))
rewriteAtomToNNF f = f

toNNF :: Eq a => PropositionalFormula a -> PropositionalFormula a
toNNF = rewriteBottomUp (rewriteTopDown rewriteAtomToNNF)

toDNF :: Eq a => PropositionalFormula a -> PropositionalFormula a
toDNF = (rewriteBottomUp (rewriteTopDown rewriteAtomToDNF)) . toNNF

rewriteAtomToDNF :: PropositionalFormula a -> PropositionalFormula a
rewriteAtomToDNF (Conjunction p (Disjunction q r)) = (Disjunction (Conjunction p q) (Conjunction p r))
rewriteAtomToDNF (Conjunction (Disjunction q r) p) = (Disjunction (Conjunction q p) (Conjunction r p))
rewriteAtomToDNF f = f

toCNF :: Eq a => PropositionalFormula a -> PropositionalFormula a
toCNF = (rewriteBottomUp (rewriteTopDown rewriteAtomToCNF)) . toNNF

rewriteAtomToCNF :: PropositionalFormula a -> PropositionalFormula a
rewriteAtomToCNF (Disjunction p (Conjunction q r)) = (Conjunction (Disjunction p q) (Disjunction p r))
rewriteAtomToCNF (Disjunction (Conjunction q r) p) = (Conjunction (Disjunction q p) (Disjunction r p))
rewriteAtomToCNF f = f

mapAtoms :: (a -> b) -> PropositionalFormula a -> PropositionalFormula b
mapAtoms f (Variable a) = (Variable (f a))
mapAtoms f (Negation a) = (Negation (mapAtoms f a))
mapAtoms f (Conjunction a b) = (Conjunction (mapAtoms f a) (mapAtoms f b))
mapAtoms f (Disjunction a b) = (Disjunction (mapAtoms f a) (mapAtoms f b))
mapAtoms f (MaterialImplication a b) = (MaterialImplication (mapAtoms f a) (mapAtoms f b))
mapAtoms f (Biconditional a b) = (Biconditional (mapAtoms f a) (mapAtoms f b))
mapAtoms f (AlternativeDenial a b) = (AlternativeDenial (mapAtoms f a) (mapAtoms f b))
mapAtoms f (JointDenial a b) = (JointDenial (mapAtoms f a) (mapAtoms f b))
mapAtoms f (MaterialNonImplication a b) = (MaterialNonImplication (mapAtoms f a) (mapAtoms f b))
mapAtoms f (ExclusiveDisjunction a b) = (ExclusiveDisjunction (mapAtoms f a) (mapAtoms f b))
mapAtoms f (ConverseImplication a b) = (ConverseImplication (mapAtoms f a) (mapAtoms f b))
mapAtoms f (ConverseNonImplication a b) = (ConverseNonImplication (mapAtoms f a) (mapAtoms f b))
mapAtoms _ Tautology = Tautology 
mapAtoms _ Contradiction = Contradiction 

renameToIntFirstOccurance :: Eq a => PropositionalFormula a -> PropositionalFormula Int
renameToIntFirstOccurance formula = fst (renameToIntFirstOccurance' formula [])
    where
    renameToIntFirstOccurance' (Variable v) dict = case (elemIndex v dict) of
                                                Nothing -> (Variable (length dict), dict ++ [v])
                                                Just i -> (Variable i, dict)


prettyWithSetInfix :: (Pretty a, Ord o) => (PropositionalFormula a -> String) -> (PropositionalFormula a -> o) -> PropositionalFormula a -> Doc ann
prettyWithSetInfix _ _ (Variable a) = pretty a
prettyWithSetInfix symbolSet precedence formula = case (operands formula) of
                                        0 -> pretty (symbolSet formula)
                                        1 -> prettyWithSetInfixUnary formula
                                        2 -> prettyWithSetInfixBinary formula
                                        _ -> pretty ""
    where
    b = pretty ' '
    o = pretty '('
    c = pretty ')'
    prettyWithSetInfixUnary formula
        | precedence formula > precedence subformula = op <> sub
        | otherwise = op <> o <> sub <> c
        where
            subformula = head (immediateSubformulas formula)
            op = pretty (symbolSet formula)
            sub = prettyWithSetInfix symbolSet precedence subformula
    prettyWithSetInfixBinary formula 
        | sub1NoParen && sub2NoParen = sub1 <> op <> sub2
        | sub1NoParen = sub1 <> op <> o <> sub2 <> c
        | sub2NoParen = o <> sub1 <> c <> op <> sub2
        | otherwise = o <> sub1 <> c <> op <> o <> sub2 <> c
        where
            sub1NoParen = predForm > predSub1 || (isAsso && opForm == opSub1)
            sub2NoParen = predForm > predSub2 || (isAsso && opForm == opSub2)
            immediateSubs = immediateSubformulas formula
            subformula1 = head immediateSubs
            subformula2 = (head . tail) immediateSubs
            isAsso = isAssociative opForm
            opForm = operator formula
            opSub1 = operator subformula1
            opSub2 = operator subformula2
            predForm = precedence formula
            predSub1 = precedence subformula1
            predSub2 = precedence subformula2
            op = b <> (pretty (symbolSet formula)) <> b
            sub1 = prettyWithSetInfix symbolSet precedence subformula1
            sub2 = prettyWithSetInfix symbolSet precedence subformula2
