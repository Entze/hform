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
    simplify,
 ) where

import Prelude (
    Int,
    Show,
    Eq,
    String,
    head,
    tail,
    otherwise,
    (&&),
    (==),
    (<=),
    (>),
    (.),
    )
import Data.Text.Prettyprint.Doc

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
    pretty = (prettyWithSymbolsInfix canonicalSymbolSet)

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

precedence :: PropositionalFormula a -> Int
precedence Contradiction = 0
precedence Tautology = 0
precedence (Variable _) = 0
precedence (Negation _) = 1
precedence (Conjunction _ _) = 2
precedence (Disjunction _ _) = 3
precedence (MaterialImplication _ _) = 4
precedence (Biconditional _ _) = 5
precedence (AlternativeDenial _ _) = 6
precedence (JointDenial _ _) = 6
precedence (MaterialNonImplication _ _) = 7
precedence (ExclusiveDisjunction _ _) = 8
precedence (ConverseImplication _ _) = 9
precedence (ConverseNonImplication _ _) = 10

operands :: PropositionalFormula a -> Int
operands Contradiction = 0
operands Tautology = 0
operands (Variable _) = 0
operands (Negation _) = 1
operands _ = 2

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


simplify :: (Eq a) => PropositionalFormula a -> PropositionalFormula a
simplify f@(Negation _)  = simplifyNegation f
simplify f@(Conjunction _ _) = simplifyConjunction f
simplify f@(Disjunction _ _) = simplifyDisjunction f
simplify f@(MaterialImplication _ _) = simplifyMaterialImplication f
simplify f@(Biconditional _ _) = simplifyBiconditional f
simplify f@(AlternativeDenial _ _) = simplifyAlternativeDenial f
simplify f@(JointDenial _ _) = simplifyJointDenial f
simplify f@(MaterialNonImplication _ _) = simplifyMaterialNonImplication f
simplify f@(ExclusiveDisjunction _ _) = simplifyExclusiveDisjunction f
simplify f@(ConverseImplication _ _) = simplifyConverseImplication f
simplify f@(ConverseNonImplication _ _) = simplifyConverseNonImplication f
simplify f = f


simplifyNegation (Negation (Negation f)) = f
simplifyNegation (Negation (Contradiction)) = Tautology
simplifyNegation (Negation (Tautology)) = Contradiction
simplifyNegation (Negation (Conjunction (Negation f1) (Negation f2))) = Disjunction f1 f2
simplifyNegation (Negation (Disjunction (Negation f1) (Negation f2))) = Conjunction f1 f2
simplifyNegation (Negation (Conjunction f1 f2)) = AlternativeDenial f1 f2
simplifyNegation (Negation (Disjunction f1 f2)) = JointDenial f1 f2
simplifyNegation (Negation (MaterialImplication f1 f2)) = MaterialNonImplication f1 f2
simplifyNegation (Negation (Biconditional f1 f2)) = ExclusiveDisjunction f1 f2
simplifyNegation (Negation (AlternativeDenial f1 f2)) = Conjunction f1 f2
simplifyNegation (Negation (JointDenial f1 f2)) = Disjunction f1 f2
simplifyNegation (Negation (MaterialNonImplication f1 f2)) = MaterialImplication f1 f2
simplifyNegation (Negation (ExclusiveDisjunction f1 f2)) = Biconditional f1 f2
simplifyNegation (Negation (ConverseImplication f1 f2)) = ConverseNonImplication f1 f2
simplifyNegation (Negation (ConverseNonImplication f1 f2)) = ConverseImplication f1 f2
simplifyNegation f = f

simplifyConjunction (Conjunction f1 Tautology) = f1
simplifyConjunction (Conjunction Tautology f2) = f2
simplifyConjunction (Conjunction _ Contradiction) = Contradiction
simplifyConjunction (Conjunction Contradiction _) = Contradiction
simplifyConjunction (Conjunction (Negation f1) (Negation f2)) = JointDenial f1 f2
simplifyConjunction (Conjunction f1 (Negation f2))
    | f1 == f2 = Contradiction
    | otherwise = MaterialNonImplication f1 f2
simplifyConjunction (Conjunction (Negation f1) f2)
    | f1 == f2 = Contradiction
    | otherwise = ConverseNonImplication f1 f2
simplifyConjunction f@(Conjunction f1 f2)
    | f1 == f2 = f1
    | otherwise = f


simplifyDisjunction (Disjunction _ Tautology) = Tautology
simplifyDisjunction (Disjunction Tautology _) = Tautology
simplifyDisjunction (Disjunction f1 Contradiction) = f1
simplifyDisjunction (Disjunction Contradiction f2) = f2
simplifyDisjunction (Disjunction (Negation f1) (Negation f2)) = AlternativeDenial f1 f2
simplifyDisjunction (Disjunction f1 (Negation f2))
    | f1 == f2 = Tautology
    | otherwise = ConverseImplication f1 f2
simplifyDisjunction (Disjunction (Negation f1) f2)
    | f1 == f2 = Tautology
    | otherwise = MaterialImplication f1 f2
simplifyDisjunction f@(Disjunction f1 f2)
    | f1 == f2 = f1
    | otherwise = f

simplifyMaterialImplication (MaterialImplication _ Tautology) = Tautology
simplifyMaterialImplication (MaterialImplication Tautology f2) = f2
simplifyMaterialImplication (MaterialImplication f1 Contradiction) = f1
simplifyMaterialImplication (MaterialImplication Contradiction _) = Tautology
simplifyMaterialImplication (MaterialImplication (Negation f1) (Negation f2)) = ConverseImplication f1 f2
simplifyMaterialImplication (MaterialImplication f1 (Negation f2))
    | f1 == f2 = Negation f1
    | otherwise = AlternativeDenial f1 f2
simplifyMaterialImplication (MaterialImplication (Negation f1) f2)
    | f1 == f2 = f1
    | otherwise = Disjunction f1 f2
simplifyMaterialImplication f@(MaterialImplication f1 f2)
    | f1 == f2 = Tautology
    | otherwise = f

simplifyBiconditional f = f

simplifyAlternativeDenial f = f

simplifyJointDenial f = f

simplifyMaterialNonImplication (MaterialNonImplication _ Tautology) = Contradiction
simplifyMaterialNonImplication (MaterialNonImplication Tautology f2) = (Negation f2)
simplifyMaterialNonImplication (MaterialNonImplication Contradiction f2) = Contradiction
simplifyMaterialNonImplication (MaterialNonImplication f1 Contradiction) = f1
simplifyMaterialNonImplication (MaterialNonImplication (Negation f1) (Negation f2)) = ConverseNonImplication f1 f2
simplifyMaterialNonImplication (MaterialNonImplication f1 (Negation f2))
    | f1 == f2 = f1
    | otherwise = Conjunction f1 f2
simplifyMaterialNonImplication (MaterialNonImplication (Negation f1) f2)
    | f1 == f2 = Negation f1
    | otherwise = JointDenial f1 f2
simplifyMaterialNonImplication f@(MaterialNonImplication f1 f2)
    | f1 == f2 = Contradiction
    | otherwise = f

simplifyMaterialNonImplication f = f

simplifyExclusiveDisjunction f = f

simplifyConverseImplication f = f

simplifyConverseNonImplication f = f



prettyWithSymbolsInfix :: (Pretty a) => (PropositionalFormula a -> String) -> PropositionalFormula a -> Doc ann
prettyWithSymbolsInfix symbolSet (Variable a) = pretty a
prettyWithSymbolsInfix symbolSet formula = case (operands formula) of
                                        0 -> pretty (symbolSet formula)
                                        1 -> prettyWithSymbolsInfixUnary formula
                                        2 -> prettyWithSymbolsInfixBinary formula
                                        _ -> pretty ""
    where
    b = pretty ' '
    o = pretty '('
    c = pretty ')'
    prettyWithSymbolsInfixUnary formula
        | precedence formula > precedence subformula = op <> sub
        | otherwise = op <> o <> sub <> c
        where
            subformula = head (immediateSubformulas formula)
            op = pretty (symbolSet formula)
            sub = prettyWithSymbolsInfix symbolSet subformula
    prettyWithSymbolsInfixBinary formula 
        | predForm > predSub1 && predForm > predSub2 = sub1 <> b <> op <> b <> sub2
        | predForm > predSub1 && predForm <= predSub2 = sub1 <> b <> op <> b <> o <> sub2 <> c
        | predForm <= predSub1 && predForm > predSub2 = o <> sub1 <> c <> op <> sub2
        | otherwise = o <> sub1 <> c <> b <> op <> b <> o <> sub2 <> c
        where
            immediateSubs = immediateSubformulas formula
            subformula1 = head immediateSubs
            subformula2 = (head . tail) immediateSubs
            predForm = precedence formula
            predSub1 = precedence subformula1
            predSub2 = precedence subformula2
            op = pretty (symbolSet formula)
            sub1 = prettyWithSymbolsInfix symbolSet subformula1
            sub2 = prettyWithSymbolsInfix symbolSet subformula2
