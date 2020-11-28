------------------------------------------------------------------------------
--- This library defines a representation for Prolog programs.
--- It does not cover all aspects of Prolog but might be useful
--- for applications generating Prolog programs.
---
--- @author Michael Hanus
--- @version November 2020
------------------------------------------------------------------------------

module Language.Prolog.Types
  ( PlClause(..), PlGoal(..), PlTerm(..), plList )
 where

----------------------------------------------------------------------------
-- Representation of Prolog programs:

--- A Prolog clause is either a program clause consisting of a head
--- and a body, or a directive or a query without a head.
data PlClause = PlClause String [PlTerm] [PlGoal]
              | PlDirective [PlGoal]
              | PlQuery [PlGoal]
 deriving Eq

--- A Prolog goal is a literal, a negated goal, or a conditional.
data PlGoal = PlLit String [PlTerm]
            | PlNeg [PlGoal]
            | PlCond [PlGoal] [PlGoal] [PlGoal]
 deriving Eq

--- A Prolog term is a variable, an atom, a number, or a structure.
data PlTerm = PlVar String
            | PlAtom String
            | PlInt Int
            | PlFloat Float
            | PlStruct String [PlTerm]
 deriving Eq

--- Constructs a Prolog list object from a list of Prolog terms.
plList :: [PlTerm] -> PlTerm
plList = foldr (\t ts -> PlStruct "." [t,ts]) (PlAtom "[]")

----------------------------------------------------------------------------

