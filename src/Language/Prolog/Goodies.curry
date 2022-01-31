------------------------------------------------------------------------------
--- This library defines operations to support the construction
--- and analysis of Prolog programs.
---
--- @author Michael Hanus
--- @version January 2022
------------------------------------------------------------------------------

module Language.Prolog.Goodies
  ( plList, isPlVar, rootOf, argsOf, goalVars, termVars, termsVars
  , termVarOccs, goalConstrs, termConstrs )
 where

import Data.List ( union )

import Language.Prolog.Types

----------------------------------------------------------------------------
--- Constructs a Prolog list object from a list of Prolog terms.
plList :: [PlTerm] -> PlTerm
plList = foldr (\t ts -> PlStruct "." [t,ts]) (PlAtom "[]")

----------------------------------------------------------------------------
-- Selector operations for Prolog programs.

--- Is a Prolog term a variable?
isPlVar :: PlTerm -> Bool
isPlVar pterm = case pterm of PlVar _ -> True
                              _       -> False

--- The name and the arity of the root of a Prolog term,
--- where the names of variables are ignored (i.e., empty).
rootOf :: PlTerm -> (String,Int)
rootOf pterm = case pterm of
  PlVar _         -> ("", 0)
  PlInt i         -> (show i, 0)
  PlFloat x       -> (show x, 0)
  PlAtom a        -> (a, 0)
  PlStruct s args -> (s, length args)

--- The arguments of a Prolog term.
argsOf :: PlTerm -> [PlTerm]
argsOf pterm = case pterm of
  PlStruct _ args -> args
  _               -> []

--- The set of all variables occurring in a Prolog goal.
goalVars :: PlGoal -> [String]
goalVars pgoal = case pgoal of
  PlLit _ ts         -> termsVars ts
  PlNeg goals        -> unionMap goalVars goals
  PlCond gs1 gs2 gs3 -> unionMap (unionMap goalVars) [gs1,gs2,gs3]

--- The set of all variables occurring in a Prolog term.
termVars :: PlTerm -> [String]
termVars pterm = case pterm of
  PlVar v       -> [v]
  PlStruct _ ts -> termsVars ts
  _             -> []

--- The set of all variables occurring in a list of Prolog terms.
termsVars :: [PlTerm] -> [String]
termsVars = unionMap termVars

--- The multi-set of all occurrences of variables in a Prolog term.
termVarOccs :: PlTerm -> [String]
termVarOccs pterm = case pterm of
  PlVar v       -> [v]
  PlStruct _ ts -> concatMap termVars ts
  _             -> []

--- The set of all data constructors together with their arities
--- used in a Prolog goal.
goalConstrs :: PlGoal -> [(String,Int)]
goalConstrs pgoal = case pgoal of
  PlLit _ ts         -> unionMap termConstrs ts
  PlNeg goals        -> unionMap goalConstrs goals
  PlCond gs1 gs2 gs3 -> unionMap (unionMap goalConstrs) [gs1,gs2,gs3]

--- The set of all data constructors together with their arities
--- used in a Prolog term.
termConstrs :: PlTerm -> [(String,Int)]
termConstrs pterm = case pterm of
  PlAtom a      -> [(a,0)]
  PlStruct s ts -> union [(s, length ts)] (unionMap termConstrs ts)
  _             -> []

----------------------------------------------------------------------------
-- Auxiliaries:

unionMap :: Eq b => (a -> [b]) -> [a] -> [b]
unionMap f = foldr union [] . map f

----------------------------------------------------------------------------
