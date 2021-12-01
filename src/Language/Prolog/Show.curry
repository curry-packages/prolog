------------------------------------------------------------------------------
--- This library contains a simple pretty printer for showing Prolog programs.
---
--- @author Michael Hanus
--- @version December 2021
------------------------------------------------------------------------------

module Language.Prolog.Show
  ( showPlProg, showPlClause, showPlGoals, showPlGoal, showPlTerm )
 where

import Data.Char ( isAlphaNum, isLower )
import Data.List ( union, intercalate )

import Language.Prolog.Types

----------------------------------------------------------------------------
--- Shows a Prolog program in standard Prolog syntax.
--- The clauses are also optimized by removing superfluous unification
--- literals.
showPlProg :: [PlClause] -> String
showPlProg clauses = unlines $ map (showPlClause . optimizeClause) clauses

--- Shows a Prolog clause in standard Prolog syntax.
showPlClause :: PlClause -> String
showPlClause (PlClause pred args []) =
  showPlGoal (PlLit pred args) ++ "."
showPlClause (PlClause pred args body@(_:_)) =
  showPlGoal (PlLit pred args) ++ " :- " ++ showPlGoals body ++ "."
showPlClause (PlDirective body) =
  ":- " ++ showPlGoals body ++ "."
showPlClause (PlQuery body) =
  "?- " ++ showPlGoals body ++ "."

--- Shows a list of Prolog goals in standard Prolog syntax.
showPlGoals :: [PlGoal] -> String
showPlGoals gs = intercalate ", " (map showPlGoal gs)

showSimpleGoals :: [PlGoal] -> String
showSimpleGoals []         = "true"
showSimpleGoals [g]        = showPlGoal g
showSimpleGoals gs@(_:_:_) = "(" ++ intercalate ", " (map showPlGoal gs) ++ ")"

--- Shows a Prolog goal in standard Prolog syntax.
showPlGoal :: PlGoal -> String
showPlGoal (PlLit pred args)
  | pred == "="
  = showPlTerm (args!!0) ++ "=" ++ showPlTerm (args!!1)
  | pred == "!" && null args
  = "!"
  | otherwise
  = showPlTerm (PlStruct pred args)
showPlGoal (PlNeg goal) =
  "\\+" ++ showSimpleGoals goal
showPlGoal (PlCond cond tgoal fgoal) =
  "(" ++ showSimpleGoals cond ++ " -> " ++ showPlGoals tgoal ++ " ; " ++
  showPlGoals fgoal ++ ")"

--- Shows a Prolog term in standard Prolog syntax.
showPlTerm :: PlTerm -> String
showPlTerm (PlVar v)       = v
showPlTerm (PlAtom a)      = showPlAtom a
showPlTerm (PlInt i)       = show i
showPlTerm (PlFloat f)     = show f
showPlTerm (PlStruct f []) = showPlAtom f
showPlTerm (PlStruct f args@(h:t))
  | f=="." && length args == 2 -- a Prolog list
  = "[" ++ showPlTerm h ++ showPlListElems (head t) ++ "]"
  | (f=="," || all (`elem` specialChars) f) && length args == 2 -- infix op
  = "(" ++ showPlTerm (args!!0) ++ f ++ showPlTerm (args!!1) ++ ")"
  | otherwise
  = showPlAtom f ++ "(" ++ intercalate "," (map showPlTerm args) ++ ")"

showPlListElems :: PlTerm -> String
showPlListElems xs = case xs of
  PlAtom "[]" -> ""
  PlStruct f [y,ys] -> if f=="." then "," ++ showPlTerm y ++ showPlListElems ys
                                 else "|" ++ showPlTerm ys
  _                 -> "|" ++ showPlTerm xs

showPlAtom :: String -> String
showPlAtom a =
  if a=="[]" || (all (\c -> isAlphaNum c || c=='_') a && isLower (head a))
             || all (`elem` specialChars) a
  then a
  else '\'': (concatMap (\c->if c=='\'' then "\\\'" else [c]) a) ++"\'"

specialChars :: String
specialChars = "+-*/<=>`\\:.?@#$&^~"

----------------------------------------------------------------------------
-- Optimize a Prolog clause: "head :- b1,X=Y,b2" is replaced by
-- "head :- b1,[X/Y]b2" if X does not occur in head and b1

optimizeClause :: PlClause -> PlClause
optimizeClause (PlClause pred args body) =
  PlClause pred args (optimizeBody (unionMap varsOf args) body)
optimizeClause (PlDirective body) = PlDirective (optimizeBody [] body)
optimizeClause (PlQuery     body) = PlQuery     (optimizeBody [] body)

optimizeBody :: [String] -> [PlGoal] -> [PlGoal]
optimizeBody _    []                  = []
optimizeBody vars (PlNeg goal : lits) =
  PlNeg goal : optimizeBody (union vars (unionMap varsOfLit goal)) lits
optimizeBody vars (PlCond cond tgoal fgoal : lits) =
  let ocond  = optimizeBody vars cond
      ocvars = union vars (unionMap varsOfLit cond)
      otgoal = optimizeBody ocvars tgoal
      ofgoal = optimizeBody ocvars fgoal
  in PlCond ocond otgoal ofgoal :
     optimizeBody (union ocvars (unionMap varsOfLit (otgoal ++ ofgoal))) lits
optimizeBody vars (PlLit pred args : lits)
  | pred == "=" && isPlVar (head args) && (varOf (head args) `notElem` vars)
  = optimizeBody (union vars (varsOf (args!!1)))
                 (map (replaceInLit (varOf (head args)) (args!!1)) lits)
  | pred == "=" && isPlVar (args!!1) && (varOf (args!!1) `notElem` vars)
  = optimizeBody (union vars (varsOf (args!!0)))
                 (map (replaceInLit (varOf (args!!1)) (args!!0)) lits)
  | otherwise
  = PlLit pred args : optimizeBody (union vars (unionMap varsOf args)) lits

replaceInLit :: String -> PlTerm -> PlGoal -> PlGoal
replaceInLit x y (PlLit pred args) = PlLit pred (map (replaceInTerm x y) args)
replaceInLit x y (PlNeg goal)      = PlNeg (map (replaceInLit x y) goal)
replaceInLit x y (PlCond cond tgoal fgoal) =
  PlCond (map (replaceInLit x y) cond)
         (map (replaceInLit x y) tgoal)
         (map (replaceInLit x y) fgoal)

replaceInTerm :: String -> PlTerm -> PlTerm -> PlTerm
replaceInTerm x y (PlVar   v) = if x==v then y else PlVar v
replaceInTerm _ _ (PlAtom  a) = PlAtom  a
replaceInTerm _ _ (PlInt   i) = PlInt   i
replaceInTerm _ _ (PlFloat f) = PlFloat f
replaceInTerm x y (PlStruct f args) = PlStruct f (map (replaceInTerm x y) args)

varsOfLit :: PlGoal -> [String]
varsOfLit (PlLit _ args)    = unionMap varsOf args
varsOfLit (PlNeg g)         = unionMap varsOfLit g
varsOfLit (PlCond g1 g2 g3) = unionMap varsOfLit (g1++g2++g3)

varsOf :: PlTerm -> [String]
varsOf (PlVar   v) = [v]
varsOf (PlAtom  _) = []
varsOf (PlInt   _) = []
varsOf (PlFloat _) = []
varsOf (PlStruct _ args) = unionMap varsOf args

isPlVar :: PlTerm -> Bool
isPlVar t = case t of
              PlVar _ -> True
              _       -> False

varOf :: PlTerm -> String
varOf t = case t of
  PlVar v -> v
  _       -> error "Internal error in Prolog.Show.varOf: not a variable"

unionMap :: Eq b => (a -> [b]) -> [a] -> [b]
unionMap f = foldr union [] . map f

----------------------------------------------------------------------------
