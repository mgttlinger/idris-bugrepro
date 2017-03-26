module Nusmv

%access public export
%default total


data NEL : Type -> Type where MkNEL : (l: List a) -> {auto ev : NonEmpty l} -> NEL a
Functor NEL where map f (MkNEL (x :: xs)) = MkNEL ((f x) :: (f <$> xs))
Foldable NEL where
  foldr f acc (MkNEL l) = foldr f acc l
    
toDPair : NEL a -> (l: List a ** NonEmpty l)  
toDPair (MkNEL l {ev}) = MkDPair l ev

nub : Eq a => NEL a -> NEL a
nub (MkNEL [] {ev}) = absurd ev
nub (MkNEL (x :: xs)) = MkNEL $ nub (x :: xs)




data NType = NBool
           | NEnum

           
mutual
  data CasePair : Lazy Bool -> Type where
    MkCasePair : NExpr NBool b ->  NExpr NEnum False -> CasePair b

  data NExpr : NType -> (futureRef: Lazy Bool) -> Type where -- others not supported for now
    BoolLiteral : Bool -> NExpr NBool False
    EnumLiteral : String -> NExpr NEnum False
    NextExpr : NExpr t False -> NExpr t True
    CaseExpr : List $ CasePair b -> NExpr NEnum b

data Range : NType -> Type where
  EnumRange : (values : List String) -> {auto ne : NonEmpty values} -> Range NEnum


interface Literable (t : Type) (tpe : NType) | t where 
  total lit : t -> NExpr tpe False


data Variable : Type where
  CVar : (name : String) -> (rng : Range NEnum) -> Variable

total name : Variable -> String 
name (CVar name _) = name

implementation Literable Bool NBool where 
  lit = BoolLiteral
implementation Literable String NEnum where 
  lit = EnumLiteral

data Assign : Type where 
  InitAss : (Variable) -> (NExpr NEnum b) -> Assign
  NextAss : (Variable) -> (NExpr NEnum b) -> Assign --complex missing for now

data Module : Type where
  MkModule : 
    (enumMembers: List Variable) ->
    (assigns: List Assign) ->
    Module

data StateVar : Type where 
  MkStateVar : (varV : Variable) -> (initA : NExpr NEnum b1) -> (nextA : Variable -> NExpr NEnum b2) -> StateVar
var : StateVar -> Variable
var (MkStateVar res _ _) = res
total init : StateVar -> Assign
init (MkStateVar v i _) = InitAss v i
total next : StateVar -> Assign
next (MkStateVar v _ n) = NextAss v $ n v
total ass : StateVar -> List Assign
ass s = [init s, next s] 


add : StateVar -> Module -> Module
add sv (MkModule es as) = MkModule (var sv :: es) (as ++ ass sv)
addAll : List StateVar -> Module -> Module
addAll svs m = foldr add m svs 

interface NShow t where nshow : t -> String

NShow String where nshow = id

implementation (NShow (NExpr NBool b), NShow (NExpr NEnum False)) => NShow (CasePair b) where 
  nshow (MkCasePair x y) = (nshow x) ++ " : " ++ (nshow y) ++ ";"
    
implementation NShow (NExpr t b) where
  nshow (BoolLiteral x) = if x then "TRUE" else "FALSE"
  nshow (EnumLiteral x) = x
  nshow (NextExpr n) = "next(" ++ nshow n ++ ")"
  nshow (CaseExpr xs) = "case\n" ++ (concat $ intersperse "\n  " $ assert_total $ nshow <$> xs) ++ "\nesac"

implementation NShow (Range NEnum)  where
  nshow (EnumRange values) = "{ " ++ (concat $ intersperse ", " values) ++ " }"
  
implementation NShow Variable where
  nshow (CVar name rng) = name ++ " : " ++ nshow rng ++ ";"
  
                                                    
implementation NShow Assign where
  nshow (InitAss x y) = "init(" ++ name x ++ ") := " ++ nshow y ++ ";"
  nshow (NextAss x y) = "next(" ++ name x ++ ") := " ++ nshow y ++ ";"
  
private section : String -> List String -> List String
section name body = if length body > 0 then name :: body else Nil
  

implementation NShow Module where
  nshow (MkModule enums assigns) = 
    let vars = nshow <$> enums
        ass = nshow <$> assigns in
  unlines $ (("MODULE ( )") ::
   (section "VAR"  vars) ++
   (section "ASSIGN" ass)
   )

enumRange : Literable t NEnum => (vals : List t) -> {auto ne: NonEmpty vals} -> Range NEnum
enumRange (x::xs) = EnumRange ((nshow $ lit x) :: (nshow . lit <$> xs))

EnumRange' : NEL String -> Range NEnum
EnumRange' n = let x = toDPair n in EnumRange (fst x) {ne = snd x}
