module Osek

import public Data.AVL.Graph
import Import
import Nusmv

%access public export
%default total

data Syscall : Type where
  Idle : Syscall
implementation Eq Syscall where 
  Idle == Idle = True 
  _ == _ = False

Literable Syscall NEnum where 
  lit Idle = EnumLiteral "Idle"

record Runnable where 
  constructor R
  name : String  
Eq Runnable where (R a) == (R b) = a == b

record RunnableState where 
  constructor MkRunnableState
  nextBlock : String

record OsekState where 
  constructor MkOsekState
  abb : String
  runnableStates : Dict String RunnableState
  
implementation Eq OsekState where 
  a == b = abb a == abb b
  
  
record StaticConf where 
  constructor MkConf
  rs : List Runnable

record OSEK where
  constructor MkOSEK
  static : StaticConf
  start : OsekState
  cfg : Graph OsekState Syscall
  
osekCtor : StaticConf -> (OsekState, Graph OsekState Syscall) -> OSEK
osekCtor sc (star, gra) = MkOSEK sc star gra

edgeToTransition : Graph OsekState Syscall -> Edge Syscall -> Maybe (OsekState, Syscall, OsekState)
edgeToTransition cfg (startID, endID, maybeSyscall) = do
                                                      syscall <- maybeSyscall
                                                      start <- getValueByID startID cfg
                                                      end <- getValueByID endID cfg
                                                      pure (start, syscall, end)

parameters (os : OSEK)
  stateTransitions : List (OsekState, Syscall, OsekState)
  stateTransitions = catMaybes $ edgeToTransition (cfg os) <$> (edges . cfg) os
  
  states : NEL OsekState
  states = MkNEL $ nub $ (start os) :: (vertices . cfg) os


parameters (sc : StaticConf)
  runnables : List Runnable
  runnables = rs sc

total abbAt : Runnable -> OsekState -> String
abbAt ru st = case nextBlock <$> (lookup (name ru) $ runnableStates st) of
                   Just rs => rs
                   Nothing => ""


parameters (os : OSEK, sc : StaticConf)
  g_fromStateWD : Literable l NEnum => (OsekState -> l) -> Variable -> NExpr NEnum True
  g_fromStateWD f v = CaseExpr $ (transToCasePair (lit . f) <$> stateTransitions os)
    where 
      transToCasePair : (OsekState -> NExpr NEnum False) -> (OsekState, Syscall, OsekState) -> CasePair True
      transToCasePair f (start, call, end) = MkCasePair (NextExpr $ lit True) $ f end

  g_runnableAbb : Runnable -> StateVar
  g_runnableAbb x = MkStateVar (CVar ("abb_") (enumRange $ toList $ abbAt x <$> states os)) (lit $ abbAt x $ start os) (g_fromStateWD $ abbAt x)


  mainModule : Module
  mainModule = addAll runnableAbbs $
    MkModule [] []
      where 
        runnableAbbs : List $ StateVar
        runnableAbbs = g_runnableAbb <$> (runnables sc)
      
public export interface Exportable t where 
  exportDomain : t -> String
      
implementation Exportable OSEK where 
  exportDomain os = let sc = static os in
                        unlines $ [ (nshow (mainModule os sc)) ]
