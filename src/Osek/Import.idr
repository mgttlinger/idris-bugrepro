module Osek.Import

import Osek
import Import 
import Data.AVL.Graph


%access public export

implementation FromJSON RunnableState where 
  fromJSON j = do
    members <- fromJSON j
    nxt <- handleLookup "next-block" members
    pure $ MkRunnableState nxt 

implementation FromJSON OsekState where 
  fromJSON j = do
    members <- fromJSON j
    abb <- handleLookup "id" members
    taskDict <- handleLookup "subtasks" members
    MkOsekState abb <$> (traverse fromJSON) taskDict
    

parameters (states : List OsekState)
  enrich' : String -> Error OsekState
  enrich' name = maybeToEither ("state " ++ name ++ " missing from known states") $ find ((name ==) . abb) states

  enrich : (String, String, Maybe Syscall) -> Error (OsekState, OsekState, Maybe Syscall)
  enrich (from, to, msc) = [| pair3 (enrich' from) (enrich' to) (pure msc)|]
    where 
      pair3 : a -> b -> c -> (a, b, c)
      pair3 av bv cv = (av, bv, cv)
      

implementation FromJSON (StaticConf -> Error (OsekState, Graph OsekState Syscall)) where
  fromJSON j = do
    members <- fromJSON j
    nodes <- handleLookup "nodes" members
    pure $ \sc => [| MkPair (enrich' nodes "1") ((buildG nodes) <$> (traverse (enrich nodes) [("1", "2", Just Idle)])) |]
    
