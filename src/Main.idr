module Main

import Nusmv
import Import
import Osek 
import Osek.Import
import Effects
import Effect.Exception
import Lightyear.Strings
import Lightyear.Core

data TotallyNotString = Str String
implicit string2TNS : String -> TotallyNotString
string2TNS = Str

Show TotallyNotString where 
  show (Str s) = s

errorLift : Either String a -> Eff a [EXCEPTION TotallyNotString]
errorLift (Left b) = raise b
errorLift (Right a) = pure a 

parseExc : Parser t -> String -> Eff t [EXCEPTION TotallyNotString]
parseExc parser dat = errorLift $ parse parser dat

decodeExc : FromJSON t => JsonValue -> Eff t [EXCEPTION TotallyNotString]
decodeExc x = errorLift $ fromJSON x

collectInputs : Eff OSEK [EXCEPTION TotallyNotString]
collectInputs = do
  graph <- parseExc parseJSONFile "{\"nodes\":[{\"subtasks\":{\"Idle\":{\"next-block\":\"ABB18\"},\"H1\":{\"next-block\":\"ABB18\"},\"H3\":{\"next-block\":\"ABB18\"},\"H2\":{\"next-block\":\"ABB18\"}},\"id\":\"2\"},{\"subtasks\":{\"Idle\":{\"next-block\":\"ABB3/StartOS\"},\"H1\":{\"next-block\":\"ABB3/StartOS\"},\"H3\":{\"next-block\":\"ABB3/StartOS\"},\"H2\":{\"next-block\":\"ABB3/StartOS\"}},\"id\":\"1\"}]}"
  sc <- pure $ MkConf [R "H2", R "H1", R "H3", R "Idle"]
  dcFkt <- decodeExc graph
  [| (osekCtor sc) (errorLift $ dcFkt sc) |]


main : IO ()
main = do
  osek <- run $ collectInputs
  putStrLn $ exportDomain osek
