module Import

import public Config.JSON

%access public export

Error : Type -> Type
Error = Either String

interface FromJSON t where 
  fromJSON : JsonValue -> Error t
  
implementation FromJSON String where 
  fromJSON (JsonString x) = Right x
  fromJSON (JsonNumber x) = Left $ show x
  fromJSON (JsonBool x) = Left $ show x
  fromJSON JsonNull = Left "null where String was expected"
  fromJSON (JsonArray xs) = Left "array where string was expected"
  fromJSON (JsonObject x) = Left "object where string was expected"

implementation (FromJSON t) => FromJSON (List t) where
  fromJSON (JsonString x) = Left x
  fromJSON (JsonNumber x) = Left $ show x
  fromJSON (JsonBool x) = Left $ show x
  fromJSON JsonNull = Left "null where List was expected"
  fromJSON (JsonArray xs) = traverse fromJSON xs
  fromJSON (JsonObject x) = Left "object where array was expected"

implementation FromJSON (Dict String JsonValue) where  
  fromJSON (JsonString x) = Left x
  fromJSON (JsonNumber x) = Left $ show x
  fromJSON (JsonBool x) = Left $ show x
  fromJSON JsonNull = Left "null where Object was expected"
  fromJSON (JsonArray xs) = Left "array where object was expected"
  fromJSON (JsonObject x) = Right x

implementation FromJSON JsonValue where 
  fromJSON = Right . id
  
implementation (FromJSON t) => FromJSON (Maybe t) where
  fromJSON JsonNull = Right Nothing
  fromJSON j = Just <$> fromJSON j

Ord a => Functor (Dict a) where 
  map func x = fromList $ (\(a, b) => (a, func b)) <$> toList x
Ord a => Foldable (Dict a) where   
  foldr func = foldr $ const func
Ord a => Traversable (Dict a) where 
  traverse f x = fromList <$> (traverse (\(a, b) => (MkPair a) <$> f b) $ Data.AVL.Dict.toList x)

handleLookup' : String -> Dict String t -> Error t
handleLookup' x members = maybeToEither ("Key " ++ x ++ " missing from keyset: " ++ (show $ keys members)) $ lookup x members

partial handleLookup : FromJSON t => String -> Dict String JsonValue -> Error t
handleLookup x members = case lookupResult of
                              Left errmsg => Left $ errmsg
                              Right res => Right res
                         where 
                           lookupResult : Error t
                           lookupResult = handleLookup' x members >>= fromJSON
