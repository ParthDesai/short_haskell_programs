data JValue = JNull
              | JString String
              | JNumber Double
              | JBool Bool
              | JObject [(String, JValue)]
              | JArray [JValue]
              deriving (Eq, Ord, Show)

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right