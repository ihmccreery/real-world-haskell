module SimpleJSON
    (JValue(..))
    where

-- JSON supports four basic types of value, (strings, numbers, Booleans, and a special value named null)
-- and two compound types, (objects and arrays)
data JValue = JString {getString :: String}
            | JNumber {getDouble :: Double}
            | JBool {getBool :: Bool}
            | JNull
            | JObject {getObject :: [(String, JValue)]}
            | JArray {getArray :: [JValue]}
            deriving (Show, Eq, Ord)
