-- a rendering function that prints a value in JSON form
module PutJSON where

import Data.List (intercalate)
import SimpleJSON

putJValue :: JValue -> IO ()
putJValue = putStrLn . renderJValue

renderJValue :: JValue -> String
renderJValue (JString s)   = show s
renderJValue (JNumber x)   = show x
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"
renderJValue (JObject o)   = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map renderPair ps)
          renderPair (s, v) = show s ++ ": " ++ (renderJValue v)
renderJValue (JArray a)    = "[" ++ values a ++ "]"
    where values [] = ""
          values vs = intercalate ", " (map renderJValue vs)
