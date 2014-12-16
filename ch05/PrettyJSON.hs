module PrettyJSON where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import SimpleJSON
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text, compact, pretty)

renderJValue :: JValue -> Doc
renderJValue (JString s)   = string s
renderJValue (JNumber x)   = double x
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JArray a)    = series '[' ']' renderJValue a
renderJValue (JObject o)   = series '{' '}' renderField o
    where renderField (s, v) = string s <> text ": " <> (renderJValue v)

string :: String -> Doc
string = (enclose '"' '"') . hcat . (map oneChar)

oneChar :: Char -> Doc
oneChar c = case (lookup c simpleEscapes) of
                 Just r -> text r
                 Nothing | mustEscape c -> hexEscape c
                         | otherwise -> char c
            where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral (d - 0x10000) where d = ord c

smallHex :: Int -> Doc
smallHex x = (text "\\u") <> (text $ replicate (4 - length h) '0') <> (text h)
    where h = showHex x ""

astral :: Int -> Doc
astral x = (smallHex $ a + 0xd800) <> (smallHex $ b + 0xdc00)
    where a = (x `shiftR` 10) .&. 0x3ff
          b = x .&. 0x3ff

enclose :: Char -> Char -> Doc -> Doc
enclose left right d = (char left) <> d <> (char right)

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close f = enclose open close . fsep . (punctuate $ char ',') . (map f)
