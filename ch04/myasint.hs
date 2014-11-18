import Data.Char

asInt :: String -> Int
asInt "" = 0
asInt cs@(c:cs') = if c == '-'
               then - asIntNat cs'
               else asIntNat cs

asIntNat :: String -> Int
asIntNat = foldl f 0
    where f n c = 10 * n + digitToInt c
