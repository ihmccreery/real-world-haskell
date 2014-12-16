module Prettify where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

char :: Char -> Doc
char c = Char c

double :: Double -> Doc
double = text . show

empty :: Doc
empty = Empty

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

fsep :: [Doc] -> Doc
fsep = foldr1 (</>)

group :: Doc -> Doc
group x = (flatten x) `Union` x

hcat :: [Doc] -> Doc
hcat = foldr1 (<>)

line :: Doc
line = Line

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

softline :: Doc
softline = group line

text :: String -> Doc
text "" = Empty
text s  = Text s

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                  Empty        -> transform ds
                  Char c       -> c : transform ds
                  Text s       -> s ++ transform ds
                  Line         -> '\n' : transform ds
                  a `Concat` b -> transform (a:b:ds)
                  _ `Union` b  -> transform (b:ds)
