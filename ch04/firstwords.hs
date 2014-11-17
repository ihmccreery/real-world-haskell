main = interact firstWords

firstWords :: String -> String
firstWords = unlines . map firstWord . lines

firstWord :: String -> String
firstWord "" = ""
firstWord s = words s !! 0
