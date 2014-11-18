myconcat :: [[a]] -> [a]
myconcat = foldr (++) []
