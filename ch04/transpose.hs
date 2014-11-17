main = interact (unlines . transpose . lines)

transpose :: [String] -> [String]
transpose xs
    | all null xs = []
    | otherwise   = (map safeHead xs):(transpose $ map safeTail xs)
        where safeHead xs = if null xs then ' ' else head xs
              safeTail xs = if null xs then [] else tail xs

