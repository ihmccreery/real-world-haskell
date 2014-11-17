safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just $ unsafeInit xs

unsafeInit :: [a] -> [a]
unsafeInit [x] = []
unsafeInit (x:xs) = x : (unsafeInit xs)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs@(x:xs') = if p x
                         then splitWith p xs'
                         else let (prefix, rest) = break p xs in prefix:(splitWith p rest)
