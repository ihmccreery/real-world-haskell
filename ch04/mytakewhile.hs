myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred = foldr f []
    where f x acc = if pred x
                    then x:acc
                    else []
