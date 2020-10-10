module Hform.Util (
    convergingLimit
) where



convergingLimit :: Eq a => [a] -> a
convergingLimit (a:b:as)
    | a == b = a
    | otherwise = convergingLimit (b:as)
convergingLimit [a] = a
convergingLimit [] = error "No elements"