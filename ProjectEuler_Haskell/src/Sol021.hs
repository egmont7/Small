
divs :: Int -> [Int]
divs n = 1:getDivs 2
    where
        getDivs i
            | i > n2 = []
            | mod n i == 0 = i:getDivs (i+1)
            | otherwise = getDivs (i+1)
        n2 = n `div` 2

d :: Int -> Int
d i = sum (divs i)

amicablePairs :: Int -> [Int]
amicablePairs n = getPairs 1
    where
        getPairs i
            | i==n           = []
            | d b == i && b/=i = i:getPairs (i+1)
            | otherwise     = getPairs (i+1)
            where
                b = d i

main :: IO()
main = print $ sum (amicablePairs 10000)
-- main = print $ amicablePairs 10000
