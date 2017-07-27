module Sol018 (run, maxPaths)
  where
tree :: [[Int]]
tree = [
        [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23],
        [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
        [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
        [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
        [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
        [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
        [41, 41, 26, 56, 83, 40, 80, 70, 33],
        [99, 65, 04, 28, 06, 16, 70, 92],
        [88, 02, 77, 73, 07, 63, 67],
        [19, 01, 23, 75, 03, 34],
        [20, 04, 82, 47, 65],
        [18, 35, 87, 10],
        [17, 47, 82],
        [95, 64],
        [75]
        ]

maxPaths :: [[Int]] -> Int
maxPaths (lp:lc:l) = maxPaths (maxPath lp lc : l)
    where
        maxPath (p1:p2:prevLevel) (c:currLevel) = (c + max p1 p2) : maxPath (p2:prevLevel) currLevel
        maxPath _ _ = []
maxPaths l = head $ head l

run :: IO()
run = print $ maxPaths tree