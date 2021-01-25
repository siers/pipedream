m <- parse =<< readFile "samples/sanity"
p = Progress 0 [Continue (0, 0) 0 True (0, 0) 0 0] m
fmap pipe <$> UV.toList <$> (UV.freeze $ board $ maze p)
putStrLn =<< renderWithPositions p

:l solve
m <- parse "   \n   \n   \n"
partEquate m (1,1)
mazeEquate m (1,1) [(2,2)]
partEquate m (1,1)
partEquate m (2,2)
mazeEquate m (0,0) [(1,1)]
partEquate m (2,2)
