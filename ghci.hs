m <- parse =<< readFile "samples/sanity"
p = Progress 0 [Continue (0, 0) 0 True (0, 0) 0 0] m
fmap pipe <$> UV.toList <$> (UV.freeze $ board $ maze p)
putStrLn =<< renderWithPositions p
