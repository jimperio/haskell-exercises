mean xs = meanHelper 0 0 xs

meanHelper s l []     = (fromIntegral s) / (fromIntegral l)
meanHelper s l (x:xs) = meanHelper (s + x) (l + 1) xs