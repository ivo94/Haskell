chi2 :: [Float] -> [Float] -> Float
chi2  xs ys = sum[((x-y)^2)/y | (x, y) <- zip xs ys, length xs == length ys ]