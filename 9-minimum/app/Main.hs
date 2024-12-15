module Main where

f :: Double -> Double
f x = x ^ 3 - 3 * sin x

simpleDivision :: (Double -> Double) -> Double -> Double -> Double -> (Double, Int)
simpleDivision f a b e = go a b 0
  where
    go a b cnt
      | (b - a) < e = (mid a b, cnt)
      | otherwise = if f m1 < f m2
                    then go a m2 (cnt + 1)
                    else go m1 b (cnt + 1)
      where
        mid a b = (a + b) / 2
        m1 = a + (b - a) / 3
        m2 = b - (b - a) / 3

goldenSection :: (Double -> Double) -> Double -> Double -> Double -> (Double, Int)
goldenSection f a b e = go a b (p - 1) 0
  where
    p = (1 + sqrt 5) / 2
    go a b ratio cnt
      | (b - a) < e = (mid a b, cnt)
      | otherwise = if f m1 < f m2 then go a m2 ratio (cnt + 1) else go m1 b ratio (cnt + 1)
      where
        m1 = b - ratio * (b - a)
        m2 = a + ratio * (b - a)
        mid a b = (a + b) / 2

main :: IO ()
main = do 
  let e = 0.00001
      (minSimple, itersSimple) = simpleDivision f 0 1 e
      (minGolden, itersGolden) = goldenSection f 0 1 e
  putStrLn $ "Minimum (simple division method): " ++ show minSimple ++ ", Iterations: " ++ show itersSimple
  putStrLn $ "Minimum (golden section method): " ++ show minGolden ++ ", Iterations: " ++ show itersGolden
  
