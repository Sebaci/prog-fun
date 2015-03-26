-- Sebastian Cielemęcki 

-- 6
powtorz [] = []
powtorz (x:xs) = 
  (aux x 0)++(powtorz xs)
  where
    aux n k
      | n == k = []
      | otherwise = n:(aux n (k+1))
