-- Sebastian Cielemęcki

incr x = x + 1

-- 1
lrepeat f list = lrepeat' 0 list
	where
	lrepeat' _ [] = []
	lrepeat' i (x:xs) = aux (f i)
		where
		aux 0 = lrepeat' (i+1) xs
		aux k = x:(aux (k-1))
		
t1 = take 30 $ lrepeat incr [1,2..]
t2 = take 100 $ lrepeat incr [3,2,1]
