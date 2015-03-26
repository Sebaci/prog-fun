-- Sebastian Cielemęcki

-- 3
mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort cmp list = 
	merge (mergeSort cmp ys) (mergeSort cmp zs) where
		merge xs [] = xs
		merge [] xs = xs
		merge (x:xs) (y:ys)
			|cmp x y = x:(merge xs (y:ys))
			|otherwise = y:(merge (x:xs) ys)
		
		split xs = go xs xs where
			go (x:xs) (_:_:ys) = ((x:vs), zs) where (vs, zs) = go xs ys
			go xs _ = ([], xs)
			
		(ys, zs) = split list
		
t1 = mergeSort (\x y -> x <= y) [1,7,3,8,2]
a = [
	(5, 1), (1, 1), (2, 1), (3, 1), (4, 1), (5, 2), (3, 2), (5, 3),
	(4, 2), (4, 3), (1, 2), (2, 2), (3, 3), (1, 3), (1, 4), (2, 3)
	]
	
t2 = mergeSort (\(x1, x2) (y1, y2) -> x1 <= y1) a
