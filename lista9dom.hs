-- Sebastian Cielemęcki

import Control.Monad.State

-- 1
data Tree a = Leaf a | Branch (Tree a) (Tree a)
	deriving (Eq, Ord, Show)

label :: Tree a -> Tree (a, Int)
label tree = fst $ label' (tree, 0)
	where
		label' (Leaf a, num) = (Leaf (a, num), num + 1)
		label' (Branch left right, num) = (Branch left1 right1, num2)
			where
				(left1, num1) = label' (left, num)
				(right1, num2) = label' (right, num1)
				
t = Branch (Branch (Leaf 'a') (Leaf 'b')) (Branch (Leaf 'c') (Leaf 'd'))
test1 = label t

-- 2 (dom)

type TreeState a = State Int (Tree (a, Int))

mlabel' :: Tree a -> TreeState a
mlabel' (Leaf a) = do
        lbl <- get
        put (lbl+1)
        return (Leaf (a, lbl))

mlabel' (Branch left right ) = do
        newLeft <- mlabel' left
        newRight <- mlabel' right
        return (Branch newLeft newRight)

        
mlabel tree = evalState (mlabel' tree) 0

test2 = mlabel t

