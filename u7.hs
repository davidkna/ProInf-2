selectSort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
selectSort operator liste = [getFirst liste]
	where
		operator' = operator . not  
		getFirst :: (Ord a) =>[a] -> a
		getFirst = foldr1 (\x acc -> if x `operator'` acc then x else acc) 
		delFirst :: (Ord a) => a -> [a] -> [a] 
		delFirst x (y:ys)
			| x == y = ys
			| otherwise = x : (delFirst x ys)
		delFirst _ _ = undefined