--This function calculate shortest path based on the Example format described in "Learn you haskell for great good" 

findShortestPath :: [(Int,Int,Int)] -> [Int]
findShortestPath xs = snd $ foldl evalPath ((0,0,0),[]) xs


evalPath :: ((Int,Int,Int),[Int]) -> (Int, Int, Int)  -> ((Int,Int,Int),[Int])
evalPath (_,[]) new@(x,y,joint)  = (new,[min x y])

evalPath (prev@(x',y',joint'),xs) new@(x,y,joint) 
		| last xs == x' = if joint' + y < x then (new, xs ++ [joint', y]) else (new, xs ++ [x])
		| last xs == y' = if joint' + x < y then (new, xs ++ [joint', x]) else (new, xs ++ [y])
