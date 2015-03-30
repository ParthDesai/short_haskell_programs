

evalRPN :: [String] -> String
evalRPN = head . foldl changeAcc [] 


isOperator :: String -> Bool
isOperator x
	   | x == "-" = True
	   | x == "*" = True
	   | x == "+" = True
	   | otherwise = False


performOp :: String -> Int -> Int -> Int
performOp op x y
	   | op == "-" = x - y
	   | op == "*" = x * y
	   | op == "+" = x + y


changeAcc :: [String] -> String -> [String]
changeAcc acc new
	      | length acc == 0 = [new]
	      | length acc == 1 = new : acc
              | length acc >= 2 =  if isOperator new then let op2:op1:rest = acc 
					   in [show $ performOp new (read op1) (read op2)] ++ rest
		                         else new:acc     




