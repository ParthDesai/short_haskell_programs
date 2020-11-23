main = putStrLn "Welcome to calculator program. I will calculate stuff until you tell me to stop by using stop command" >> chooseFn >>= 
    (\maybeFn -> 
            return (maybeFn 
                >>= (\fn -> 
                        return (chooseOperand >>= (\op1 -> chooseOperand >>= (\op2 -> return $ fn op1 op2))) 
                    )
            ) 
    ) >>= (\answer -> case answer of Nothing -> putStrLn "This failed, we didn't get answer"
                                     Just (action) -> (action >>= (\result -> putStrLn ("Finally, I got the result:" ++ show result))))

chooseFn :: (Num a, Fractional a) => IO (Maybe (a -> a -> a))
chooseFn = putStrLn "Please select one of the operations:*,/,-,+" >> getLine >>= (\line -> if length line /= 1 then return Nothing else case line of "*" -> return (Just (*))
                                                                                                                                                     "-" -> return (Just (-))
                                                                                                                                                     "/" -> return (Just (/))
                                                                                                                                                     "+" -> return (Just (+)))

chooseOperand :: IO Double
chooseOperand = putStrLn "Please choose your operand" >> getLine >>= (\line -> return $ read line)

data Expr = I Int
        | B Bool           -- boolean constants
        | Add Expr Expr
        | Mul Expr Expr
        | Eq  Expr Expr    -- equality test

eval :: Expr -> Maybe (Either Int Bool)
eval (I i) = Just (Left i)
eval (B b) = Just (Right b)
eval (Add (I i1) (I i2)) = Just (Left $ i1+i2)
eval (Mul (I i1) (I i2)) = Just (Left $ i1*i2)
eval (Eq (I i1) (I i2)) = Just (Right $ i1 == i2)
eval (Eq (B b1) (B b2)) = Just (Right $ b1 == b2)
eval (Add (I _) (B _)) = Nothing
eval (Add (B _) (I _)) = Nothing
eval (Mul (B _) (I _)) = Nothing
eval (Mul (I _) (B _)) = Nothing
eval (Add expr1 expr2) = (eval expr1) >>= (\ans1 -> (eval expr2 >>= (\ans2 ->  case (ans1, ans2) of (Left i1, Left i2) -> return (Left $ i1+i2)
                                                                                                    _ -> Nothing)))
eval (Mul expr1 expr2) = (eval expr1) >>= (\ans1 -> (eval expr2 >>= (\ans2 -> case (ans1, ans2) of (Left i1, Left i2) -> return (Left $ i1*i2)
                                                                                                   _ -> Nothing)))
eval (Eq expr1 expr2) = (eval expr1) >>= (\ans1 -> (eval expr2 >>= (\ans2 -> case (ans1, ans2) of (Right b1, Right b2) -> return (Right $ b1==b2)
                                                                                                  (Left i1, Left i2) -> return (Right $ i1==i2)
                                                                                                  _ -> Nothing)))                                                                              