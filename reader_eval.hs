import Control.Monad.Reader 

type Env = [(String, Int)]
type Eval a = ReaderT Env Maybe a

data Expr = Val Int | Add Expr Expr | Var String  

eval :: Expr -> Eval Int
eval exp = case exp of Val a -> return a
                       Add exp1 exp2 -> eval exp1 >>= (\x -> (eval exp2 >>= (\y -> return $ x+y)))
                       Var s -> ReaderT (lookup s)


example1 = eval (Add (Add (Val 5) (Var "Hello")) (Add (Var "World") (Val 6))) 