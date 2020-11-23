import Control.Monad.State


test :: State Int Int 
test = do
  put 3
  modify (+1)
  get

test2 :: State Int (Maybe Int)
--test2 = (put 3) >> ((modify (+1)) >> get)
test2 = modify (+1) >>= (\_ -> ((modify (+3) >>=  (\_ -> get >>= (\input -> state (\x -> (if input > 7 then Just (x*2) else Nothing, x*3)))))))

test3 :: State Int Int
test3 = (put 3) >>= (\_ -> get)

--runStateT test2 2 :: Data.Functor.Identity.Identity (Maybe Int, Int)

main :: IO ()
main = print $ execState test2 2

-- instance MonadTrans (StateT s) where
--     lift m = StateT $ \ s -> do
--         a <- m
--         return (a, s)
--    {-# INLINE lift #-}

me :: StateT Int IO Int
me = lift (return 3)
-- me = return 3

meIO :: StateT Int IO String
meIO = liftIO getLine

meIO2 :: StateT Int IO String
meIO2 = lift getLine