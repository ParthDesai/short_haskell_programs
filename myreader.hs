newtype MyReader r a = MyReader { runMyReader :: r -> Maybe a }



--instance Monad (MyReader r) where
--return a = MyReader $ \_ -> a
--m >>= k = MyReader $ \r -> runMyReader (k (runMyReader m r)) r

ask :: MyReader r r
ask = MyReader (\x -> Just x)

asks :: (r -> Maybe a) -> MyReader r a 
asks f = MyReader f

local :: (r -> r) -> MyReader r a -> MyReader r a 
local f m = MyReader $ runMyReader m . f