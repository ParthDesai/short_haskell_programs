import Control.Monad.Reader

data MyContext = MyContext { foo :: String
, bar :: Int
} deriving (Show)

computation2 :: Reader MyContext (Maybe String)
computation2 = asks bar >>= (\n -> (asks foo >>= \x -> if n > 0 then return (Just $ "Gigig" ++ x) else return Nothing))

-- m >>= k = Reader $ \r -> runReader (k (runReader m r)) r
-- asks bar >>= (\n -> (asks foo >>= \x -> if n > 0 then return (Just $ "Gigig" ++ x) else return Nothing))
-- Reader $ (\r -> runReader ((\n -> (asks foo >>= \x -> if n > 0 then return (Just $ "Gigig" ++ x) else return Nothing)) (runReader (asks bar) r)) r)
-- Reader $ (\r -> runReader ((\n -> (Reader $ \r' -> runReader ((\x -> if n > 0 then return (Just $ "Gigig" ++ x) else return Nothing) (runReader (asks foo) r') r'))) (runReader (asks bar) r)) r)
-- Reader $ (\r -> runReader ((\n -> (Reader $ \r' -> runReader ((\x -> if n > 0 then return (Just $ "Gigig" ++ x) else return Nothing) (runReader (asks foo) r') r'))) (IntR)) r)
-- Reader $ (\r -> runReader ((\IntR -> (Reader $ \r' -> runReader ((\x -> if IntR > 0 then return (Just $ "Gigig" ++ x) else return Nothing) (runReader (asks foo) r') r'))) (IntR)) r)
-- Reader $ (\r -> runReader 
-- (Reader $ (\r' -> runReader ((\x -> if IntR > 0 then return (Just $ "Gigig" ++ x) else return Nothing) (runReader (asks foo) r') r'))) 
-- r) 


-- (asks foo >>= \x -> if n > 0 then return (Just $ "Gigig" ++ x) else return Nothing)
-- Reader $ \r' -> runReader ((\x -> if n > 0 then return (Just $ "Gigig" ++ x) else return Nothing) (runReader (asks foo) r') r')

computation :: Reader MyContext (Maybe String) 
computation = do
n <- asks bar 
x <- asks foo 
if n > 0 then return (Just x) else return Nothing
