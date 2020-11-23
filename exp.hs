import Control.Monad (guard)
lucky :: (Integral a) => a -> String
lucky 7 = "H"
lucky _ = "g"

sayME :: (Show a) => a -> String
sayME x = show x

fibonacci :: (Integral a) => a -> [a]
fibonacci 1 = [0,1]
fibonacci x = let ans = fibonacci (x-1) 
                  ans_length = length ans 
              in ans ++ [(ans !! (ans_length - 1)) + (ans !! (ans_length - 2))]  


addVectors :: Num a => (a, a) -> (a, a) -> (a, a)
addVectors (x, y) (a, b) = (x + a, y + b)

addVectorsAlt :: Num a => (a, a) -> (a, a) -> (a, a)
addVectorsAlt x y = (fst x + fst y, snd x + snd y)

myHead :: [a] -> a
myHead [] = error "Can't call head on an empty list, dummy!" 
myHead (x:xs) = x

tell :: (Show a) => [a] -> String
tell [] = "It's an empty list"
tell (x:[]) = "It's a list with only one element " ++ show x
tell (x:y:[]) = "it's a list with only two element " ++ show x ++ " " ++ show y
tell (x:y:_) = "It's list with more than two element " ++ show x ++ " " ++ show y

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: Integral a => [a] -> a
sum' xs = case xs of [] -> 0
                     (x:xs') -> x + sum' xs'

bmiTell :: (Integral a, Num b, Eq b) => a -> b -> String
bmiTell x y | z == 21 = "This is 21"
            | z == 22 = "This is 22"
            | z == 32 = "This is 32"
            | otherwise = "Don't know" where z = (fromIntegral x)*y

bmiTell2 :: (Integral a, Num b, Eq b) => a -> b -> String
bmiTell2 x y | z x y == 21 = "This is 21"
             | otherwise = "Don't lnow man" where z x y = (fromIntegral x)*y

max' :: Ord a => [a] -> a -> a
max' [] def = def
max' (x:[]) _ = x
max' (x:xs) def = let xs_max = max' xs def in if x > xs_max then x else xs_max

repeat' :: (Num b, Eq b) => a -> b -> [a]
repeat' elem 0 = []
repeat' elem n = elem:repeat' elem (n-1)

take' :: [a] -> Int -> [a]
take' xs n | length xs < n = error "you cannot take more element than you have."
           | length xs == 0 = []
           | n <= 0 = xs
           | otherwise = let (_:xs') = xs in take' xs' (n-1)

reverse' :: [a] -> [a]
reverse' xs = case xs of [] -> []
                         (x:xs) -> reverse' xs ++ [x]


zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys = case (xs, ys) of ([],_) -> []
                              (_, []) -> []
                              ((x:xs'), (y:ys')) -> (x,y):zip' xs' ys' 

elem' :: (Eq a) => [a] -> a -> Bool
elem' xs x = case xs of [] -> False
                        (x':xs') -> if x' == x then True else elem' xs' x 

quicksort' :: (Ord a) => [a] -> [a]
quicksort' xs = case xs of [] -> []
                           (x:xs) -> (quicksort' [x' | x'<-xs, x' < x]) ++ [x] ++ (quicksort' [x' | x' <- xs, x' >= x])

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface s = case s of (Circle (Point x y) y') -> x+y+y'
                      (Rectangle (Point x y) (Point a b)) -> (x+a)*(y+b)

tripleMulti :: (Num a) => a -> a -> a -> a
tripleMulti x y z = x*y*z

tripleHigherMulti :: (Num a) => (a -> a) -> a -> a
tripleHigherMulti f z = f z

compareWith100 :: (Num a, Ord a) => a -> Ordering
compareWith100 = (`compare` 100)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> (a, b)) -> [a] -> [b] -> [(a, b)]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g where g y x = f x y

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x<-xs, f x]

findLargestDivisable :: (Ord a, Integral a) => a -> a -> a
findLargestDivisable divisable limit | limit == 0 = error "limit need to be greater than 0"
                                     | divisable == 0 = error "you cannot divide by zero"
                                     | otherwise = let divisables = [x| x<-[limit,limit-1..1], x `mod` divisable == 0] in if length divisables == 0 then 0 else head divisables 


sumOddSquares :: Integral a => a -> a
sumOddSquares limit = case limit of 0 -> 0
                                    n -> let isOddSquare x = ((x*x) `mod` 2 /= 0) 
                                             sqr x = x*x 
                                         in 
                                         sum' (filter' isOddSquare (map sqr [1..n])) 

collatzSequence :: (Show a, Integral a, Fractional a) => a -> [a]
collatzSequence n = [n]  ++ (collatzSequence (n+1))
                

findLCS :: Integral a => a -> a -> a
findLCS x 0 = x
findLCS 0 y = y
findLCS x y = findLCS y $ x `mod` y

data CMaybe c a = CJust c a | CNothing deriving (Show)
instance (Num c) => Functor (CMaybe c) where 
    fmap _f CNothing = CNothing
    fmap f (CJust x y) = CJust (x-1) (f y)

myFn = [1..50] >>= (\n -> guard (n > 7) >> return n)

myFn2 = [1,2,3] >>= (\x -> [0,1]  >>= \z -> (guard (x > 1) >> [1,2,z]) >>= (\y -> [x, y]))

myFn3 = getLine >>= (\x -> guard (x == "quit") >> return (x ++ "This is my Line") >>= (\y -> return (y ++ "Line")))

myFn4 = getLine >>= (\x -> putStrLn "Processing" >> getLine >>= (\y -> return $ reverse $ y ++ x))

whatIsMyName = putStrLn "What is your Name?" >> getLine >>= (\x -> putStrLn $ "Your name in reverse is:" ++ reverse x)

mcons :: (Monad m) => m t -> m [t] -> m [t]
mcons x y = x >>= (\p -> y >>= (\q -> return $ p:q))

mySequence :: (Monad m) => [m a] -> m [a]
mySequence = foldr mcons (return [])


addStuff :: Int -> Int  
addStuff = do 
    a <- (*2)  
    b <- (+10) 
    return (a+b)  



