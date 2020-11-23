

import Control.Applicative
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft b (l, r) = if abs (l+b-r) > 3 then Nothing else Just (l+b, r)

landRight :: Birds -> Pole -> Maybe Pole
landRight b (l, r) = if abs (r+b-l) > 3 then Nothing else Just (l, r+b)

banana :: Pole -> Maybe Pole
banana _ = Nothing

sequenceAB :: (Applicative f) => [f a] -> f [a]
sequenceAB [] = pure []
sequenceAB (x:xs) = (:) <$> x <*> sequenceAB xs  

sequenceAC :: (Applicative f) => [f a] -> f [a]
sequenceAC xs = foldr  (liftA2 (:)) (pure []) xs

applyLog :: (Int, String) -> (Int -> (Int, String)) -> (Int, String)
applyLog (gang, s) f = let (new_gang, new_log) = (f gang) in (new_gang, s ++ new_log) 

strCompare :: String -> String -> Ordering
strCompare x y = (length x `compare` length y) `mappend` (x `compare` y)             

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

instance Foldable Tree where
    foldMap _f Empty = mempty
    foldMap f (Node a leftTree rightTree) = f a `mappend` (foldMap f leftTree) `mappend` (foldMap f rightTree)

    foldr _f accumulator Empty = accumulator
    foldr f accumulator (Node a leftTree rightTree) = foldr f (f a (foldr f accumulator rightTree)) leftTree

    foldl _f accumulator Empty = accumulator
    foldl f accumulator (Node a leftTree rightTree) = foldl f (f (foldl f accumulator leftTree) a) rightTree


instance Functor Tree where
    fmap _f Empty = Empty
    fmap f (Node a leftTree rightTree) = Node (f a) (fmap f leftTree) (fmap f rightTree)

instance Applicative Tree where
    pure a = Node a (Empty) (Empty)
    (<*>) _ Empty = Empty
    (<*>) Empty _ = Empty
    (<*>) (Node fa fl fr) (Node a l r) = Node (fa a) ((<*>) fl l) ((<*>) fr r) 

instance Monad Tree where
    return a = Node a (Empty) (Empty)
    (>>=) (Empty) _f = Empty 
    (>>=) (Node a _leftTree _rightTree) f = f a

generateTree :: Tree Int
generateTree = Node 1 (Node 1 (Empty) (Empty)) (Node 2 (Empty) (Empty))

applyLogMonoid :: (Monoid m) => (a -> (b, m)) -> (a, m) -> (b, m)
applyLogMonoid f (input, log) = let (output, new_log) = f input in (output, log `mappend` new_log)

data Writer a w = Writer a w

fib :: Int -> Int
fib n 
    | n == 0 = 0
    | n > 0 = case n of 
            1 -> 1
            n 
                | n == 5 -> 4
                | n > 5 -> 50
                | otherwise -> 4



myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = (\y x-> f x y)

myOn :: (b -> b -> c) -> (a -> b) -> a -> a -> c
myOn f g = (\x y -> f (g x) (g y))

myListFn :: [Int] -> [Int]
myListFn list = myFn list where myFn (x:_xs) = x : [n | n <- [x+1, x+2]]


myUndefined :: a -> a
myUndefined = undefined


-- This yields f1 :: (Show x) => x -> String
f1 x = show x

-- But this doesn't. Instead, f2 :: () -> String
f2 = \x -> show x

-- ...but we can fix that with an explicit type signature.
f3 :: (Show a) => a -> String
f3 = \x -> show x

-- Similarly this produces () -> String
f4 = show

-- ...but this is allowed
f5 :: (Show a) => a -> String
f5 = show

foo :: _a -> _a 
foo _ = False


deSugaredApplicativeFns = (\x'' -> (\x' -> (\x y z -> x*y+z) x' ((+3) x')) x'' ((+5) x''))
