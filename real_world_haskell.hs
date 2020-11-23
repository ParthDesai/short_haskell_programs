import Data.List
import Data.Char
import Debug.Trace

lastButOne :: [a] -> a
lastButOne [] = error "Empty list" 
lastButOne [x, y] = x
lastButOne (_:xs) = lastButOne xs 

data MyList a = Nil | Cons a (MyList a) deriving Show

convertFromList :: [a] -> MyList a 
convertFromList (x:xs) = Cons x (convertFromList xs)
convertFromList [] = Nil

convertFromMyList :: MyList a -> [a]
convertFromMyList (Cons x xs) = x:(convertFromMyList xs)
convertFromMyList Nil = []

data MyTree a = Empty | Node a (MyTree a) (MyTree a)


guardHelp (Cons x _) (Cons y _) 
    | x == y = "First element is same"
    | otherwise = "First element is different"
guardHelp Nil _ = "One of list is empty"
guardHelp _ Nil = "One of list is empty"

myLength :: [a] -> Int
myLength xs | null xs = 0
            | otherwise = 1 + (myLength $ tail xs)

listMean :: [Int] -> Float
listMean [] = 0
listMean xs = fromIntegral (foldl (\x y -> x+y) 0 xs) / (fromIntegral $ length xs)

makePalindrome :: [a] -> [a]
makePalindrome x = foldr (\elem x -> x++[elem]) (init x) x

verifyPalindrome :: (Eq a) => [a] -> Bool
verifyPalindrome [] = True
verifyPalindrome (x:[]) = True
verifyPalindrome xs = if head xs == last xs then verifyPalindrome $ (tail . init) xs else False

sortListOfList :: [[a]] -> [[a]]
sortListOfList = sortBy (\x y -> compare (length x) (length y)) 

myIntersperse :: a -> [[a]] -> [a]
myIntersperse s [] = []
myIntersperse s list = foldl (\a l -> a ++ [s] ++ l) (head list) (tail list)

myIntersperse2 :: a -> [[a]] -> [a]
myIntersperse2 s [] = []
myIntersperse2 s (x:[]) = x
myIntersperse2 s list = (head list) ++ [s] ++ (myIntersperse2 s $ tail list)

treeHeight :: MyTree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)
 
data Direction = Left | Right | Straight deriving (Show, Eq)

calculateTurn :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Direction, Int)
calculateTurn (x1, y1) (x2, y2) (x3, y3) | result == 0 = (Straight, 0)
                                         | result < 0 = (Main.Right, abs result)
                                         | result > 0 = (Main.Left, abs result)
                                         where result = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

calculateTurns :: [(Int, Int)] -> [Direction]
calculateTurns points 
                | length points < 3 = []
                | otherwise = case points of (x:y:z:xs) -> [fst $ calculateTurn x y z] ++ calculateTurns (y:z:xs)

sortByY :: [(Int, Int)] -> [(Int, Int)]
sortByY = sortBy (\(x1, y1) (x2, y2) -> if y1 == y2 then if x1 > x2 then GT else LT else if y1 > y2 then GT else LT) 

sortByPolarAngle :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
sortByPolarAngle (p1, p2) = sortBy (\(x1, y1) (x2, y2) -> flip compare (fromIntegral (x1 - p1) / fromIntegral (y1 - p2)) (fromIntegral (x2 - p1) / fromIntegral (y2 - p2)))

sortByPolarAngle2 :: [(Int, Int)] -> [(Int, Int)]
sortByPolarAngle2 = sortBy calculateAngle

calculateAngle :: (Int, Int) -> (Int, Int) -> Ordering
calculateAngle (x1, x2) (y1, y2)| (y1 == 0 && x1 > 0) = LT
                                | (y2 == 0 && x2 > 0) = GT
                                | (y1 > 0 && y2 < 0) = LT
                                | (y1 < 0 && y2 > 0) = GT
                                | otherwise = ((x1 * y2) - (y1 * x2)) `compare` 0

grahamScan :: [(Int, Int)] -> [(Int, Int)]
grahamScan points = let sortedPoints = sortByY points
                        sortedByPolarAngle = sortByPolarAngle (head sortedPoints) sortedPoints
                         in foldl (manageStack) [] sortedByPolarAngle

grahamScanIncorrect :: [(Int, Int)] -> [(Int, Int)]
grahamScanIncorrect points = let sortedPoints = sortByPolarAngle2 points
                         in foldl (manageStack) [] sortedPoints

manageStack :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
manageStack stack input = if length stack > 1 then let (direction, _result) = calculateTurn (lastButOne stack) (last stack) input in if direction /= Main.Left then manageStack (init stack) input else stack ++ [input] 
                                              else stack ++ [input]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x 

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:[]) = Just []
safeInit (x:xs) = (++) <$> Just [x] <*> safeInit xs

splitsWith :: (a -> Bool) -> [a] -> [[a]]
splitsWith f list = splitsWithInternal f list [] 

splitsWithInternal :: (a -> Bool) -> [a] -> [[a]] -> [[a]]
splitsWithInternal f [] combinedList = combinedList
splitsWithInternal f (x:xs) combinedList = splitsWithInternal f xs (if f x && not (null combinedList) then (init combinedList ++ [((last combinedList) ++ [x])]) else combinedList ++ [[x]])

myFoldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
myFoldl f acc = foldr f' acc where f' i a = f a i 

-- Unfolding of myFoldl
-- foldr (\i a -> f a i) acc list
-- (\i a -> f a i) x (foldr (\i a -> f a i) acc xs)
-- (\i a -> f a i) x ((\i a -> f a i) x' (foldr (\i a -> f a i) acc xs'))
-- (\i a -> f a i) x ((\i a -> f a i) x' ((\i a -> f a i) x'' (foldr (\i a -> f a i) acc xs'')))

-- Let's consider f = (-), list = [1,2,3], acc = 1
-- (\i a -> a - i) x ((\i a -> a - i) x' ((\i a -> a - i) x'' (foldr (\i a -> a - i) acc xs'')))
-- (\i a -> a - i) 1 ((\i a -> a - i) 2 ((\i a -> a - i) 3 (foldr (\i a -> a - i) 1 [])))
-- (\i a -> a - i) 1 ((\i a -> a - i) 2 ((\i a -> a - i) 3 1))
-- (\i a -> a - i) 1 ((\i a -> a - i) 2 (-2))
-- (\i a -> a - i) 1 (-4)
-- (-5)

-- Lets contrast with normal foldr operation
-- foldr (\i a -> f i a) acc list
-- (\i a -> f i a) x (foldr (\i a -> f i a) acc xs)
-- (\i a -> f i a) x ((\i a -> f i a) x' (foldr (\i a -> f i a) acc xs'))
-- (\i a -> f i a) x ((\i a -> f i a) x' ((\i a -> f i a) x'' (foldr (\i a -> f i a) acc xs'')))

-- Let's consider f = (-), list = [1,2,3], acc = 1
-- (\i a -> i - a) x ((\i a -> i - a) x' ((\i a -> i - a) x'' (foldr (\i a -> i - a) acc xs'')))
-- (\i a -> i - a) 1 ((\i a -> i - a) 2 ((\i a -> i - a) 3 (foldr (\i a -> i - a) acc [])))
-- (\i a -> i - a) 1 ((\i a -> i - a) 2 ((\i a -> i - a) 3 (1)))
-- (\i a -> i - a) 1 ((\i a -> i - a) 2 (2))
-- (\i a -> i - a) 1 (0)
-- 1

-- In foldr If the function doesn't need the second argument, thunk for that arg won't be evaluated


-- Unfolding of foldl
-- foldl (\a i -> f a i) acc list
-- foldl (\a i -> f a i) ((\a i -> f a i) acc x) xs
-- foldl (\a i -> f a i) ((\a i -> f a i) ((\a i -> f a i) acc x) x') xs'
-- foldl (\a i -> f a i) ((\a i -> f a i) ((\a i -> f a i) ((\a i -> f a i) acc x) x') x'') xs''

-- Let's consider f = (-) list = [1,2,3] acc = 1
-- foldl (\a i -> a - i) ((\a i -> a - i) ((\a i -> a - i) ((\a i -> a - i) acc x) x') x'') xs''
-- foldl (\a i -> a - i) ((\a i -> a - i) ((\a i -> a - i) ((\a i -> a - i) 1 1) 2) 3) []
-- ((\a i -> a - i) ((\a i -> a - i) ((\a i -> a - i) 1 1) 2) 3)
-- ((\a i -> a - i) ((\a i -> a - i) ((\a i -> a - i) 1 1) 2) 3)
-- ((\a i -> a - i) ((\a i -> a - i) 0 2) 3)
-- ((\a i -> a - i) (-2) 3)
-- -5

-- Let's consider f = (+) list = [1,2,3] acc = 0
-- foldl (\a i -> a - i) ((\a i -> a - i) ((\a i -> a - i) ((\a i -> a - i) acc x) x') x'') xs''
-- foldl (\a i -> a - i) ((\a i -> a - i) ((\a i -> a - i) ((\a i -> a - i) 0 1) 2) 3) []
-- ((\a i -> a + i) ((\a i -> a + i) ((\a i -> a + i) 0 1) 2) 3)
-- ((\a i -> a + i) ((\a i -> a + i) ((\a i -> a + i) 0 1) 2) 3)
-- ((\a i -> a + i) ((\a i -> a + i) 1 2) 3)
-- ((\a i -> a + i) 3 3)
-- 6


asIntDiff :: [Char] -> Int
asIntDiff [] = 0
asIntDiff (x:xs) = (i * (round (10 ** (fromIntegral (length xs))))) + (asIntDiff xs) where i = toInt x

asIntEasy :: [Char] -> Int
asIntEasy = asIntInternal 0

asIntInternal :: Int -> [Char] -> Int
asIntInternal acc [] = acc
asIntInternal acc (x:xs) = asIntInternal ((acc * 10) + (toInt x)) xs

toInt :: Char -> Int
toInt c = case c of '1' -> 1
                    '2' -> 2
                    '3' -> 3
                    '4' -> 4
                    '5' -> 5
                    '6' -> 6
                    '7' -> 7
                    '8' -> 8
                    '9' -> 9
                    '0' -> 0
                    _ -> error "Invalid character"

tryToInt :: Char -> Either String Int
tryToInt c = case c of '1' -> Prelude.Right 1
                       '2' -> Prelude.Right 2
                       '3' -> Prelude.Right 3
                       '4' -> Prelude.Right 4
                       '5' -> Prelude.Right 5
                       '6' -> Prelude.Right 6
                       '7' -> Prelude.Right 7
                       '8' -> Prelude.Right 8
                       '9' -> Prelude.Right 9
                       '0' -> Prelude.Right 0
                       c -> Prelude.Left ("Invalid character:" ++ [c])


asIntFoldR :: [Char] -> Int
asIntFoldR =  snd . (foldr (\c (multiplier, acc) -> (multiplier*10, acc + multiplier * toInt c)) (1, 0))

asIntFoldL :: [Char] -> Int
asIntFoldL ('-':xs) = -1 * (foldl (\acc c -> (acc * 10)+(toInt c)) 0 xs)
asIntFoldL xs = foldl (\acc c -> (acc * 10)+(toInt c)) 0 xs

asIntFoldLWithError :: [Char] -> Either String Int
asIntFoldLWithError ('-':xs) = case (asIntFoldLWithError xs) of err@(Prelude.Left str) -> err
                                                                (Prelude.Right i) -> Prelude.Right (-1 * i)
asIntFoldLWithError xs = foldl (\acc c -> case (acc, tryToInt c) of (err@(Prelude.Left str), _) -> err
                                                                    ((Prelude.Right acc'), (Prelude.Right i)) -> Prelude.Right (acc'*10+i)
                                                                    (_, err@(Prelude.Left str)) -> err) 
                         (Prelude.Right 0) xs


myConcat :: [[a]] -> [a]
myConcat = foldl (\acc x -> acc ++ x) []

myConcatR :: [[a]] -> [a]
myConcatR = foldr (\x acc -> x ++ acc) []

takeWhileExplicit :: (a -> Bool) -> [a] -> [a]
takeWhileExplicit f [] = []
takeWhileExplicit f (x:xs) = if f x then x:(takeWhileExplicit f xs) else []

takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr f = foldr (\i acc -> if f i then i:acc else []) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\i acc -> if f i then i:acc else acc) []

-- for infinite list this will hang
takeWhileFoldl :: (a -> Bool) -> [a] -> [a]
takeWhileFoldl f = foldl (\acc i -> if f i then acc ++ [i] else []) []

groupByFold1 :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFold1 f = foldl (\acc i -> if null acc then [[i]] else if isPartOfGroup f (last acc) i then (init acc) ++ [(last acc)++[i]] else acc ++ [[i]]) []

groupByFold2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFold2 f list = let foldOutput = (foldr (\i (buff, acc) -> if null buff then ([i], acc) else if isPartOfGroup f buff i then (i:buff, acc) else ([i], buff:acc)) ([], []) list) in fst foldOutput:(snd foldOutput)

isPartOfGroup :: (a -> a -> Bool) -> [a] -> a -> Bool
isPartOfGroup f group = (\new -> all (\element -> f element new) group)

groupByFold3 :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFold3 f = foldl (\acc input -> if null acc then [[input]] else if f (head (last acc)) input then init acc ++ [(last acc)++[input]] else acc ++ [[input]]) [] 

-- This works for infinite list
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\input acc -> if f input then True else acc) False

-- This does not work on infinite list
myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f = foldl (\acc input -> if f input then True else acc) False

-- This works on infinite list
myAll :: (a -> Bool) -> [a] -> Bool
myAll f = foldr (\input acc -> if not (f input) then False else acc) True

-- Does not work on infinite list
myWords :: String -> [String]
myWords = snd . foldl' (\(flag, acc) input -> if input == ' ' then (True, acc) else if flag || null acc then (False, acc ++ [[input]]) else (False, (init acc) ++ [(last acc) ++ [input]])) (False, [])

-- This does not work on infinite list
myWords2 :: String -> [String]
myWords2 = foldr (\input acc ->  
                                    if null acc then
                                        [[input]]
                                    else 
                                        [input]:acc
                 ) [] 

-- Reasoning behind working with infinite list is at some point in the list evaluation, we should be able to return result
-- without evaluating entire list.
-- For example in below definition it will work on myWords3 (cycle "Hello World")
-- but not on myWords3 (cycle "HelloWorld")
-- as in the first case there is a space, so one of the guard will be true at some point and guard body does not reference gs in decision making,
-- so we do not need to evaluate further
-- in second case, there is no space in "HelloWorld" due to that it will always go to otherwise and since otherwise always refer to 
-- the gs, it cannot be calculated without evaluating gs as a whole, so it will not work for infinite list
myWords3 l = foldr step [] (zip (tail l ++ " ") l)
  where step (a, b) gs  | isSpace a = trace ("*isSpaceA:" ++ [a,b] ++ "*") (if null gs then [b]:gs else [b]:gs)
                        | isSpace b = trace ("*isSpaceB:" ++ [a, b] ++ "*") (if null gs then [a]:gs else [a]:gs)
                        | otherwise = trace ("*Otherwise:" ++ [a, b] ++ "*") (a:(head gs)):(tail gs)


-- This works on infinite list
myWords4 l = foldr step ["h"] l
  where step a gs       | isSpace a = []:gs
                        | otherwise = (a:(head gs)):(tail gs)

myCycle :: [a] -> [a]
myCycle (x:xs) = x:myCycle (xs++[x])  

-- This works with infinite list
myCycleFold :: [a] -> [a]
myCycleFold list = foldr (\x acc -> x:acc) (myCycleFold list) list

-- This does not work with infinite list
myCycleFold2 :: [a] -> [a]
myCycleFold2 list = foldr (\x acc -> x:acc) list (myCycleFold2 list)

myUnlines :: [String] -> String
myUnlines = foldr (\x acc -> x ++ "\n" ++ acc) []

myFn = length . filter (isUpper . head) . words