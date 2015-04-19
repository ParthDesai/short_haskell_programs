import Control.Monad
import Data.List

isValidMove :: (Int,Int) -> Bool
isValidMove (r,c) = r >= 1 && r <= 8 && c >= 1 && c <= 8

calculateMove :: (Int, Int) -> [(Int, Int)]
calculateMove m@(r,c) = guard (isValidMove m) >> [(r + 2, c + 1), (r - 2, c + 1), (r + 2, c - 1) , (r - 2, c - 1), (r + 1, c + 2), (r - 1, c + 2), (r + 1, c - 2), (r - 1, c - 2)]

calculateMoves :: (Int,Int) -> [(Int, Int)]
calculateMoves m = nub $ filter (isValidMove) $ (return m) >>= calculateMove >>= calculateMove >>= calculateMove 






