import System.IO
import System.Random
import System.Environment

data Distance = Smaller | SlightlySmaller | Equal | SlightlyBigger | Bigger

main = do 
	args <- getArgs
	progName <- getProgName
	gen <- getStdGen
	let (number, _) = randomR (1, 1000) gen
	if (length args) == 0 then do
				  putStrLn $ "Syntex : " ++ progName ++ " " ++ "[Attempts]"
				  return ()
	else startGame number $ read $ head args
			   		 

startGame :: Int -> Int -> IO ()
startGame num guessCount = do
				
		putStrLn $ "I've picked up random number between 1 and 1000, your job is to guess it in " ++ show guessCount  ++ " attempts."
		playGame num guessCount guessCount
		return ()


isNearBy :: RandomGen g => Int -> Int -> g -> Bool
isNearBy num guessed gen = let (margin, _) = randomR (5, 20) gen in  (abs (num - guessed)) <= margin 

analyseNumber :: RandomGen g =>  Int -> Int -> g ->  Distance
analyseNumber num guessed gen
			  | not nearBy && (num > guessed) = Smaller
			  | nearBy && (num > guessed) = SlightlySmaller
			  | nearBy && (num < guessed) = SlightlyBigger
			  | not nearBy && (num < guessed) = Bigger
			  | otherwise = Equal
			  where nearBy = isNearBy num guessed gen   
			      
playTurn :: Int -> IO Bool
playTurn num = do
		gen <- newStdGen
		putStrLn "Please Guess a number :"
		number <- getLine
		case analyseNumber num (read number) gen of Smaller -> do
									 putStrLn "The number is smaller."
								         return False
							    SlightlySmaller -> do
									         putStrLn "The number is slighly smaller."
									         return False
						            Equal -> do
									putStrLn "Congrats !! You guessed it right."
									return True
							    SlightlyBigger -> do
									        putStrLn "The number is slighlty bigger."
									        return False
							    Bigger -> do
									putStrLn "The number is Bigger."
									return False						     

playGame :: Int -> Int -> Int -> IO Bool
playGame actual 0 total = do 
				   putStrLn "Sorry you have exceeded your attemts. You have lost!!!!"
				   return False


playGame actual remain total = do
				result <- playTurn actual 
				if result then return False else playGame actual (remain - 1) total 
								 
		
