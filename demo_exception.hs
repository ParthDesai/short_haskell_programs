import System.Environment
import System.IO
import System.IO.Error

main = catchIOError tryTo handler
	

tryTo :: IO () 
tryTo = do
	 args <- getArgs
	 contents <- readFile (args !! 0)
	 putStrLn $ "This file has :" ++ show (length (lines contents)) ++ " lines."

handler :: IOError -> IO ()
handler e 
	| isDoesNotExistError e = case ioeGetFileName e of (Just path) -> putStrLn $ "Path :" ++ path ++ " " ++ "does not exists."
						           Nothing -> putStrLn "Unknown path!!!"
	| otherwise = putStrLn "Whooops"
