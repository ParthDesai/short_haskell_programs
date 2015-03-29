import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import System.Directory
import System.Environment
import Control.Monad


main = do
	args <- getArgs
	prog <- getProgName
	if length args < 2 then putStrLn $ "Syntex : " ++ prog ++ " [Source-File] [Destination-File]"
	else mycopyFile (args !! 0) (args !! 1)
	      



mycopyFile :: String -> String -> IO ()
mycopyFile source dest =  do
		lbs <- LB.readFile source
		let chunks = LB.toChunks lbs
		writeData dest chunks


writeData :: String -> [SB.ByteString] -> IO ()
writeData file chunks = do
		        forM_ chunks $ SB.writeFile file
			return () 
	      
	      

		

