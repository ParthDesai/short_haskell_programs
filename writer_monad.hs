import Control.Monad
import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
	Writer (x, y) >>= f = let Writer (x', y') = f x in Writer (x', y `mappend` y')
	return x = Writer(x, mempty) 

tell :: (Monoid w) => w -> Writer w ()
tell x = Writer ((), x)

sumWithLog :: (Num a, Show a) => a -> a -> Writer String a
sumWithLog x y = Writer (x + y, "Summed x:" ++ show x ++ " " ++ "y:" ++ show y)

doWithWriter :: Int -> Writer String Int
doWithWriter input = do
			first <- sumWithLog 5 input
		        second <- sumWithLog 5 first
	                tell " Shhh. This is secret."
                        third <- sumWithLog 6 second
                        return (first + second + third)

			




