{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Debug.Trace


type Stack = [Int]
type Output = [Int]
type Program = [InStr]

type VM a = ReaderT Program (WriterT Output (State Stack)) a

data InStr = Push Int | Pop | Increment | Decrement | Print deriving Show

newtype Comp a = Comp {runComp :: VM a} deriving (Functor, Applicative, Monad, MonadReader Program, MonadWriter Output, MonadState Stack)


eval :: ReaderT Program (WriterT Output (StateT Stack Identity)) Int
eval = ask >>= flip processInstructions 0

processInstructions ::  [InStr] -> Int -> ReaderT Program (WriterT Output (StateT Stack Identity)) Int
processInstructions instructions returnCode = case instructions of [] -> return returnCode
                                                                   (Pop:rest) -> modify (\stack -> init stack) >>= (\_ -> get) >>= (\stack -> return $ last stack) >>= (processInstructions rest)
                                                                   ((Push i):rest) -> modify (\stack -> stack ++ [i]) >>= (\_ -> get) >>= (\stack -> return $ last stack) >>= (processInstructions rest)
                                                                   (Print:rest) -> get >>= (\stack -> writer (last stack, [last stack])) >>= (processInstructions rest)
                                                                   (Increment:rest) -> modify (\stack -> let l = last stack in (init stack) ++ [l+1]) >>= (\_ -> get) >>= (\stack -> return $ last stack) >>= (processInstructions rest)
                                                                   (Decrement:rest) -> modify (\stack -> let l = last stack in (init stack) ++ [l-1]) >>= (\_ -> get) >>= (\stack -> return $ last stack) >>= (processInstructions rest)
                                                      



execVM :: Program -> Output
execVM = runIdentity . flip evalStateT [] . execWriterT . runReaderT eval


                        


