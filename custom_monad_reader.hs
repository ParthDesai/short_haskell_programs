{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Reader
import Debug.Trace


instance MonadReader Int Maybe where
    local f x = Nothing
    ask = reader id
    reader f = do
      r <- ask
      return (f r)

l :: Maybe Int
l = reader (\x -> Just 1)
