{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
class MultiClass a b c | a -> b c where
    comp :: a -> b -> c -> Ordering

data MyType d e f = MyTypeCons1 d e f | MyTypeCons2 d e f


instance MultiClass Int Int Int where 
    comp x y z = GT

instance MultiClass Char Int Int where 
    comp x y z = GT

instance MultiClass (Maybe a) (Maybe a) (Maybe a) where
    comp x y z = GT

multiOperate :: (MultiClass a b c) => a -> b -> c -> Ordering
multiOperate = comp
