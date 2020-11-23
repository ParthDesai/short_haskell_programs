{-# LANGUAGE TypeFamilies #-}

class VM a where
    type Stack a
    type Word a

instance VM Int where
    type Stack Int = [Int]
    type Word Int = Int

instance VM Bool where
    type Stack Bool = [Bool]
    type Word Bool = Bool

