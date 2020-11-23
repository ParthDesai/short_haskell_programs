{-# LANGUAGE TypeFamilies #-}

data family F a b :: * -> * 

data instance F Char [Int] = AEither Int Char      
data instance F Char [Int] Bool = BEither (Char, [Int]) Bool 
data instance F IO Bool = CEither (IO Bool)    
data instance F Bool  = DBool           

