addVector :: (Integral a, Integral b) => (a, b) -> (a, b) -> (a, b)
addVector (c, d) (c', d') = (c + c',d + d') 

getLenth :: (Integral b) =>  [a] -> b
getLenth [] = 0
getLenth (x:rList) = 1 + getLenth rList

bmiTell :: (Num a, Ord a, Fractional a) => a -> a -> String
bmiTell height  weight  
     | weight / height ^ 2 <= 18.5 = "You are underweight"
     | weight / height ^ 2 <= 25.5 = "You are normal"
     | weight / height ^ 2 <= 30.5 = "You are overweight"
     | otherwise = "You are ugly" 


max' :: (Ord a) => a -> a -> a
max' x y 
    | x >= y = x
    | otherwise = y 
