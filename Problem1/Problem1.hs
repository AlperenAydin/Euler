--sumOfMultiplesIn :: (Integral a) => [a] -> [a] -> a
--Finds the sum of numbers in interval which are multiples of a number in multiples
sumOfMultiplesIn interval multiples =  sum $ map f interval
    where
      -- Checks if x is a multiple of y (ie y is a factor of x)
      isMultipleOf x y        = ( x `mod` y == 0)          
      -- Checks if any number in list is a factor of x  
      isMultipleOfList x list = any (==True) $ map (x `isMultipleOf` ) list 
      f x=  if x `isMultipleOfList` multiples then x else 0
          
 
