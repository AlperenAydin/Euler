sumOfSqr :: [Int] -> Int
sumOfSqr list = sum sqr_list
    where 
      sqr_list = map (^2) list


sqrOfSum :: [Int] -> Int
sqrOfSum list = s ^ 2
    where s= sum list


answer = sqrOfSum [1..100]- sumOfSqr [1..100]
