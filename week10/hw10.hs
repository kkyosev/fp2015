isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = isPrime' 2 n
        where
            isPrime' current n
                | current == n = True
                | mod n current == 0 = False
                | otherwise = isPrime' (current + 1) n

digitProgList :: Int -> [Int]
digitProgList x
	|x==0 = []
	|otherwise = x:digitProgList (div x 10)

digitList::Int->[Int]
digitList x
	|x==0 = []
	|otherwise = (mod x 10):digitList (div x 10)

truncatablePrime :: Int -> Bool
truncatablePrime x = all isPrime (digitProgList x)

containsDigits :: Int -> Int -> Bool
containsDigits x y
	|y==0 = True
	|otherwise = elem (mod y 10) (digitList x) && containsDigits x (div y 10)

productOfDigits :: Int -> Int
productOfDigits x = product (digitList x)

sumDivisors :: Int->Int
sumDivisors n = sumDivisors' 1 n
	where
		sumDivisors' k n 
			|k*2>=n = k
			|mod n k ==0 =k+(sumDivisors' (k+1) n)
			|otherwise = sumDivisors' (k+1) n
			
interestingNumber :: Int -> Bool
interestingNumber x = sumDivisors x == sumDivisors (sumDivisors x)

quadrant :: Double -> Double -> Int
quadrant x y
	|x==0 && y==0 = 0
	|x>0 && y>0 = 1
	|x<0 && y<0 = 3
	|x>0 && y<0 = 4
	|otherwise = 2