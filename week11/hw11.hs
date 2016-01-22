listToNumber :: [Int] -> Int
listToNumber l = listToNumber' 0 l
	where
		listToNumber' res [] = res
		listToNumber' res (x:xs) = listToNumber' (10*res+x) xs

listEq::(Eq a) => [a] -> [a] -> Bool
listEq [] [] = True
listEq [] _ = False
listEq _ []= False
listEq (x:xs) (y:ys) = (x==y) && (listEq xs ys)

suffix :: (Eq a) => [a] -> [a] -> Bool
suffix [] _ = True
suffix xs ys = listEq (reverse xs) (take (length xs) (reverse ys))

occurrences :: [Int] -> [Int] -> [Int]
occurrences [] _ = []
occurrences (x:xs) l = ((length $ filter (==x) l):occurrences xs l)

removeAt :: Int -> [a] -> [a]
removeAt k l
	|k<0 = error "Index must be non-negative!"
	|k>=length l = error "List is shorter than exoected!"
	|otherwise = take k l ++ drop (k+1) l