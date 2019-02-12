-- Week 2 Haskell examples

module Week2Examples where 

-- Ex. 1
flip' :: (a -> a -> a) -> a -> a -> a
flip' f a b = f b a


-- Ex. 2
applyMultiple :: Int -> (a -> a) -> a -> a
applyMultiple 1 f b = f b
applyMultiple n f b = let c = f b
                          next = (n - 1)
                          in applyMultiple next f c


-- Ex. 3
replicate' :: Integer -> a -> [a]
replicate' 1 b = [b]
replicate' n b = let next = (n - 1)
                 in  replicate' next b ++ [b]


-- Ex. 4
allNums :: Int -> [Int]
allNums n  
    | n < 10 = [n] 
    | otherwise = [n] ++ (allnums first)
    where first = n `div` 10
	
isPrime :: Int -> Bool
isPrime n 
    | n <= 1 = False
    | otherwise = isPrime' 2 n
    where
        isPrime' current n
			| current == n = True
			| mod n current == 0 = False
			| otherwise = isPrime' (current + 1) n

truncatablePrime :: Int -> Bool
truncatablePrime n = let list = allNums n
                     in if n < 10 then isPrime n
					           else if (isPrime $ head list) == False then False
					           else truncatablePrime (head $ tail list)


-- Ex.5
filterCount pred a b = length $ filter pred [a .. b]


-- Ex. 6
allNums2 :: Int -> [Int]
allNums2 n 
    | n < 10 = [n]
    | otherwise = [(n `mod` 10)] ++ allNums2  (n `div` 10)
	
productOfDigits :: Int -> Int
productOfDigits n = product $ allNums2 n
