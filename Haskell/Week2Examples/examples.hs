-- Week 2 Haskell examples

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
