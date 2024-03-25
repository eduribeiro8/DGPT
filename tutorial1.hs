module Main where

soma10 :: Int -> Maybe Int
soma10 x 
    | x >= 10 = Just (x + 10)
    | otherwise = Nothing

metade :: Int -> Maybe Int
metade x = Just (x `div` 2)

foo :: Maybe Int
foo = Just 3  >>= (\x ->
      Just 10 >>= (\y ->
      Just (x + y)))

foo' :: Maybe Int
foo' = do
    x <- Just 3
    y <- Just 10
    Just (x + y)

faztudo :: Maybe Int
faztudo = do
    x <- Just 20
    y <- soma10 x
    z <- soma10 y
    Nothing
    k <- metade z
    j <- metade k    
    metade j

main :: IO ()
main = do
    print "oi"