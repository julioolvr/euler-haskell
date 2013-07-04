--The prime factors of 13195 are 5, 7, 13 and 29.
--What is the largest prime factor of the number 600851475143 ?

divisors :: Integral a => a -> [a]
divisors n = filter (isDivisor) (takeWhile (>2) [top,top-1..])
  where isDivisor = (==0) . mod n
        top = floor $ sqrt $ fromIntegral n

isPrime :: Integral a => a -> Bool
isPrime x = (==0) $ length $ divisors x

result = head $ filter isPrime (divisors 600851475143)

main = print result