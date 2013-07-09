module Primes
(isPrime,
  primes)
where

divisors :: Integral a => a -> [a]
divisors n = filter (isDivisor) (takeWhile (>=2) [top,top-1..])
  where isDivisor = (==0) . mod n
        top = floor $ sqrt $ fromIntegral n

isPrime :: Integral a => a -> Bool
isPrime x = (==0) $ length $ divisors x

primes :: [Integer]
primes = [x | x <-  [2..], isPrime x]