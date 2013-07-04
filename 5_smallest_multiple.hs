--2520 is the smallest number that can be divided by each of the numbers from
--1 to 10 without any remainder.
--What is the smallest positive number that is evenly divisible by all of the
--numbers from 1 to 20?

result = head [x | x <- [2520..], all ((==0) . (x`mod`)) [2..20]]
main = print result