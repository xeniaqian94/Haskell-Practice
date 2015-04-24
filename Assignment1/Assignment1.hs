module Assignment1 where

import Test.QuickCheck
import Data.List
import Data.Array

-- Exercise 1
ryanGosling :: Integer -> String
ryanGosling x -- using Guards
	| x `mod` 3 == 0 && x `mod` 5 == 0 = "Ryan Gosling"
	| x `mod` 3 == 0                   = "Ryan"
	| x `mod` 5 == 0                   = "Gosling"
	| otherwise                        = show (abs x)

-- Exercise 2
prop_ryanGosling :: Integer -> Bool
prop_ryanGosling x = ryanGosling x == ryanGosling (-x)

-- Exercise 3
recRyanGosling :: [Integer] -> String 
recRyanGosling []     = "will not eat cereal" --using pattern matching
recRyanGosling (x:xs) = ryanGosling x ++ "\n" ++ recRyanGosling xs

-- Exercise 4
prop_recRyanGosling :: Integer -> [Integer] -> Property
prop_recRyanGosling n ls = (n>=0) ==> (recRyanGosling ls == recRyanGosling (negateFirstN n ls)) 
	where 
		negateFirstN :: Integer -> [Integer] -> [Integer] -- auxiliary recursive function that negates the first N integers in L
		negateFirstN 0 ls = ls -- 0 elements need to be negated
		negateFirstN _ [] = [] -- empty list, nothing need to be negated
		negateFirstN n (l:ls) = -l:(negateFirstN (n-1) ls) --negate the elements recursively

-- Exercise 5
balancerStack :: String -> Int -> Bool
balancerStack [] 0 = True  -- if the stack is empty, all brackets are matched
balancerStack [] _ = False -- if the stack is not empty, all brackets are not matched
balancerStack (x:xs) n 
	| x=='('         = balancerStack xs (n+1) -- if left bracket, push into the stack
	| x==')' && n==0 = False -- no left bracket in stack, brackets are not closed
	| x==')' && n>0  = balancerStack xs (n-1) -- if right bracket, pop one left bracket out
	| otherwise      = balancerStack xs n -- if not bracket, ignore it and continue

bracketBalancer :: String -> Bool
bracketBalancer s = balancerStack s 0 -- the stack is initially empty

-- Exercise 6
waysToReturnMoney :: Integer -> [Integer] -> Integer
waysToReturnMoney 0 _  = 1 -- no money need to change, 1 solution
waysToReturnMoney _ [] = 0 -- no coin, 0 solution
waysToReturnMoney n (x:xs) 
	| n<0                = 0 -- negative sum of money, 0 solution
	| otherwise          = waysToReturnMoney n xs + waysToReturnMoney (n-x) (x:xs)

waysToReturnMoneyDP :: Integer -> [Integer] -> Integer
waysToReturnMoneyDP n xs = ds!(n, m) -- ways to return n with m coins
  where m = length xs   
        sorted_xs = sort xs -- sort coins
        d 0 _ = 1  -- no money need to change, 1 solution
        d _ 0 = 0  -- no coin, 0 solution
        d i j      
          | i<sorted_xs!!(j-1) = ds ! (i, j-1)  -- ways to return i with only (j-1) coins
          | otherwise          = ds ! (i-(sorted_xs!!(j-1)),j) + ds ! (i,j-1) -- the Bellman equation

        ds = listArray bounds
               [d i j | (i, j) <- range bounds] -- saving computation time using memory space (listArray) 
        bounds = ((0, 0), (n, m))