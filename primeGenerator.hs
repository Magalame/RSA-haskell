import Data.Bits
import Data.Char
import System.Random

-- https://gist.github.com/trevordixon/6788535


millerExps :: (Integral a) => [a] -> [a]
millerExps [x] = if x `mod`2 == 0
           then init $ millerExps [x `div` 2, x] -- if i don't put the , it gets stuck in the first parsing situation
           else [x]  
millerExps listnb = if (head listnb) `mod` 2 == 0
               then millerExps $ ((head listnb) `div` 2):listnb
               else listnb

millerExpsWrapper :: (Integral a) => a -> [a]
millerExpsWrapper x = millerExps [x]

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
          where t = if testBit e 0 then b `mod` m else 1

millerTest :: Integer -> Integer -> [Integer] -> Bool
millerTest a n (x:exps) = if (modExp a x n) == 1
                        then True
                        else millerHelper a n (x:exps)
                        where millerHelper a n [] = False
                              millerHelper a n (x:exps) = if (modExp a x n) == n-1
                                                        then True
                                                        else millerHelper a n exps

gen :: (Num a, Random a) => a -> a -> IO [a]
gen up down = do sequence [randomRIO (down, up) | x <- [1..100]]

gen2 :: (Integral a, Random a) => a -> a -> IO [a]
gen2 up down = do sequence [randomRIO (down, up) | x <- [1..100]]

genWrapper :: Integer -> Integer -> IO [Integer]
genWrapper up down = gen2 up down

genTest :: Integer -> IO String
genTest n = getLine

displayTest n = do 
              rs <- genWrapper 1 10
              print rs
              if helperisPrime n (millerExpsWrapper (n-1)) rs
              then do 
                    putStrLn "Yes"
              else do
                    putStrLn "No"
                    
              putStrLn "Hey"

helperisPrime :: Integer -> [Integer] -> [Integer] -> Bool
helperisPrime _ _ [] = True
helperisPrime n exps (a:as) = if millerTest a n exps
                            then helperisPrime n exps as
                            else False
              

--genWrapper :: (Num a) => a -> a -> [a]

--isPrime :: (Num a) => a -> Bool
--isPrime n = do
  --          rand <- gen 1000 2000
    --        putStrLn "hey"
   --         helperisPrime n (millerExpsWrapper n) 100
     --       where helperisPrime _ _ 0 = True
       --           helperisPrime n exps count = if millerTest (rand !! (count -1)) n exps
         --                                      then helperisPrime n exps (count-1)
           --                                    else False
                                                 
