import Data.Bits
import Data.Char
import System.Random




millerExps :: (Integral a) => [a] -> [a]
millerExps [x] = if x `mod`2 == 0
           then init $ millerExps [x `div` 2, x] -- if i don't put the ,x it gets stuck in the first parsing situation
           else [x]  
millerExps listnb = if (head listnb) `mod` 2 == 0
               then millerExps $ ((head listnb) `div` 2):listnb
               else listnb

millerExpsWrapper :: (Integral a) => a -> [a]
millerExpsWrapper x = millerExps [x]


---- https://gist.github.com/trevordixon/6788535
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


genRandom :: (Integral a, Random a) => a -> a -> IO [a]
genRandom up down = do sequence [randomRIO (down, up) | x <- [1..100]]

genWrapper :: Integer -> Integer -> IO [Integer]
genWrapper up down = genRandom up down

isPrime :: Integer -> IO Bool
isPrime n = do 
              if n == 1
              then do
                  return False
              else do

                  rs <- genWrapper 2 (n-1)
              --print rs
                  if helperisPrime n (millerExpsWrapper (n-1)) rs
                  then do 
                       return True
                  else do
                        return False
                    
              --putStrLn "Hey"

helperisPrime :: Integer -> [Integer] -> [Integer] -> Bool
helperisPrime _ _ [] = True
helperisPrime n exps (a:as) = if millerTest a n exps
                            then helperisPrime n exps as
                            else False
              
--primeGen :: Integer -> IO Integer
--primeGen :: (Integral a, Random a) => a -> IO a 
primeGen down up = do 
                   res <- randomRIO (down,up)
                  -- print res
                   if res `mod` 2 == 0
                   then primeGen down up
                   else helperprimeGen res
                   where helperprimeGen x = do
                                            resisprime <- isPrime x
                                            if resisprime
                                            then return x
                                            else helperprimeGen $ x + 2


--randomRIO (round ((2**0.5)*(2**(((fromIntegral nlen)/2)-1))+1), round(2**((fromIntegral nlen)/2))-1)
                --print res
                --return nb             

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
                                                 
