import Data.Bits
import Data.Char
import System.Random

millerExps :: (Integral a) => [a] -> [a]

millerExps [x] = if x `mod`2 == 0

           then init $ millerExps [x `div` 2, x] -- if i don't put the x it gets stuck in the first parsing situation
           else [x]
millerExps (x:xs) = if x `mod` 2 == 0
                    then millerExps $ (x `div` 2):(x:xs)
                    else (x:xs)


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

genWrapper :: Integer -> Integer -> IO [Integer]
genWrapper up down = do sequence [randomRIO (down, up) | x <- [1..100]]

isPrime :: Integer -> IO Bool
isPrime n = do
              if (n == 1) || (n `mod` 2 == 0)
              then do
                  return False
              else do

                  rs <- genWrapper 2 (n-1)
              --print rs

                  return $ helperisPrime n (millerExpsWrapper (n-1)) rs


              --putStrLn "Hey"

helperisPrime :: Integer -> [Integer] -> [Integer] -> Bool
helperisPrime _ _ [] = True
helperisPrime n exps (a:as) = if millerTest a n exps

                              then helperisPrime n exps as
                              else False

--primeGen :: Integer -> IO Integer
--primeGen :: (Integral a, Random a) => a -> IO a
primeGen :: Integer -> Integer -> IO Integer
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

primeGen2 :: Integral t => t -> IO Integer
primeGen2 nlen = do
                  res <- randomRIO (round ((2**0.5)*(2**(((fromIntegral nlen)/2)-1))+1), round(2**((fromIntegral nlen)/2))-1)

                  if res `mod` 2 == 0
                  then primeGen2 nlen
                  else helperprimeGen res
                  where helperprimeGen x = do
                                           resisprime <- isPrime x
                                           if resisprime
                                           then return x
                                           else helperprimeGen $ x + 2


-- https://rosettacode.org/wiki/Modular_inverse#Haskell
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)

           -- Given a and m, return Just x such that ax = 1 mod m.  If there is no such x
           -- return Nothing.
modInv a m = let (i, _, g) = gcdExt a m
             in if g == 1 then Just (mkPos i) else Nothing
     where mkPos x = if x < 0 then x + m else x


createKeys :: Integral p => p -> IO (Integer, Integer, Integer)
createKeys nlen = do
                  (p,q) <- helpPQ 0 0
                  e <- helperE
                  let phi = (p-1)*(q-1)
                      m=p*q
                      bigd = modInv e phi
                      res = case bigd of Nothing -> createKeys nlen
                                         Just d -> return (e,d,m)
                  res
                  where helpPQ 0 0 = do
                                     tmpP <- primeGen2 nlen
                                     tmpQ <- primeGen2 nlen
                                     helpPQ tmpP tmpQ
                        helpPQ p q = if fromIntegral (abs (p-q)) > (2**((fromIntegral nlen)/2-100))
                                     then return (p,q)
                                     else do
                                          tmpP <- primeGen2 nlen
                                          tmpQ <- primeGen2 nlen
                                          helpPQ tmpP tmpQ
                        helperE  = do
                                   tmp <- randomRIO(round (2**16+1), round (2**256-1))
                                   if tmp `mod` 2 == 1
                                   then return tmp
                                   else helperE

main = createKeys 1024
