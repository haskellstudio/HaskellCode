{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import qualified Data.DList as DL 



data Die = 
    DieOne
   |DieTwo
   |DieThree
   |DieFour
   |DieFive
   |DieSix
   deriving (Eq, Show)




intToDie :: Int -> Die
intToDie n = 
    case n of 
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        x -> error $ "intToDie got non 1-6 interger: " ++ show x
        

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do 
    let s = mkStdGen 0
        (d1, s1) = randomR(1, 6) s
        (d2, s2) = randomR(1, 6) s1
        (d3, _)  = randomR(1, 6) s2
    (intToDie d1, intToDie d2, intToDie d3)
    

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR(1, 6) 
    return (intToDie n, s)
 

--main :: IO()
--main = print "hi"


rollDie' :: State StdGen Die
rollDie' = 
    intToDie <$> state (randomR (1, 6))


rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = 
    liftA3 (, ,) rollDie' rollDie' rollDie'
    
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie'

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie'







rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
    where go :: Int -> Int -> StdGen -> Int
          go sum count g 
              | sum >= 20 = count
              | otherwise = 
                  let (die, nxtGen) = randomR (1, 6) g 
                  in go (sum + die) (count + 1) nxtGen
                  
{--
rollsToGetTwenty' :: State StdGen (Int, Int)
rollsToGetTwenty' = do
    get
--}
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g   = go 0 0 g
    where go :: Int -> Int -> StdGen -> Int
          go sum count gen 
            | sum >= n = count 
            | otherwise = 
                let (die, nxtg) = randomR(1, 6) gen
                in go (sum + die) (count+1) nxtg
                

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 (0, [])
    where go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
          go sum (count, ds) g 
            | sum >= n = (count, ds)
            | otherwise = 
                let (die, nxtGen) = randomR(1, 6) g 
                in go (sum + die) (count + 1, intToDie die :ds) nxtGen



                
newtype Sta s a = 
    Sta { runSta :: s -> (a, s)}
    

instance Functor (Sta s) where 
    fmap :: (a -> b) -> Sta s a -> Sta s b 
    fmap f (Sta sf) =
                      Sta $ \x ->  let (result, resultState) = sf x
                                   in (f result, resultState)
                            

                           
instance Applicative (Sta s) where
    pure :: a -> Sta s a 
    pure a = Sta $ \s -> (a, s)
    
    -- <*> :: Sta s ( a -> b) -> Sta s a -> Sta s b
    (Sta f) <*> (Sta a) = Sta $ \s -> let (fv, sf) = f s
                                          (y, sa) = a sf
                                      in  (fv y , sa)
     
                                        
    
                               
instance Monad (Sta s) where 
    return = pure
    
    (>>=) :: Sta s a  
             -> (a -> Sta s b)
             -> Sta s b
             
    (Sta f) >>= g = Sta $ \s -> let (v1, s1) = f s
                                in  runSta (g v1) s1
                                

get_ :: Sta s s
get_ = Sta $ \s -> (s, s)

put_ :: s -> Sta s ()
put_ s = Sta $ \x -> ((), s)
       --Sta $ const ((), s) -- 也可以用const.
       
exec_ :: Sta s a -> s ->s 
exec_ (Sta sa) s = let (a, s1) = (sa s)
                   in  s1

eval :: Sta s a -> s -> a 
eval (Sta sa) = \s -> fst (sa s)
                    
modify_ :: (s -> s) -> Sta s ()
modify_ = \f -> Sta $ \s -> ((), f s)




fizzBuzz :: Integer -> String
fizzBuzz n 
    | n `mod` 15 == 0 = "fizzBuzz"
    | n `mod` 5  == 0 = "Buzz"
    | n `mod` 3  == 0 = "Fizz"
    | otherwise = show n 
    

fizzBuzzList :: [Integer] -> [String] 

fizzBuzzList list = 
    execState (mapM_ addResult list) []




addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)
    


--fizzBuzzList' :: [Integer] -> [String]
--fizzBuzzList' list = 
--    let dlist = execState (mapM_ addResult' list) DL.empty
--    in DL.apply dlist []
    


addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
    xs <- get 
    let result = fizzBuzz n
    put (DL.snoc xs result)



fizzBuzzList' :: [Integer] -> DL.DList String
fizzBuzzList' list = 
    execState (mapM_ addResult' list) DL.empty
