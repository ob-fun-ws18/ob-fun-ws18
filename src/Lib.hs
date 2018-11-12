module Lib where

someFunc :: IO ()
someFunc = putStrLn "Super Funktion"

(+?!%$) x y = x + y

add' x y = x + y

f :: Num a => a -> a
f x = x + y + z
 where
  y = 7
  z = 19

bigNumber :: (Ord x, Num x) => x -> x -> String
bigNumber x y = if x > 1000 || y /= x then "biiiiiggg" else "small"


data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sunday
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

myFavDay :: Day -> Bool
myFavDay Sat = True

type DayOfWeek = Int
data Date = Date DayOfWeek Day


f' :: (Double, Double) -> Bool
f' _ = True

t :: Num a => (a, a)
t = (2, 3)

-- tupFun, tupFun' :: (Int, String) -> (Int, String) -> Int
tupFun' t s = fst t + fst s

tupFun t@(a, _) (c, _) = if a < c then (a + c, "a war kleiner") else t

fst4 :: (Int, b, c, d) -> Int
fst4 (x, _, _, _) = x + 2

add'' :: Num a => (a, a) -> a
add'' (x,y) = x + y


data Trivial = Trivial
  deriving (Eq, Show)

myAbs :: Int -> Int
myAbs x = if x < 0 then -x else x

myAbs' 2 = 2
myAbs' x
  | x < 0 = -x
myAbs' x = x
