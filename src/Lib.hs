{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
add'' (x, y) = x + y


data Trivial = Trivial
  deriving (Eq, Show)

myAbs :: Int -> Int
myAbs x = if x < 0 then -x else x

myAbs' 2         = 2
myAbs' x | x < 0 = -x
myAbs' x         = x

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv a 0 = Nothing
safeDiv a b = Just $ a `div` b

divInfo :: Integer -> Integer -> String
divInfo a =
  maybe "not possible" (\x -> if x < 1 then "smaller" else "bigger") . safeDiv a

data Points = Point
   { pointX :: Int
   , pointY :: Int
   }

 | Point3
  { point3X :: Int
  , point3Y :: Int
  , point3Z :: Int
  }
  deriving Show


xs =
  [ "Hallo ksjhsadkjhdskajhdaskjhdskjdhaskjdashkdasjhdaskjhdsakjh"
  , "Weltsakjhadskjdhsakjdahskdsjahdsakjdaskjh"
  ]
getX :: Points -> Int
getX (Point x _) = x

setX :: Points -> Int -> Points
setX (Point _ y) x = Point x y

newtype Grade = Grade { unGrade :: Int }
  deriving Show

addGrades :: Grade -> Grade -> Grade
addGrades x y = Grade $ unGrade x + unGrade y

newtype Students = Students Int
  deriving (Headcount, Show)

newtype Staff = Staff
  { staffHeadcount :: Int
  }
  deriving (Headcount)

class Headcount a where
  headcount :: a -> Int

instance Headcount Int where
  headcount i = i

-- Monoide

appendMaybe Nothing  y        = y
appendMaybe x        Nothing  = x
appendMaybe (Just x) (Just y) = Just $ x ++ y

instance Semigroup Students where
  Students x <> Students y = Students $ x + y

instance Monoid Students where
  mempty = Students 0

data StudentGroups = StudentGroups
    { maxMembersPerGroup :: Int
    , groups :: [Int]
    }
  deriving Show

mkStudentGroups :: Int -> Students -> StudentGroups
mkStudentGroups size (Students i) =
  let first = i `mod` size
      rest  = replicate (i `div` size) size
  in  StudentGroups size $ if first /= 0 then first : rest else rest



instance Semigroup StudentGroups where
    (StudentGroups sizeX xs) <> (StudentGroups sizeY ys) =
      mkStudentGroups (max sizeX sizeY)
                     (Students $ sum xs + sum ys)

instance Monoid StudentGroups where
    mempty = StudentGroups 0 []
