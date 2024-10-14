module Exercises
    ( change,
      BST(..),  -- Export the BST type and its constructors
      insert,
      firstThenApply,
      powers,
      Shape(..),
      volume,
      surfaceArea,
      is_approx,
      meaningfulLineCount,
      size,
      contains,
      empty,
      inorder,
    ) where


import qualified Data.Map as Map
import Data.Text (pack, unpack, replace)
import Data.List(isPrefixOf, find)
import Data.Char(isSpace, toLower)
import System.IO (readFile)

-- Approximate equality function for Double
is_approx :: Double -> Double -> Bool
is_approx x y = abs (x - y) < 1e-10

change :: Integer -> Either String (Map.Map Integer Integer)
change amount
    | amount < 0 = Left "amount cannot be negative"
    | otherwise = Right $ changeHelper [25, 10, 5, 1] amount Map.empty
        where
          changeHelper [] remaining counts = counts
          changeHelper (d:ds) remaining counts =
            changeHelper ds newRemaining newCounts
              where
                (count, newRemaining) = remaining `divMod` d
                newCounts = Map.insert d count counts

firstThenApply :: [a] -> (a -> Bool) -> (a -> b) -> Maybe b
firstThenApply xs p f = f <$> find p xs

lower :: String -> String
lower = map toLower

lengthOverThree :: String -> Bool
lengthOverThree str = length str > 3

powers :: Integral a => a -> [a]
powers base = iterate (* base) 1

meaningfulLineCount :: FilePath -> IO Int
meaningfulLineCount path = do
    content <- readFile path
    return $ length $ filter isMeaningfulLine (lines content)

isMeaningfulLine :: String -> Bool
isMeaningfulLine line =
    not (null trimmed) && head trimmed /= '#'
  where
    trimmed = dropWhile isSpace line

data Shape
    = Box Double Double Double
    | Sphere Double
    deriving (Show, Eq)

surfaceArea :: Shape -> Double
surfaceArea (Box w h d) = 2 * (w * h + h * d + d * w)
surfaceArea (Sphere r)  = 4 * pi * r^2

volume :: Shape -> Double
volume (Box w h d) = w * h * d
volume (Sphere r)  = (4/3) * pi * r^3

data BST a = Empty | Node a (BST a) (BST a)
    deriving (Eq) -- Remove the `Show` derivation here


empty :: BST a
empty = Empty


insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x < y     = Node y (insert x left) right
  | x > y     = Node y left (insert x right)
  | otherwise = Node y left right -- If x is equal to y, do nothing


lookupBST :: Ord a => a -> BST a -> Bool
lookupBST _ Empty = False
lookupBST x (Node y left right)
    | x < y     = lookupBST x left
    | x > y     = lookupBST x right
    | otherwise = True
size :: BST a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

contains :: Ord a => a -> BST a -> Bool
contains _ Empty = False
contains x (Node y left right)
    | x < y     = contains x left
    | x > y     = contains x right
    | otherwise = True

count :: BST a -> Int
count Empty = 0
count (Node _ left right) = 1 + count left + count right

inorder :: BST a -> [a]
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

-- showTree :: Show a => BST a -> String
-- showTree Empty = "()"
-- showTree (Node x Empty Empty) = "(" ++ show x ++ " () ()" ++ ")"
-- showTree (Node x left right) = 
--     "(" ++ show x ++ " " ++ showTree left ++ " " ++ showTree right ++ ")"
instance Show a => Show (BST a) where
    show Empty = "()"
    show (Node x Empty Empty) = "(" ++ show x ++ ")"
    show (Node x left right) = "(" ++ show left ++ show x ++ show right ++ ")"
