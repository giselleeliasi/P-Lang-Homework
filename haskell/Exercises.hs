module Exercises
    ( change,
      BST,
      empty,
      insert,
      lookupBST,
      count,
      inorder,
      showTree,
      firstThenApply,  -- Export this function
      lower,           -- Add this line to export lower
      lengthOverThree  -- Add this line to export lengthOverThree
    ) where   

import qualified Data.Map as Map
import Data.Text (pack, unpack, replace)
import Data.List(isPrefixOf, find)
import Data.Char(isSpace, toLower)  -- Import toLower
import System.IO (readFile)


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

-- Write your first then apply function here


firstThenApply :: [a] -> (a -> Bool) -> (a -> b) -> Maybe b
firstThenApply xs p f = f <$> find p xs
lower :: String -> String
lower = map toLower

lengthOverThree :: String -> Bool
lengthOverThree str = length str > 3

-- Write your infinite powers generator here

powers :: Integral a => a -> [a]
powers base = iterate (* base) 1

-- Write your line count function here
meaningfulLineCount :: FilePath -> IO Int
meaningfulLineCount path = do
    content <- readFile path
    return $ length $ filter isMeaningfulLine (lines content)
isMeaningfulLine :: String -> Bool
isMeaningfulLine line =
    not (null trimmed) && head trimmed /= '#'
  where
    trimmed = dropWhile isSpace line
-- Write your shape data type here
data Shape
    = Box Double Double Double  -- Box with width, height, depth
    | Sphere Double             -- Sphere with radius
    deriving (Show, Eq)

surfaceArea :: Shape -> Double
surfaceArea (Box w h d) = 2 * (w * h + h * d + d * w)
surfaceArea (Sphere r)  = 4 * pi * r^2

volume :: Shape -> Double
volume (Box w h d) = w * h * d
volume (Sphere r)  = (4/3) * pi * r^3

-- Write your binary search tree algebraic type here
data BST a = Empty
           | Node a (BST a) (BST a)
           deriving (Eq, Show)

-- Create an empty tree
empty :: BST a
empty = Empty

-- Insert a value into the tree, maintaining the BST property
insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | x > y     = Node y left (insert x right)
    | otherwise = Node y left right  -- No duplicates allowed

-- Lookup a value in the tree
lookupBST :: Ord a => a -> BST a -> Bool
lookupBST _ Empty = False
lookupBST x (Node y left right)
    | x < y     = lookupBST x left
    | x > y     = lookupBST x right
    | otherwise = True

-- Count the number of elements in the tree
count :: BST a -> Int
count Empty = 0
count (Node _ left right) = 1 + count left + count right

-- Perform an inorder traversal (returns elements in sorted order)
inorder :: BST a -> [a]
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

-- Show the tree structure as a string (for TypeScript and Haskell)
showTree :: Show a => BST a -> String
showTree Empty = "Empty"
showTree (Node x left right) =
    "Node " ++ show x ++ " (" ++ showTree left ++ ") (" ++ showTree right ++ ")"
