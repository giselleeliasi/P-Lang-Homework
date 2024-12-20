module Exercises
    ( change,
      firstThenApply,
      meaningfulLineCount,
      BST (Empty),
      size,
      insert,
      inorder,
      powers,
      Shape (Sphere, Box),
      volume,
      surfaceArea,
      contains
    ) where

import qualified Data.Map as Map
import Data.Text (pack, unpack, replace)
import Data.List (isPrefixOf, find)
import Data.Char (isSpace, toLower)


change :: Integer -> Either String (Map.Map Integer Integer)
change amount
    | amount < 0 = Left "amount cannot be negative"
    | otherwise = Right $ changeHelper [25, 10, 5, 1] amount Map.empty
  where
    changeHelper [] _ counts = counts
    changeHelper (d:ds) remaining counts =
      let (count, newRemaining) = remaining `divMod` d
          newCounts = Map.insert d count counts
      in changeHelper ds newRemaining newCounts


firstThenApply :: [a] -> (a -> Bool) -> (a -> b) -> Maybe b
firstThenApply xs pred f = fmap f (find pred xs)


powers :: Integral a => a -> [a]
powers base = map (base^) [0..]


meaningfulLineCount :: FilePath -> IO Int
meaningfulLineCount filePath = do
    content <- readFile filePath
    let linesOfFile = lines content
        isMeaningful line = not (null line) &&
                            not (all isSpace line) &&
                            not (startsWithHash (dropWhile isSpace line))
        startsWithHash line = not (null line) && head line == '#'
    return $ length (filter isMeaningful linesOfFile)



data Shape
    = Sphere Double
    | Box Double Double Double
    deriving (Eq, Show)


volume :: Shape -> Double
volume (Sphere r) = (4/3) * pi * r^3
volume (Box l w h) = l * w * h


surfaceArea :: Shape -> Double
surfaceArea (Sphere r) = 4 * pi * r^2
surfaceArea (Box l w h) = 2 * (l * w + w * h + h * l)

 
data BST a = Empty | Node a (BST a) (BST a)
    deriving (Eq) 

empty :: BST a
empty = Empty
insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x <= y    = Node y (insert x left) right   
    | otherwise = Node y left (insert x right)   

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
instance Show a => Show (BST a) where
    show Empty = "()"
    show (Node x Empty Empty) = "(" ++ show x ++ ")"
    show (Node x left Empty) = "(" ++ show left ++ show x ++ ")"
    show (Node x Empty right) = "(" ++ show x ++ show right ++ ")"
    show (Node x left right) = "(" ++ show left ++ show x ++ show right ++ ")"