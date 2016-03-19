module StinkyTofu.Utils where

import qualified Data.Word as W
import qualified Numeric as N

import System.Random (RandomGen, randomR)

-- hex2Word64 - hexadecimal parser from [Char] to Word64
{- | Author: 'Fumin' https://github.com/fumin -}
--
-- Input:  "0xEFFFFFFFFFFFFF"
-- Output: 67553994410557439
hex2Word64 :: [Char] -> W.Word64
hex2Word64 h = fst $ head $ N.readHex $ drop 2 h

genArguments :: RandomGen g => Int -> g -> ([W.Word64], g)
genArguments 0 g = ([], g)
genArguments i g =
  let (w , g' ) = randomR range g
      (ws, g'') = genArguments (i - 1) g'
  in (w : ws, g'')
  where range = (0, 0xFFFFFFFFFFFFFFFF)

-- | It will insert first parameter between element of second parameter, list.
--   input 1 [2,3]
--   output [[1,2,3],[2,1,3],[2,3,1]]
interval :: a -> [a] -> [[a]]
interval x [] = [[x]]
interval x ls@(y:ys) = (x:ls):(map (y:) (interval x ys))

-- | It will get all of permutations
--   input [1,2,3]
--   output [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
permutation :: [a] -> [[a]]
permutation [] = [[]]
permutation (x:xs) = concat $ map (interval x) (permutation xs)
