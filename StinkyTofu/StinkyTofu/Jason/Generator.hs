{- | Author: 'Chi-En Wu' https://github.com/jason2506 -}

module StinkyTofu.Jason.Generator where

import Data.Word
import Control.Applicative

import StinkyTofu.Core.Syntax
import StinkyTofu.Core.Evaluator

generateByUnOps :: [Op1] -> Int -> [[Exp]] -> [Exp]
generateByUnOps unOps n exps
    | n < 3     = []
    | otherwise = (map UnOp unOps) <*> head exps

generateByBiOps :: [Op2] -> Int -> [[Exp]] -> [Exp]
generateByBiOps biOps n exps
    | n < 4     = []
    | otherwise = concat $ map f range
        where f m = (map BiOp biOps) <*> (exps !! (n - m - 2)) <*> (exps !! m)
              range = [1 .. ((n - 2) `quot` 2)]

generateByIf :: Int -> [[Exp]] -> [Exp]
generateByIf n exps
    | n < 5     = []
    | otherwise = concat [f p q | p <- [2 .. n - 3], q <- [n - p - 1 .. n - 3]]
        where f p q = If <$> (exps !! p) <*> (exps !! q) <*> (exps !! (2 * n - p - q - 4))

generate :: [Op1] -> [Op2] -> Int -> [[Exp]]
generate unOps biOps n
    | n < 2     = []
    | n == 2    = [[Zero, Unit, l "x"]]
    | otherwise = (unExps ++ biExps):exps
        where unExps = generateByUnOps unOps n exps
              biExps = generateByBiOps biOps n exps
              -- ifExps = generateByIf n exps
              exps = generate unOps biOps (n - 1)

construct :: [Op1] -> [Op2] -> Int -> [Exp]
construct unOps biOps = (filter hasVar) . head . (generate unOps biOps)

evalX :: Word64 -> Exp -> Word64
evalX x = eval [(VName "x", x)]

filterWithEvalResult :: (Word64, Word64) -> [Exp] -> [Exp]
filterWithEvalResult (input, output) = filter ((== output) . (evalX input))

generateWithEvalResult :: [Op1] -> [Op2] -> Int -> [(Word64, Word64)] -> [Exp]
generateWithEvalResult unOps biOps n ios = foldl (flip filterWithEvalResult) conds ios
    where conds = construct unOps biOps n

main :: IO ()
main = do
    print $ generateWithEvalResult [Not, Shl1, Shr1, Shr4, Shr16] [And, Or, Xor, Plus] 5
          [ (0x0000000000000001, 0x0000000000000001)
          , (0x0000000000000010, 0x0000000000000010)
          , (0x0000000000000000, 0x0000000000000000)
          , (0xFFFFFFFFFFFFFFFF, 0x0000FFFFFFFFFFFE) ]
