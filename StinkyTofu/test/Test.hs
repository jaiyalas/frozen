
module Main where

import StinkyTofu.Core.Evaluator
import StinkyTofu.Core.Syntax
import StinkyTofu.Jason.Generator (generateWithEvalResult)

import System.Random (getStdGen, RandomGen)
import StinkyTofu.Utils

import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, assertBool)

import Data.Word

main :: IO ()
main = do
  stdGen <- getStdGen
  defaultMain [testGenerator, testHex2Word64, testSize, testEval,
               testGenArguments stdGen]

testHex2Word64 :: Test
testHex2Word64 = testCase "hex2Word64" $ do
  assertEqual "L19"  67553994410557439 (hex2Word64 "0xEFFFFFFFFFFFFF")
  assertEqual "L20"                  1 (hex2Word64 "0x0001")
  assertEqual "L21" 0x0000FFFFFFFFFFFE (hex2Word64 "0x0000FFFFFFFFFFFE")

testGenerator :: Test
testGenerator = testCase "Generator" $ do
  assertEqual "Simple" [simpleExp] simple
    where simple = generateWithEvalResult [Not, Shl1, Shr1, Shr4, Shr16]
                                          [And, Or, Xor, Plus] 5
                                          simpleExamples

testSize :: Test
testSize = testCase "size" $ do
  assertEqual "size" 4 (size simpleExp)

testEval :: Test
testEval = testCase "eval" $
  mapM_ (\(argument, expected) ->
          let result = eval (simpleEnv argument) simpleExp
          in  assertEqual "testEval" expected result) simpleExamples

simpleExp :: Exp
simpleExp = BiOp Plus (l "x") (UnOp Shr16 (l "x"))

simpleExamples :: [(Word64, Word64)]
simpleExamples =
  [ (0x0000000000000001, 0x0000000000000001)
  , (0x0000000000000010, 0x0000000000000010)
  , (0x0000000000000000, 0x0000000000000000)
  , (0xFFFFFFFFFFFFFFFF, 0x0000FFFFFFFFFFFE) ]

simpleEnv :: Word64 -> Env
simpleEnv x = [(VName "x", x)]

testGenArguments :: RandomGen g => g -> Test
testGenArguments g = testCase "genArguments" $ do
  mapM_ (\arg -> assertBool "testGenArguments"
                   (arg > 0 && arg < 0xFFFFFFFFFFFFFFFF)) args
  where args = fst (genArguments 100 g)
