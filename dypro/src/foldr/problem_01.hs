module Main where
  import Data.List(sort)
  import DynamicProg(dpTable,DPTable)
  import Gen(generator,genRDomain,genRLs)
  import System.Random(mkStdGen)
  import System.Environment(getArgs)
  
  type Cell = [Int]
  
  op :: Int -> Int -> Int
  op x y = y*y - x
  
  -- dpTable wrap glue tape [cell]
  
  -- first, def the wrap :: x -> Cell
  wrap :: Int -> Cell
  wrap x = [x]
  
  -- second, def the glue :: Cell -> Cell -> Cell
  glue :: Cell -> Cell -> Cell
  glue xs ys = [xopy | x <- xs, y <- ys, 
               let xopy = x `op` y, xopy >= 0, xopy <= 255]
  
  -- third, def the tape :: Cell -> Cell -> Cell
  -- this is some kind of sorted merge
  -- suppose xs and ys are both ordered
  tape :: Cell -> Cell -> Cell
  tape [] ys = ys
  tape xs [] = xs
  tape (x:xs) (y:ys) | x >  y = y : tape (x:xs) ys
                     | x == y = tape (x:xs) ys
                     | x <  y = x : tape xs (y:ys)
  
  -- So, one may need a sorting function 
  -- which will be applied after appling glue.
  -- Fortunately, there is already a 'sort' in Data.List.
  -- However, this sorting will leave duplicates in list,
  -- that is to say that one have to define a function for de-dup.
  dedup :: Cell -> Cell
  dedup [] = []
  dedup [x] = [x]
  dedup (x:y:zs) | x == y = dedup (y:zs)
                 | otherwise = x : dedup (y:zs)
  
  -- =============
  -- = evaluator =
  -- =============
  eval :: [Int] -> DPTable Cell
  eval = dpTable wrap (\xs ys -> dedup.sort$glue xs ys) tape
  
  evalWith :: (Cell -> Int) -> [Int] -> [Int]
  evalWith f = concat . map (map f) . eval
  
  check :: [Int] -> Bool
  check = elem 128.head.head.eval
  
  -- ========================
  -- = batch tester/checker =
  -- ========================
  bEval :: [[Int]] -> [DPTable Cell]
  bEval = map eval
  
  bEvalWith :: (Cell -> Int) -> [[Int]] -> [[Int]]
  bEvalWith f = map (evalWith f)
  
  bCheck :: [[Int]] -> [Bool]
  bCheck = map check
  
  maxSpace :: [[Int]] -> [Int]
  maxSpace = map (foldr max 0).bEvalWith length
  
  testDomain :: Int -> Int -> [Int] -> [[Int]]
  testDomain len pivot = map (\x -> generator len pivot (mkStdGen x)) 
  
  test :: Int -> Int -> [Int]
  test l n = maxSpace $ testDomain l 128 [1..n]
  
  testCheck :: Int -> Int -> [Bool]
  testCheck l n = bCheck $ map (\n -> genRLs l (mkStdGen n)) [1..n]
  
  
  -- ====================================================
  -- = main function: to obtain the max length of cells =
  -- ====================================================
  main :: IO ()
  main = do
    args <- getArgs
    if(args == [])
      then printUsage
      else do
        case (head args) of
          "e" -> do
            let [a] = tail args
            let xs = read a :: [Int]
            putStrLn $ show xs
            putStrLn "----"
            putStrLn $ show $ eval xs
          "be" -> do
            let [a] = tail args
            let xss = read a :: [[Int]]
            putStrLn $ show xss
            putStrLn "----"
            putStrLn $ show $ bEval xss
          "c" -> do
            let [a] = tail args
            let xs = read a :: [Int]
            putStrLn $ show xs
            putStrLn "----"
            putStrLn $ show $ check xs
          "bc" -> do
            let [a] = tail args
            let xss = read a :: [[Int]]
            putStrLn $ show xss
            putStrLn "----"
            putStrLn $ show $ bCheck xss
          "l" -> do
            if ((length$init args) == 2)
              then printUsage 
              else do
                let [a,b] = tail args
                let l = read a
                let n = read b
                let r = test l n
                putStr.show$foldr max 0 r
                putStr " < "
                putStrLn.show $ r
          "g" -> do
            let [a] = tail args
            let n = read a
            let dom = genRDomain n (mkStdGen 1)
            putStrLn $ show dom
          "bg" -> do
            let [a] = tail args
            let n = read a
            let dom = genRDomain n (mkStdGen 1)
            putStrLn $ show dom
            putStrLn "----"
            putStrLn $ show $ bCheck $ map tail dom
          "x" -> do
            let [a,b] = tail args
            let l = read a
            let n = read b
            putStrLn $ show $ testCheck l n
          otherwise -> do printUsage
  
  printUsage = do
    putStrLn "Usage:"
    putStrLn "e <[Int]>: eval the given <list>"
    putStrLn "be <[[Int]]>: eval the given <list of list>"
    putStrLn "c <[Int]>: check the given <list>"
    putStrLn "bc <[[Int]]>: check the given <list of list>"
    putStrLn "g <Int>: generator <num> samples"
    putStrLn "bg <Int>: generator <num> samples and eval result"
    putStrLn "l <l:Int> <n:Int>: to obtain a prosible max-requirement"
    putStrLn "                   with length=2^l and number=n"
    
    