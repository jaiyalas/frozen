module Main where
  import System.Random
  import ArgsDef
  import System.IO
  import System.Console.CmdArgs
  import GHC.Float
  import Graphics.Rendering.Cairo
  
  main :: IO ()
  main = do
    pargs <- cmdArgsRun mode
    case pargs of 
      RMode _rep _intv _seed _max _min _size _tar -> do
        kernel pargs
      FMode _src _intv _tar -> do
        kernel pargs
  
  
  kernel :: Dist -> IO ()
  kernel (RMode _rep _intv _seed _max _min _size _tar) = do
    setStdGen $ mkStdGen _seed
    runInRmode _intv _rep _max _min _size _tar
  kernel (FMode _intv _src _tar) = do
    runInFmode _intv _src _tar
  
  
  getFileContents :: FilePath -> IO [Double]
  getFileContents _src = do
    putStrLn ("loading data file "++_src)
    ff <- openFile _src ReadMode
    ns <- getCT ff
    hClose ff
    putStrLn ("completed for loading data file "++_src)
    return ns
  --
  getCT :: Handle -> IO [Double] 
  getCT f = do
    b <- hIsEOF f
    if (b)
      then return ([] :: [Double])
      else do
        l <- hGetLine f
        temp <- getCT f
        return ((read l):temp)
  --
  runInFmode :: Int -> FilePath -> Maybe FilePath -> IO ()
  runInFmode _intv _src _tar = do
    dt <- getFileContents _src
    let li = (ssplit _intv dt)
    let al = (init.zipp.qsort.map double2Int.map ave) li
    --v               putStrLn$show li
    putStrLn$show al
    svgConstructor 1 _intv al _tar
  
  --
  runInRmode :: Int -> Int -> Int -> Int -> Int -> Maybe FilePath-> IO ()
  runInRmode _ 0 _ _ _ _ = do return ()
  runInRmode _intv _rep _max _min _size _tar = do
    dt <- ramData _size _max _min
    let li = (ssplit _intv dt)
    let al = (init.zipp.qsort.map double2Int.map ave) li
    --putStrLn$show li
    --putStrLn$show al
    svgConstructor _rep _intv al _tar
    runInRmode _intv (_rep-1) _max _min _size _tar
    
  svgConstructor :: Int -> Int -> [(Int,Int)] -> Maybe FilePath -> IO()
  svgConstructor _rep _intv _al (Just s) = do
    let maxHR = (250.0 /)$int2Double$(foldr max 0 (map snd _al)) :: Double
    putStrLn "Now Image Creating..."
    let fp = (show _rep)++"_"++(show _intv)++"_"++s 
    withSVGSurface (fp) 
      (fromInteger 350) (fromInteger 350) (myDraw _al maxHR)
    putStrLn (fp++" was created!")
  svgConstructor _rep _intv _al Nothing = do
    let maxHR = (250.0 /)$int2Double$(foldr max 0 (map snd _al)) :: Double
    putStrLn "Now Image Creating..."
    withSVGSurface ("sample"++(show _rep)++"_"++(show _intv)++".svg") 
      (fromInteger 350) (fromInteger 350) (myDraw _al maxHR)
    putStrLn ("sample"++(show _rep)++"_"++(show _intv)++".svg was created!")
    
  
  --
  myDraw :: [(Int,Int)] -> Double-> Surface -> IO ()
  myDraw list maxHR surface = renderWith surface (myPoint list maxHR)
  
  --
  myPoint :: [(Int,Int)] -> Double -> Render ()
  myPoint [] _ = do return ()
  myPoint (l:ls) maxHR = do
    case l of 
      (v,n) -> do
        save
        setSourceRGB 0.0 0.0 0.5
        moveTo 10.0 345.0
        relMoveTo ((int2Double v)*2) 0
        relLineTo 0.0 (-1*(maxHR)*(int2Double n))
        stroke
        restore
        (myPoint ls maxHR)
  
  
  --
  ramData :: Int -> Int -> Int -> IO [Double]
  ramData 0 _ _ = do 
    return []
  ramData n smax smin = do
    num <- (getStdRandom (randomR (int2Double smin,int2Double smax))) :: IO Double
    other <- ramData (n-1) smax smin
    return (num:other)
  
  --
  qsort :: (Ord a) => [a] -> [a]
  qsort [] = []
  qsort (x:xs) = (qsort [a|a<-xs,a<x])++[x]++(qsort [a|a<-xs,a>=x])
  
  --
  zipp :: (Num a,Eq a) => [a] -> [(a,Int)]
  zipp [] = [(-1,-1)]
  zipp (x:xs) = let (t,n) = (head$zipp xs) in
    case (t == x) of
      True  -> (t,n+1):(tail$zipp xs)
      False -> (x,1):(zipp xs)
  
  --
  ssplit :: Int -> [Double] -> [[Double]]
  ssplit _ [] = []
  ssplit _intv list = (take _intv list) : (ssplit _intv (drop _intv list))
  
  --
  ave :: [Double] -> Double
  ave l = (sum l) / (int2Double (length l))
  
  -- end of file
  
  
