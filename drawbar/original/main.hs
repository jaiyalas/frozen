module Main where
    import System.Random
    import GHC.Float
    import Graphics.Rendering.Cairo
    
    total = 100000
    smax = 150
    smin = 0
    
    main :: IO ()
    main = do
        --build 10000 3
        --build 50000 3
        --build 1000 3
        build 500 1
        --build 100 3
        --build 50 3
        --build 10 2
        --build 5 2
    
    build :: Int -> Int -> IO()
    build k 0 = do return ()
    build k n = do
        dt <- ramData total
        let li = (ssplit k dt)
        let al = (reverse.tail.reverse.zipp.qsort.map double2Int.map ave) li
        --putStrLn$show li
        --putStrLn$show al
        let maxHR = (250.0 /)$int2Double$(foldr max 0 (map snd al)) :: Double
        
        putStrLn "Now Image Creating..."
        withSVGSurface ("sample"++(show k)++"_"++(show n)++".svg") (fromInteger 350) (fromInteger 350) (myDraw al maxHR)
        putStrLn ("sample"++(show k)++"_"++(show n)++".svg created!")
        build k (n-1)
    
    myDraw :: [(Int,Int)] -> Double-> Surface -> IO ()
    myDraw list maxHR surface = renderWith surface (myPoint list maxHR)
    
    myPoint :: [(Int,Int)] -> Double -> Render ()
    myPoint [] maxHR = do return ()
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
    
    qsort :: (Ord a) => [a] -> [a]
    qsort [] = []
    qsort (x:xs) = (qsort [a|a<-xs,a<x])++[x]++(qsort [a|a<-xs,a>=x])
    
    zipp :: (Num a, Eq a) => [a] -> [(a,Int)]
    zipp [] = [(-1,-1)]
    zipp (x:xs) = let (t,n) = (head$zipp xs) in
        case (t == x) of
            True  -> (t,n+1):(tail$zipp xs)
            False -> (x,1):(zipp xs)
    
    ramData :: Int -> IO [Double]
    ramData 0 = do 
        return []
    ramData n = do
        num <- (getStdRandom (randomR (smin,smax))) :: IO Double
        other <- ramData (n-1)
        return (num:other)
    
    ssplit :: Int -> [Double] -> [[Double]]
    ssplit n [] = []
    ssplit n list = (take n list) : (ssplit n (drop n list))
    
    ave :: [Double] -> Double
    ave l = (sum l) / (int2Double (length l))
    
    