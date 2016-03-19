module Gen where
  
  import System.Random
  
  maxValidNumber = 255
  
  op :: Int -> Int -> Int 
  op x y = y*y - x
  
  validTest :: Int -> Bool
  validTest n = n >= 0 && n <= maxValidNumber
  
  domain = [0..maxValidNumber]
  
  super = [(x,y) | x <- domain, y <- domain]
  
  inop n = [(x,y) | x <- domain, y <- domain, op x y == n]
  
  
  generator :: RandomGen g => Int -> Int -> g -> [Int]
  generator 0 n _ = [n]
  generator m n currentStdGen = 
    let ((lseed,rseed),nextStdGen) = expStep currentStdGen n
        lhs = generator (m-1) lseed nextStdGen
        rhs = generator (m-1) rseed nextStdGen
    in lhs++rhs
  
  expStep :: RandomGen g => g -> Int -> ((Int,Int),g)
  expStep currentStdGen n = 
    let predicate = inop n
        plen = length predicate
        (loc,nextStdGen) = rGen currentStdGen plen
    in ((inop n) !! loc,nextStdGen)
  
  rGen :: RandomGen g => g -> Int -> (Int,g)
  rGen g plen =
    let (rv,g') = randomR (0,20) g in 
    if rv >= plen then rGen g' plen else (rv,g')
  
  genRDomain :: RandomGen g => Int -> g -> [[Int]]
  genRDomain 0 _ = []
  genRDomain n g = 
    let (v,g') = randomR (1,10) g 
        (l,g2) = randomR (4,7) g'
    in if (even (v :: Int))
      then (2^l : (generator l 128 g2)) : genRDomain (n-1) g2
      else let (ll,g3) = randomR (15,30) g2 in
        (ll : (genRLs ll g3)) : genRDomain (n-1) g3
  
  genRLs :: RandomGen g => Int -> g -> [Int]
  genRLs 0 _ = []
  genRLs l g = let (v, g') = (randomR (0,255) g) in 
    v : genRLs (l-1) g'
  
  
