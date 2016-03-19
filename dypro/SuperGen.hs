module SuperGen where
  import System.Random
  
  maxValidNumber = 255
  
  op :: Int -> Int -> Int 
  op x y = y*y - x
  domain = [0..maxValidNumber]
  inop n = [[x,y] | x <- domain, y <- domain, op x y == n]
  
  steepR seed 0 = [[seed]]
  steepR seed n = [ (l:ys) | [l,r] <- inop seed, ys <- steepR r (n-1)]
  
  steepL seed 0 = [[seed]]
  steepL seed n = [ (ys++[r]) | [l,r] <- inop seed, ys <- steepL l (n-1)]
  
  -- 
  fulltree seed [] = [[seed]]
  fulltree seed (x:xs) 
    | even x = [ (l:ys)    | [l,r] <- inop seed, ys <- fulltree r xs]
    | odd  x = [ (ys++[r]) | [l,r] <- inop seed, ys <- fulltree l xs]
  