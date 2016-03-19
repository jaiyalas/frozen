module DynamicProg where
  
  import Control.Monad -- merely for the test
  
  
  foldl' f z []     = z
  foldl' f z (x:xs) = let z' = z `f` x 
                      in seq z' $ foldl' f z' xs
  
  
  type DPColumn a = [a]
  type DPTable a = [DPColumn a]
  
  dpTable :: -- trans a seed into dpCell
             (seed -> dpCell) -> 
             -- generate a newer dpCell from two olders
             (dpCell -> dpCell -> dpCell) ->
             -- pick the best choise from two candidates
             (dpCell -> dpCell -> dpCell) ->
             [seed] -> DPTable dpCell
  dpTable wrap glue tape = foldl' (oneStep wrap glue tape) []
  
  oneStep :: (seed -> dpCell) ->
             (dpCell -> dpCell -> dpCell) ->
             (dpCell -> dpCell -> dpCell) ->
             DPTable dpCell -> seed -> DPTable dpCell
  oneStep wrap glue tape table seed = table ++ [newCol]
    where newCol = genNextColumn wrap glue tape seed table
  
  genNextColumn :: (seed -> dpCell) ->
                   (dpCell -> dpCell -> dpCell) ->
                   (dpCell -> dpCell -> dpCell) ->
                   seed -> DPTable dpCell -> DPColumn dpCell
  genNextColumn wrap _ _ seed [] = [wrap seed]
  genNextColumn wrap glue tape seed table =
    let (topRow, sub_table) = (map head table, tail $ map tail table)
        newCol = genNextColumn wrap glue tape seed sub_table
        candidates = zipWith glue topRow newCol
        newCol_head = foldr1 tape candidates
    in newCol_head : newCol
  
  -- =================================
  -- = test function stole from scm. =
  -- = usage: id_list <a list>       =
  -- = for example: id_list [1..4]   =
  -- ===============================.=
  id_list :: Eq a => [a] -> [[Maybe [a]]] 
  id_list = dpTable f g h
   where f x = Just [x]
         g = liftM2 (++)
         h xm ym = do xs <- xm
                      ys <- ym
                      if xs == ys then Just xs
                                  else Just (xs)--Nothing