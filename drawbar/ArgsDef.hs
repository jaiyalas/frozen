{-# LANGUAGE DeriveDataTypeable #-}

module ArgsDef where
  import System.Console.CmdArgs
  
  data Dist = RMode 
                {rep :: Int
                ,intv :: Int
                ,rseed :: Int
                ,rmax :: Int
                ,rmin :: Int
                ,size :: Int
                ,tar :: Maybe FilePath }
            | FMode 
                {intv :: Int 
                ,src :: FilePath 
                ,tar :: Maybe FilePath }
            deriving (Show, Data, Typeable, Eq)
            
  --
  rMode :: Dist
  rMode = RMode
    {rep = 1 &= name "r" &= typ "<repeat>" &= help "repeat number (default as \"1\")"
    ,intv = 50 &= name "i" &= typ "<interval>" &= help "interval value (default as \"50\")"
    ,rmin = 1 &= name "m" &= typ "<min>" &= help "minimum of random (default as \"1\")"
    ,rmax = 100 &= name "M" &= typ "<max>" &= help "maximum of random (default as \"100\")"
    ,size = 10000 &= name "z" &= typ "<size>" &= help "size of data (default as \"10000\")"
    ,rseed = 1 &= name "s" &= typ "<seed>" &= help "random seed (default as \"1\")"
    ,tar = Nothing &= typ "<file>" &= help "output file"} 
      &= help "info: run in random mode"
  --
  fMode :: Dist
  fMode = FMode
    {src = def &= typ "<source file>" &= argPos 0
    ,intv = 50 &= name "i" &= typ "<interval>"  &= help "interval value (default as \"50\")"
    ,tar = Nothing &= typ "<file>" &= help "output file"}
      &= help "info: run in file mode - perform a given file"
  
  --
  mode :: Mode (CmdArgs Dist)
  mode = cmdArgsMode $ modes [rMode,fMode] 
    &= summary "DistDotSvg v1.0"
  
  -- end of 'argsDef.hs'