module Main where

import System.IO
import System.Environment
  {-

  import System.Console.CmdArgs

  data CSVS = CSVS
      {src :: Maybe FilePath
      ,out :: FilePath
      ,_min :: Int
      ,_max :: Int
      }deriving (Data,Typeable,Show,Eq)

  outFlags x = x &= help "Output file" &= typFile
  --
  _csvs = CSVS
      {src = def &= help "Source directory" &= typDir
      ,out = outFlags "out.csv"
      ,_min = def &= name "m" &= name "min" &= typ "NUM" &= help "minimum of file name"
      ,_max = def &= name "M" &= name "max" &= typ "NUM" &= help "maxmum of file name"
      } &=
      auto &=
      summary "version = 1.0"
  -}
  --getOpt Permute [(Option 'M' "max" (ReqArg read ) "")]

main :: IO ()
main = do
    --cmdArgsRun (cmdArgsMode _csvs)
    --let n = _min _csvs
    --let m = _max _csvs
    putStrLn ("CSV Squasher v1.0")
    str <- getArgs
    let (j:n:m:_) = (map read str) :: [Int]
    let src__ = "./testData/"
    let out__ = "./out.csv"
    --let src__ = case (src _csvs) of Nothing -> "./"
    --                                (Just a) -> a
    file <- openFile out__ WriteMode
    allContents <- getFileContents j src__ [n..m]
    printList2Handle file allContents
    hFlush file
    hClose file
    putStrLn ("completed file := "++out__)

getFileContents :: Int         ->  -- ^
                     String      ->
                     [Int]       ->
                     IO [String]
getFileContents _ _ [] = return [] -- error "it should not happen!"
getFileContents j src__ [i] = do
    putStrLn ("start := "++ src__ ++(show i)++".csv")
    file <- openFile (src__++(show i)++".csv") ReadMode
    jump j file
    fff <- getCT file
    hClose file
    putStrLn ("finish := "++src__++(show i)++".csv")
    return fff
getFileContents j src__ (i:is) = do
    putStrLn ("start := "++src__++(show i)++".csv")
    file <- openFile (src__++(show i)++".csv") ReadMode
    jump j file
    fff <- getCT file
    hClose file
    re <- getFileContents j src__ is
    putStrLn ("finish := "++src__++(show i)++".csv")
    return $ zipWith (\x y -> x ++ "," ++ y) fff re

-- | skip n lines of file
jump :: Int -> Handle-> IO ()
jump 0 _ = return ()
jump i ff = do
    _ <- hGetLine ff
    jump (i-1) ff

-- |
getCT :: Handle -> IO [String]
getCT f = do
    isEND <- hIsEOF f
    if (not isEND)
        then do
            l <- hGetLine f
            re <- getCT f
            return ((init l):re)
        else return []

-- |
printList2Handle :: Handle -> [String] -> IO ()
printList2Handle _    []     = return ()
printList2Handle file (x:xs) = do
    hPutStrLn file x
    printList2Handle file xs
