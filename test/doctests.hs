module Main where

import Control.Applicative
import Control.Monad
import Test.DocTest
import System.Directory
import System.FilePath

findHs :: FilePath -> IO [FilePath]
findHs dir = do
  fs <- map (dir </>) <$>
    filter (`notElem` ["..","."]) <$>
    getDirectoryContents dir
  subDirs <- filterM doesDirectoryExist fs
  files1 <- filter ((`elem` [".hs", ".lhs"]) . takeExtension) <$>
    filterM doesFileExist fs
  files2 <- concat <$> mapM findHs subDirs
  return $ files1 ++ files2

main :: IO ()
main = do
  files <- findHs "Physics"
  putStrLn $ "testing: " ++ unwords files
  doctest files
