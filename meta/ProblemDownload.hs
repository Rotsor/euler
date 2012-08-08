import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup
import Network.HTTP
import Data.Ord
import Data.List
import Control.Monad
import System.Directory

dumpFile = "dump.txt"

getPage i = simpleHTTP (getRequest $ "http://projecteuler.net/problems;page=" ++ show i) >>= getResponseBody
dumpAll = mapM getPage [1..8] >>= \pages -> writeFile dumpFile (show pages)

readDump :: IO [String]
readDump = fmap read (readFile dumpFile)

type Roww = [TagTree String]

findRows :: [TagTree String] -> [Roww]
findRows ss = snd $ go ss where
  mrg (i, x) (j, y) | i >= j = (i, x) | otherwise = (j, y)
  none = (0, error "is that the best you can do?!")
  go :: [TagTree String] -> (Int, [Roww])
  go = foldr mrg none . map got
  got (TagBranch tg _ prows) 
     | tg == "table" = mrg (length rows, rows) (go prows)
     | otherwise = go prows
   where
   rows = concatMap (\pr -> case pr of
             TagBranch "tr" _ row -> [row]
             _ -> []) prows
  got _ = none

getText (TagBranch _ _ [rec]) = getText rec
getText (TagLeaf (TagText txt)) = txt

processRow l = case concatMap getTD l of
  [] -> []
  [[a],[b],[c]] -> [(read $ getText a, read $ getText c)]
 where
  getTD (TagBranch "td" _ nested) = [nested]
  getTD _ = []
  

process :: String -> [(Int, Int)]
process = concatMap processRow . findRows . tagTree . parseTags

haveSolved :: Int -> IO Bool
haveSolved number = doesFileExist $ "eu" ++ show number ++ ".hs"

main = readDump >>=
       filterM (fmap not . haveSolved . fst) . sortBy (comparing $ negate . snd) . concat . map process >>= mapM_ print . take 100
