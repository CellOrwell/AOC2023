import System.IO
import Data.Char
import Language.Haskell.TH (numTyLit)

numWordMap :: [(String, Int)]
numWordMap = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]

toLowerCase :: String -> String
toLowerCase = map toLower

isWordNum :: String -> [(String, Int)] -> Int
isWordNum s1 [] = -1
isWordNum s1 ((s2, n) : xs) = if toLowerCase s1 == s2 then n else isWordNum s1 xs

wordSearch :: String -> String -> String
wordSearch [] _ = []
wordSearch cs cs'
    | length newWord > 5 = []
    | isWordNum newWord numWordMap > 0 = newWord
    | otherwise = wordSearch (tail cs) newWord
        where newWord = cs' ++ [head cs]

searchLine :: String -> [String]
searchLine [] = []
searchLine (c:cs)
    | isDigit c = [c] : searchLine cs
    | otherwise = wordSearch (c:cs) [] : searchLine cs

removeWhitespace :: [String] -> [String]
removeWhitespace = filter (not . null) 

getFinalInt :: (Int, Int) -> Int
getFinalInt (x, y) = (x*10) + y

convToInt :: (String, String) -> (Int, Int)
convToInt (x, y)
    | isDigit xChar && isDigit yChar = (digitToInt xChar, digitToInt yChar)
    | isDigit xChar = (digitToInt xChar, isWordNum y numWordMap)
    | isDigit yChar = (isWordNum x numWordMap, digitToInt yChar)
    | otherwise = (isWordNum x numWordMap, isWordNum y numWordMap)
        where xChar = head x
              yChar = head y

getLineDig :: String -> Int
getLineDig [] = 0
getLineDig cs = getFinalInt (convToInt (head numList, last numList))
    where numList = removeWhitespace (searchLine cs)

cycle' :: Handle -> Int -> IO Int
cycle' handle n = do
    eof <- hIsEOF handle
    if eof
        then return n
        else do
            line <- hGetLine handle
            res <- cycle' handle (getLineDig line)
            return (n + res)

main :: IO ()
main = do
    handle <- openFile  "inputs/day1.txt" ReadMode
    result <- Main.cycle' handle 0
    print result
    hClose handle


