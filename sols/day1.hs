import System.IO
import Data.Char

turnToInt :: [Int] -> Int
turnToInt [] = error "Not possible"
turnToInt (x:y:xs) = (x * 10) + y
turnToInt (x:xs) = (x * 10) + x

getDigit :: String -> [Int] -> Int
getDigit [] [] = error "Problem"
getDigit [] xs = turnToInt xs
getDigit (c:cs) [] = if isDigit c then getDigit cs [digitToInt c] else getDigit cs []
getDigit (c:cs) (x:xs) = if isDigit c then getDigit cs [x, digitToInt c] else getDigit cs (x:xs)

cycle' :: Handle -> Int -> IO Int
cycle' handle n = do
    eof <- hIsEOF handle
    if eof
        then return n
        else do
            line <- hGetLine handle
            res <- cycle' handle (getDigit line [])
            return (n + res)

main :: IO ()
main = do
    handle <- openFile  "../inputs/day1.txt" ReadMode
    result <- Main.cycle' handle 0
    putStrLn (show result)
    hClose handle


