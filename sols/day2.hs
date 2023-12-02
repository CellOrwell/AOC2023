import System.IO
import Data.Char

gameLimits :: (Int, Int, Int)
gameLimits = (12, 13, 14)

isGameBig :: [String] -> (Int, Int, Int) -> Bool
isGameBig (x:y:xs) (r, g, b)
    | arg == 'r' = (int > r) || isGameBig xs (r, g, b)
    | arg == 'g' = (int > g) || isGameBig xs (r, g, b)
    | arg == 'b' = (int > b) || isGameBig xs (r, g, b)
    where arg = head y
          int = read x
isGameBig [] (r, g, b) = False
isGameBig (x:xs) (r, g, b) = False

getGameInstr :: [String] -> IO (Int, [String])
getGameInstr (x:y:xs) = return (read (onlyNums y), xs)
getGameInstr _ = return (-1, [""])

onlyNums :: String -> String
onlyNums [] = []
onlyNums (s : ss)
    | isDigit s = s : onlyNums ss
    | otherwise = onlyNums ss

cycle :: Handle -> IO Int
cycle handle = do
    eof <- hIsEOF handle
    if eof
        then return 0
    else do 
        line <- hGetLine handle
        (x, y) <- getGameInstr (words line)
        res <- Main.cycle handle
        if isGameBig y gameLimits
            then return (0 + res)
            else return (x + res)

main :: IO ()
main = do
    handle <- openFile "inputs/day2.txt" ReadMode
    res <- Main.cycle handle
    print res