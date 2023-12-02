import System.IO
import Data.Char

gameLimits :: (Int, Int, Int)
gameLimits = (12, 13, 14)

cubePower :: (Int, Int, Int) -> Int
cubePower (a, b, c) = a * b * c 

gameLims :: [String] -> (Int, Int, Int) -> (Int, Int, Int)
gameLims (x:y:xs) (r, g, b)
    | arg == 'r' = if int > r then gameLims xs (int, g, b) else gameLims xs (r, g, b)
    | arg == 'g' = if int > g then gameLims xs (r, int, b) else gameLims xs (r, g, b)
    | arg == 'b' = if int > b then gameLims xs (r, g, int) else gameLims xs (r, g, b)
    where arg = head y
          int = read x
gameLims [] (r, g, b) = (r, g, b)
gameLims (x:xs) (r, g, b) = (r, g, b)

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
        return (cubePower (gameLims y (0,0,0)) + res)
        -- if isGameBig y gameLimits
        --     then return (0 + res)
        --     else return (x + res)

main :: IO ()
main = do
    handle <- openFile "inputs/day2.txt" ReadMode
    res <- Main.cycle handle
    print res