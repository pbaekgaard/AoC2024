import Data.List (delete)
getRows content = do
    let rows = map words (lines content) -- Split content into rows, then split each row into words
    map (\x -> map read x :: [Int]) rows

getSafeRows :: [[Int]] -> [[Int]]
getSafeRows rows = [x | x <- rows, isSafe x]

getUnsafeRows :: [[Int]] -> [[Int]]
getUnsafeRows rows = [x | x <- rows, not (isDecreasingSafely x) && not (isIncreasingSafely x)]


isDecreasingSafely [x,y] = x > y && x-y <= 3 && x-y>= 0
isDecreasingSafely (x:y:xs) = x > y && x-y <= 3 && x-y >= 0&& isDecreasingSafely (y:xs)


isIncreasingSafely [x,y] = x < y && y-x <= 3 && y-x >= 0
isIncreasingSafely (x:y:xs) = x < y && y-x <= 3 && y-x >= 0 && isIncreasingSafely (y:xs)

isSafe row = isDecreasingSafely row || isIncreasingSafely row

canBeSafe :: [Int] -> Bool
canBeSafe row = any (isSafe . (`delete` row)) row

getDampenedSafeRows :: [[Int]] -> [[Int]]
getDampenedSafeRows rows = [row | row <- rows, canBeSafe row]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let rows = getRows content
    let safeRows = getSafeRows rows
    let numberOfSafeRows = length safeRows
    putStrLn $ "PART 1: " ++ show numberOfSafeRows

    let unsafeRows = getUnsafeRows rows
    -- putStrLn $ "unsafe before: " ++ show unsafeRows
    let dampenedSafeRows = getDampenedSafeRows rows
    print dampenedSafeRows
    let safeRowsWithDampening = length dampenedSafeRows
    putStrLn $ "PART 2: " ++ show safeRowsWithDampening

