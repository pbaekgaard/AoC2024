import Data.List (sort)

parseInput :: String -> ([String], [String])
parseInput "" = ([],[])
parseInput content = (leftlist, rightlist)
  where
    rows = map words (lines content) -- Split content into rows, then split each row into words
    leftlist = [head row | row <- rows]
    rightlist = [row !! 1 | row <- rows]

getDistance :: [Int] -> [Int] -> Int
getDistance [] [] = 0
getDistance [x] [y] = dist
            where dist = if x > y then
                            x-y
                         else y - x
getDistance (x:xs) (y:ys) = curr + getDistance xs ys
                where curr = if x > y then
                                x - y
                             else y - x

getSimilarity :: [Int] -> [Int] -> Int
getSimilarity [] [] = 0
getSimilarity [] ys = 0
getSimilarity [x] [y] = if x == y then
                                x
                        else 0
getSimilarity (x:xs) ys = x * length [y | y <- ys, y == x] + getSimilarity xs ys

main :: IO ()
main = do
    content <- readFile "input.txt"
    -- Generate two lists (left and right)
    let (left, right) = parseInput content
    let leftInts = map read left :: [Int]
    let rightInts = map read right :: [Int]

    -- Sort the lists
    let sortedLeft = sort leftInts
    let sortedRight = sort rightInts
    let distanceSum = getDistance sortedLeft sortedRight
    let similarity = getSimilarity sortedLeft sortedRight
    putStrLn $ "PART 1: " ++ show distanceSum
    putStrLn $ "PART 2: " ++ show similarity
