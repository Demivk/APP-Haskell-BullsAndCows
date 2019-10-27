import Data.List
import System.Random

main :: IO()
main = bullsAndCows

numberLength = 4 :: Int
minRange = '1'
maxRange = '9'

bullsAndCows = do
    numberToGuess <- getStdRandom(generateNumberToGuess numberLength [minRange .. maxRange])
    putStrLn "Bulls and Cows: Game started! (Type STOP to stop the game)"
    loop numberToGuess 1 where
    loop :: [Char] -> Int -> IO()
    loop numberToGuess turns = do
            input <- getLine
            if isInputValid input
                then
                    let (bulls, cows) = generateScore numberToGuess input in
                    if bulls == numberLength
                        then
                            putStrLn ("You won the game! The answer was " ++ show numberToGuess ++ ". It took " ++ show turns ++ " turns.")
                    else do
                            putStrLn (show bulls ++ " bulls, " ++ show cows ++ " cows")
                            loop numberToGuess (succ turns)
                else if input == "STOP"
                    then
                        return()
                else do
                    putStrLn "Input is not valid"
                    loop numberToGuess (succ turns)

generateNumberToGuess :: Int -> [a] -> StdGen -> ([a], StdGen)
generateNumberToGuess numberLength list generator = f numberLength list generator (length list - 1) []
  where  f 0 _ generator _   ps = (ps, generator)
         f numberLength list generator max ps =
             f (numberLength - 1) (left ++ right) generator' (max - 1) (picked : ps)
          where (index, generator') = randomR (0, max) generator
                (left, picked : right) = splitAt index list

isInputValid :: String -> Bool
isInputValid input = 
    isEveryNumberUnique input &&
    length input == numberLength &&
    all predicate input
    where predicate p = (minRange <= p) && (p <= maxRange)

isEveryNumberUnique :: String -> Bool
isEveryNumberUnique input = do
    let tempInput = input
    let nubInput = nub input
    if nubInput == tempInput
        then 
            True
        else 
            False

generateScore :: String -> String -> (Int, Int)
generateScore answer guess = (length bulls, cows)
  where (bulls, others) = partition (uncurry (==)) (zip answer guess)
        cows = length (uncurry intersect (unzip others))