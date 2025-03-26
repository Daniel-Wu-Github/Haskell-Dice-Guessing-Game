-- Daniel Wu 23400749
import System.Random

-- Define the DiceGame ADT
data DiceGame -- used to run all the functions of the game
    = Round Int
    | Guess Int Int
    | UpdateScore Int Int
    | HighestScore Int
    deriving (Show, Read)

-- Functions to run the game
runRound :: DiceGame -> Int
runRound (Round n) = n+1 -- iterate the round number
runRound (Guess input actual) -- calculate the points earned
    | input == actual =  5
    | abs (input - actual) == 1 = 1
    | otherwise = 0
runRound (UpdateScore current new) = current + new -- update the score



--Main Function

main :: IO()
main = do
    putStrLn "Welcome to the Dice Guessing Game!\n" -- welcome message and start the game
    playGame 0 0

playGame :: Int -> Int -> IO() -- each individual game
playGame totalScore highestScore = do -- keeps track of the total score of that game and the highest score
    putStrLn "Starting a new game...\n"
    finalScore <- playRound 1 0 -- start the game with the first round and 0 points
    let newHighScore = max finalScore highestScore -- compare the final score to the highest score
    putStrLn $ "Game over! Your total score was: " ++ show finalScore
    putStrLn $ "Highest score so far: " ++ show newHighScore
    
    putStrLn "Do you want to play again? (y/n): " --play again if prompted
    playAgain <- getLine
    if playAgain == "y"
        then playGame 0 newHighScore
    else if playAgain == "n"
        then putStrLn "Thank you for playing!"
    else putStrLn "Invalid input, game over." -- if the input is not y or n, defualt to game over

playRound :: Int -> Int -> IO Int -- each individual round that returns a score at the end
playRound roundNumber currentScore
    | roundNumber > 5 = return currentScore -- if the round number is greater than 5, return the current score, along with recursion, similar to a while loop
    | otherwise = do
        putStrLn $ "--- Round " ++ show roundNumber ++ " ---"
        putStrLn "Guess the dice roll (1-6): "
        guess <- readLn
        if guess < 1 || guess > 6  -- if the guess is not between 1 and 6, start the round over
            then do
                putStrLn "Invalid guess, Round starting over..."
                playRound roundNumber currentScore
        else do
            actual <- randomRIO (1, 6)
            let earnedPoints = runRound (Guess guess actual)
            putStrLn $ "The dice rolled: " ++ show actual
            putStrLn $ "You earned " ++ show earnedPoints ++ " points!\n"
            let newScore = runRound (UpdateScore currentScore earnedPoints) -- update the score using ADT
            let newRoundNumber = runRound (Round roundNumber)
            playRound newRoundNumber newScore -- pass in the new score and round number to the next round



