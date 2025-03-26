import System.Random

main :: IO ()
main = do
    gen <- newStdGen
    -- Generate a random integer
    let (randomInt, gen') = randomR (1, 100) gen :: (Int, StdGen)
    putStrLn $ "Random integer between 1 and 100: " ++ show randomInt
    -- Generate a random floating-point number
    let (randomFloat, _) = randomR (0.0, 1.0) gen' :: (Float, StdGen)
    putStrLn $ "Random float between 0.0 and 1.0: " ++ show randomFloat