import Data.List
import System.Random
import Solver


{- main
   Run the game
   SIDE EFFECTS: reading from file, print to terminal and input from keyboard,
                 plus all side effects from functions main calls
-}
main :: IO ()
main = do
  putStrLn "English or Spanish? (E/S)"
  lang <- getLine
  if lang == "E" || lang == "e"
    then do
    putStrLn "4 or 5 letter words? (4/5)"
    num <- getLine
    if num == "4" || num == "5"
      then
        if num == "4"
          then do
            contents <- readFile "4letterwords.txt"
            randomizer contents (length (lines contents) - 1)
          else do
            contents <- readFile "5letterwords.txt"
            randomizer contents 2308
      else do
        putStrLn "Invalid input"
        main
    else if lang == "S" || lang == "s"
      then do
        contents <- readFile "spanish.txt"
        randomizer contents (length (lines contents) - 1)
    else do
      putStrLn "Invalit input"
      main


{- randomizer contents rnum
   Finds a random number between 0 and rnum
   PRE: rnum > 0
   SIDE EFFECTS: prints a string to terminal
-}
randomizer :: String -> Int -> IO ()
randomizer contents rnum = do
   putStrLn "Welcome to Wordle"
   gen <- newStdGen
   let (x,_) = randomR (0,rnum) gen :: (Int, StdGen)
   choose contents x


{- choose contents int
   Finds a word in contents with int
   SIDE EFFECTS: all side effects from wordle function
-}
choose :: String -> Int -> IO ()
choose contents int =
   wordle (head (drop int (lines contents))) contents 6 "abcdefghijklmnopqrstuvwxyz"


{- wordle str contents lives albet
   Playes the game Wordle
   SIDE EFFECTS: input from keyboard and printing to terminal
-}
wordle :: String -> String -> Int -> String -> IO ()
wordle str contents lives albet = do
  if lives == 0
    then do
         putStrLn " "
         putStrLn $ "YOU LOSE! Correct word: " ++ str
         putStrLn "Would you like to restart? (y/n)"
         input <- getLine
         restart input
    else do
      putStrLn "Make a guess: "
      word <- getLine
      if validWord (lines contents) word
        then
          if word == str
            then do
              putStrLn " "
              putStrLn "YOU WIN!"
              putStrLn "Would you like to restart? (y/n)"
              input <- getLine
              restart input
            else do
              let (_,p) = rightPosition word str in putStr $ (subWord word p) ++ "  "
              putStrLn $ "contains::[" ++ (last (right word str)) ++ "]  " ++ "remaining::["
                ++ (removeStr (rwLetter word str False) albet) ++ "]  " ++ "Lives left: " ++ (show (lives - 1))
              wordle str contents (lives - 1) (removeStr (rwLetter word str False) albet)
        else do
          putStrLn "Invalid word"
          wordle str contents lives albet


{- restart input
   Asks if player wants to play again
   SIDE EFFECTS: printing to terminal and input from keyboard
-}
restart :: String -> IO ()
restart input
 | "y" == input = main
 | "n" == input = putStrLn "Goodbye!"
 | otherwise = do
    putStrLn "Try again! "
    input <- getLine
    restart input
