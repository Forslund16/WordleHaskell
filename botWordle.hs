import Data.List
import System.Random
import Control.Concurrent
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
   putStr "Welcome to Wordle"
   gen <- newStdGen
   let (x,_) = randomR (0,rnum) gen :: (Int, StdGen)
   choose contents x


{- choose contents int
   Finds a word in contents with int
-}
choose :: String -> Int -> IO ()
choose contents int =
   wordle (head (drop int (lines contents))) (lines contents) (lines contents) 6 "abcdefghijklmnopqrstuvwxyz"

--(head (drop int (lines contents)))


{- wordle str contents lives albet
   Playes the game Wordle
   SIDE EFFECTS: input from keyboard and printing to terminal
-}
wordle :: [Char] -> [String] -> [String] -> Int -> String -> IO ()
wordle str allcontents contents lives albet = do
  if lives == 0
    then do
       putStrLn " "
       putStrLn $ "YOU LOSE! Correct word: " ++ str
       putStrLn "Would you like to restart? (y/n)"
       input <- getLine
       restart input
    else do
      word <- theBestWord contents allcontents
      putStrLn ""
      work "Calculating optimal word..."
      putStrLn $ "Computer guess: " ++ word
      if validWord allcontents word
        then
          if word == str
            then do
              putStrLn " "
              putStrLn "YOU WIN!"
              putStrLn "Would you like to restart? (y/n)"
              input <- getLine
              restart input
            else
              if length (fst (rightPosition word str)) == 4
                then do
                  let rList = posFilter word (snd (rightPosition word str)) 0
                  let newContents = step [] [] "" contents (eraseWords rList (toListTuple word))
                  work "Eliminating non-possible words..."
                  putStrLn $ "Remaining words: \n"
                  printList newContents
                  wordle str allcontents newContents (lives - 1) (removeStr (rwLetter word str False) albet)
                else do
                  let rList = posFilter word (snd (rightPosition word str)) 0
                  let contList = removeTuple (rwLetter word str False) (removeStr (posFilter word (snd (rightPosition word str)) 0) (toListTuple word))
                  let wrongLetters = removeStr (rwLetter word str True) (rwLetter word str False)
                  let newContents = step rList contList wrongLetters contents []
                  work "Eliminating non-possible words..."
                  putStrLn $ "Remaining words: \n"
                  printList newContents
                  wordle str allcontents newContents (lives - 1) (removeStr (rwLetter word str False) albet)
        else do
          putStrLn "Invalid word"
          wordle str allcontents contents lives albet


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


{- printList contents
   Prints contents with delay
   SIDE EFFECTS: printing to terminal
-}
printList :: [String] -> IO ()
printList [x] = do
  putStr x
  threadDelay 100000
printList (x:xs) = do
  putStr $ x ++ ", "
  threadDelay 50000
  printList xs


{- work str
   Prints str with rows over and under with delay
   SIDE EFFECTS: printing to terminal
-}
work :: String -> IO ()
work str = do
 putStrLn $ "\n" ++ "---------------------------------\n" ++ str ++ "\n---------------------------------" ++ "\n"
 threadDelay 2000000
