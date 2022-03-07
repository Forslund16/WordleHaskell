import Data.List
import System.Random
import Solver
import WordLists

eng = WordLists.file

spa = WordLists.file2

eng4 = WordLists.file3


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
            putStrLn ""
            randomizer eng4 69
          else do
            randomizer eng 69
      else do
        putStrLn "Invalid input"
        main
    else if lang == "S" || lang == "s"
      then do
        putStrLn ""
        randomizer spa 69
    else do
      putStrLn "Invalit input"
      main


{- randomizer contents rnum
   Finds a random number between 0 and rnum
   PRE: rnum > 0
   SIDE EFFECTS: prints a string to terminal
-}
randomizer :: [String] -> Int -> IO ()
randomizer contents rnum = do
   putStrLn "Welcome to Wordle"
   gen <- newStdGen
   let (x,_) = randomR (0,rnum) gen :: (Int, StdGen)
   choose contents x


{- choose contents int
   Finds a word in contents with int
-}
choose :: [String] -> Int -> IO ()
choose contents int = do
  putStrLn "How many words to run?"
  times <- getLine
  wordle contents contents contents 6 0 0 (read times)


{- wordle str contents lives albet
   Playes the game wordle recursively to find win rate
   SIDE EFFECTS: input from keyboard and printing to terminal
-}
wordle :: (Show a, Fractional a, Eq a) => [[Char]] -> [String] -> [String] -> Int -> a -> a -> a -> IO ()
wordle [] _ _ _ winCount iteration _ = putStrLn $ show winCount ++ "/" ++ show iteration ++ "    " ++ show (winCount / iteration)
wordle (str:xs) allcontents contents lives winCount iteration times = do
  if iteration == times
    then
      putStrLn $ show winCount ++ "/" ++ show iteration ++ "    " ++ show (winCount / iteration)
    else
      if lives == 0
        then do
           putStrLn $ show contents ++ " " ++ str
           wordle xs allcontents allcontents 6 winCount (iteration + 1) times
        else do
          word <- theBestWord contents allcontents
          if word == str
            then do
              wordle xs allcontents allcontents 6 (winCount + 1) (iteration + 1) times
            else
              if length (fst (rightPosition word str)) == 4
                then do
                  let rList = posFilter word (snd (rightPosition word str)) 0
                  let newContents = step [] [] "" contents (eraseWords rList (toListTuple word))
                  wordle (str:xs) allcontents newContents (lives - 1) winCount iteration times
                else do
                  let rList = posFilter word (snd (rightPosition word str)) 0
                  let contList = removeTuple (rwLetter word str False) (removeStr (posFilter word (snd (rightPosition word str)) 0) (toListTuple word))
                  let wrongLetters = removeStr (rwLetter word str True) (rwLetter word str False)
                  let newContents = step rList contList wrongLetters contents []
                  wordle (str:xs) allcontents newContents (lives - 1) winCount iteration times
