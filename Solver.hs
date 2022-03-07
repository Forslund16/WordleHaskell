
-- EXPORT ----------------------------------------------------------------------
module Solver(BitCode, step, sumWords, elemFreq, findBestWord,validWord,subWord,right,rightPosition,rwLetter,removeStr,removeChar,toListTuple,removeTuple,posFilter,eraseWords,theBestWord) where


-- IMPORT ----------------------------------------------------------------------
import WordLists
import Data.List
import Data.Bits
import Data.Maybe
import Control.Applicative (liftA2)
import Test.HUnit

-- TYPES ----------------------------------------------------------------------

{- BitCode
   a bit code (of a character or string) is represented by a list of Booleans
-}
type BitCode = [Bool]

{- Position
   Describes index of Char in terms of Int.
   INVARIANT: Int > -1
-}
type Position = (Char, Int)

{- Strength a
   Describes the quantitative power of a
   by representing the occurrence of a in terms of Int.
   INVARIANT: Int > -1
-}
type Strength a = (a, Int)



-- VARIABLES -------------------------------------------------------------------
contents = WordLists.file
contentos = WordLists.file2



-- TYPES AND SPECIFICATIONS ------------------------------------------------------

{- elemFreq lst
   Calculates the number of occurrences of all elements in given list.
   PRE: True
   RETURNS: a list of tuples with assigned values, in a ordered fashion (descending).
   EXAMPLES: elemFreq [('a','c'),('d','d'),('d','d')] == [(('d','d'),2),(('a','c'),1)]
             elemFreq ['a','a','b','c','c','f','a']   == [('a',3),('c',2),('f',1),('b',1)]
             elemFreq []                              == []
-}
elemFreq :: Eq a => [a] -> [Strength a]

{- elemFreqAux lst
   Calculates the number of occurrences of all elements in given list.
   PRE: True
   RETURNS: a list of tuples with assigned values according to amount of occurrence in lst
   EXAMPLES: elemFreqAux [('a','c'),('d','d'),('d','d')] == [(('a','c'),1),(('d','d'),2)]
             elemFreqAux "hello" == [('h',1),('e',1),('l',2),('o',1)]
             elemFreqAux []                              == []
-}
--VARIANT: length lst
elemFreqAux :: Eq a => [a] -> [Strength a]


{- posFreq int words
   Calculates the occurrences of a letter in a word at given position.
   PRE: int < length String
   RETURNS: a list of tuples with assigned values for every char, in ordered fashion (descending).
   EXAMPLES: posFreq 0 ["david", "william", "oliver"] == [('o',1),('w',1),('d',1)]
             posFreq 0 []                             == []
-}
posFreq :: Int -> [String] -> [Strength Char]


{- step rList contList wrongLetters contents special
   The step function is responsible for eliminating and keeping words based on given arguments.
   Each argument of the function correlates to a particular part of information gathered by the program.
   RETURNS: a list of remaining possible words.
   EXAMPLES:
             Possible 5-letter english words (contents) ending with 'k',
             that contain the letter 'u' anywhere except in the middle of the word,
             after guessing the word "soare",
             is expressed as:
             step [('k',4)] [('u',2)] "soare" contents [] == ["quick","mujik","pulik","tupik"]
-}
step :: [Position] -> [(Char,Int)] -> String -> [String] -> [(Char,Int)] -> [String]


{- posTrial pos contents bool
   Removes (bool == False) or keeps (bool == True) strings that contain char at specified position.
   RETURNS: A list of strings in accordance with pos, with regard to bool.
   EXAMPLES: posTrial [('u', 0)] ["unity", "budge", "pkd"] True  == ["unity"              ]
             posTrial [('u', 0)] ["unity", "budge", "pkd"] False == [        "budge","pkd"]
             posTrial []         ["unity", "budge", "pkd"] False == ["unity","budge","pkd"]
             posTrial []         ["unity", "budge", "pkd"] True  == ["unity","budge","pkd"]
-}
--VARIANT: length pos
posTrial :: [Position] -> [String] -> Bool -> [String]


{- posTrialAux (c,n) contents bool
   Removes (bool == False) or keeps (bool == True) strings that contain char at specified position.
   RETURNS: if bool == True then a list of words from contents containing c in position n,
            else a list of words from contents not containing c in position n
   EXAMPLES: posTrialAux ('u', 0) ["unity", "budge", "pkd"] True  == ["unity"              ]
             posTrialAux ('u', 0) ["unity", "budge", "pkd"] False == [        "budge","pkd"]
             posTrialAux          ["unity", "budge", "pkd"] False == ["unity","budge","pkd"]
             posTrialAux []       ["unity", "budge", "pkd"] True  == ["unity","budge","pkd"]
-}
--VARIANT: length contents
posTrialAux :: Position -> [String] -> Bool -> [String]


{- wordKeeper pos words
   Keeps words that contain Char in pos, regradless of their index.
   RETURNS: A list of strings meeting the condition.
   EXAMPLE: wordKeeper [('u',0)] ["unity", "budge", "pkd"] == ["unity", "budge"       ]
            wordKeeper []        ["unity", "budge", "pkd"] == ["unity", "budge", "pkd"]
-}
--VARIANT: length pos
wordKeeper :: [Position] -> [String] -> [String]

{- wordKeeperAux char words
   Keeps strings from words that contain char
   PRE: char is a list of one element
   RETURNS: words with only strings containing char
   EXAMPLE: wordKeeperAux ['u'] ["unity", "budge", "pkd"] == ["unity", "budge"]
            wordKeeperAux []    ["unity", "budge", "pkd"] == ["unity", "budge", "pkd"]
-}
wordKeeperAux :: String -> [String] -> [String]


{- wordFilter fltr lst
   Filters out words that share same letters as fltr,
   regradless of their index.
   RETURNS: lst whose elements share contents with fltr.
   EXAMPLE: wordFilter "gu" ["unity","budge","pkd"] == [                  "pkd"]
            wordFilter  []  ["unity","budge","pkd"] == ["unity", "budge", "pkd"]
-}
wordFilter :: String -> [String] -> [String]

{- theBestWord contents allcontents
   Finds the best word in contents
   PRE: contents is not empty, length of each word is the same in contents and allcontents
   RETURNS: a list of the word giving the most information according to contents and allcontents
   EXAMPLE: theBestWord contents contents == ["soare"]
            theBestWord ["hello","mello","cello"] contents == ["cello"]
-}
theBestWord :: Monad m => [String] -> [String] -> m String


{- wordStrength lst
   Determines Strength of individual words.
   RETURNS: a list, describing the Strength of elements in lst.
   EXAMPLE: wordStrength  ["soare", "crane", "yummy"] == [("soare",8),("crane",8),("yummy",5)]
-}
wordStrength :: [String] -> [Strength String]


{- findBestWord inpt
   Selects the strongest element in list.
   PRE: null inpt == False
   RETURNS: first element of selected tuple in inpt.
   EXAMPLE: findBestWord [("foo",3),("pkd", 100),("bar",99)]  == "pkd"
            findBestWord [("foo",3),("pkd",-100),("bar",99)] == "bar"
-}
findBestWord :: [Strength a] -> a


{- sumWords lst tpl
   Calculates the strength of each word, relative to tpl.
   Pre: null tpl == False
   RETURNS: Strength of each element in lst
   EXAMPLE: sumWords ["ab","cd"] [('a',10),('b',1),('c',1),('d',1)] == [("ab",11),("cd",2)]
-}
--VARIANT: length lst
sumWords :: [String] -> [Strength Char] -> [Strength String]


{- strSum str tpls acc
   Calcuates strength of given word.
   PRE: (null str) || ((null tpls) == False)
   RETURNS: a value describing the strength of str
   EXAMPLE: strSum "ab" [('a',10),('b',1),('c',1),('d',1)] [] == 11
            strSum  ""  []                                 [] ==  0
-}
--VARIANT: length str
strSum :: String -> [Strength Char] -> [Char] -> Int


{- fromBestWord lst acc
   Determines all the words with the highest strength.
   PRE: (null lst == False) && lst must be sorted on snd and reversed
   RETURNS: a list of words from lst with the highest strength
   EXAMPLE: fromBestWord [("soare",6),("yummy",6),("d",5),("g",1)] [] == [("yummy",6),("soare",6)]

-}
--VARIANT: length lst
fromBestWord :: [Strength String] -> [Strength String] -> [Strength String]


{- smartBestWord tpls lst
   Calculates the Strength of a word relative to the Position-Strength of its Chars.
   PRE: length of words in tpls == length of words in lst
   RETURNS: a list of the new Strength of each element in tpls.
   EXAMPLE:  smartBestWord [("arose",0),("soare",0)] contents == [("arose",3180),("soare",5604)]
             smartBestWord []                        contents == []
-}
--VARIANT: length tpls
smartBestWord :: [Strength String] -> [String] -> [Strength String]


{- smartBestWordAux str n contents
   Calculates a word's positional strength
   PRE: length str == length of the words in contents
   RETURNS: a value of how strong str is according to its characters position in contents
   EXAMPLE:  smartBestWordAux "soare" 0 contents == 7123
             smartBestWordAux "arose" 0 contents == 4699
-}
--VARIANT: length str
smartBestWordAux :: String -> Int -> [String] -> Int


{- findInt c lst
   Finds the neighbouring element in an association list.
   PRE: c must be present in lst
   EXAMPLES: findInt 'f' [('b',0),('f',1)] == 1
-}
findInt :: Char -> [(Char,Int)] -> Int


{- eraseWords str1 str2
   Filters out chars in str2 that is shared between the two arguments.
   Pre: True
   EXAMPLE: eraseWords "abc" "adbeca" == "de"
            eraseWords ""  "f"       == "f"
            eraseWords "f" ""        == ""
            eraseWords ""  ""        == ""
-}
--VARIANT: length str2
eraseWords :: (Foldable t, Eq a) => t a -> [a] -> [a]


{- toListTuple str
   Assigns numerical index to each character in string.
   RETURNS: a list, containing Position of each element in str.
   EXAMPLE: toListTuple "haskell" == [('h',0),('a',1),('s',2),('k',3),('e',4),('l',5),('l',6)]
            toListTuple ""        == []
-}
toListTuple :: String -> [Position]


{- removeTuple str tpls
   Filters out any tuple whose key share same Char as str.
   EXAMPLES: removeTuple "david" [('e',0),('g',1)] == [('e',0),('g',1)]
             removeTuple "dave"  [('e',0),('g',1)] == [('g',1)]
-}
removeTuple :: String -> [Position] -> [Position]


{- posFilter str bits iteration
   Filters str according to bits.
   PRE: length str == length bits
   EXAMPLE: posFilter "David" [True,False,True,False,True] 0 == [('D',0),('v',2),('d',4)]
-}
--VARIANT: length str
posFilter :: String -> BitCode -> Int -> [Position]


{- validWord lst guess
   Looks if guess is a element of lst.
   RETURNS: True if lst contains guess, otherwise False
   EXAMPLE: validWord contents "yummy" == True
-}
--VARIANT: length lst
validWord :: [String] -> String -> Bool

{- subWord str bits
   Visualizes where in a word a letter is
   PRE: length str == length bits
   RETURNS: a string with only the letters of str that match with bits are visable,
            other letters is shown as ' '
   EXAMPLES: subWord "hello" [True,False,True,False,True] == "h l o"
             subWord "hello" [False,False,False,False,False] == "     "
-}
--VARIANT: length str
subWord :: String ->   BitCode -> String

{- right str1 str2
   Finds the letters that str1 and str2 have in common
   PRE: length str1 == length str2
   RETURNS: [a string of the letters in the correct position, a string of letters that are correct but in the wrong position]
   EXAMPLES: right "hello" "hales" == ["hl","e"]
             right "hello" "laugh" == ["","hl"]
-}
right :: String -> String -> [String]

{- rightPosition str1 str2
   Finds letters from str1 that are in the same position as in str2
   PRE: length str1 == length str2
   RETURNS: a tuple with a string containing the right letters
            and a BitCode showing what position they were in
   EXAMPLES: rightPosition "hello" "jello" == ("ello",[False,True,True,True,True])
             rightPosition "hello" "hales" == ("hl",[True,False,True,False,False])
             rightPosition "hello" "laugh" == ("",[False,False,False,False,False])
-}
--VARIANT: 2 * length str1
rightPosition :: String -> String  -> (String,BitCode)

{- rwLetter str1 str2 bool
   Finds letters from str1 that exist in str2, or letters that does not exist in str2
   PRE: length str1 == length str2
   RETURNS: a string of the common letters between str1 and str2 if bool == True,
            else a string of the non-common letters between str1 and str2
   EXAMPLES: rwLetter "hello" "laugh" True == "hl"
             rwLetter "hello" "laugh" False == "elo"
-}
--VARIANT: length str1
rwLetter :: String -> String -> Bool -> String

{- removeStr str1 str2
   Removes the exact amount of each char in str1 from str2
   PRE: True
   RETURNS: str2 without the characters from str1
   EXAMPLE: removeStr "abc" "adbeca" == "dea"
            removeStr ""  "f"       == "f"
            removeStr "f" ""        == ""
            removeStr ""  ""        == ""
-}
--VARIANT: length str1
removeStr :: Eq a => [a] -> [a] -> [a]

{- removeChar c str
   Removes the first occurrence of c in str
   PRE: True
   RETURNS: str without the first c
   EXAMPLE: removeChar 'a' "adbeca" == "dbeca"
            removeChar 'e'  "f"       == "f"
-}
removeChar :: Eq a => a -> [a] -> [a]

-- QUANTITATIVE LANGUAGE ANALYSIS -----------------------------------------------------------


posFreq int words = elemFreq $ head $ [map (!! int) words]


elemFreq lst = reverse $ sortOn snd $ elemFreqAux lst


elemFreqAux [] = []
elemFreqAux str@(x:xs) =
 (x, length str - length fltr) : elemFreqAux fltr
  where fltr = filter (/= x) xs


-- ALGORITHM -----------------------------------------------------------------------------

step rList contList wrongLetters contents special = posTrial special (wordFilter wrongLetters lst) False
  where lst = wordKeeper contList (posTrial contList (posTrial rList contents True) False)

--WORDLE FUNCTIONS ------------------------------------------------------------------------

posTrial [] lst bool = lst
posTrial (x:xs) lst bool = posTrial xs (posTrialAux x lst bool) bool


posTrialAux (c,n) [] bool = []
posTrialAux (c,n) (x:xs) bool
 | not(bool) `xor` (c == (x !! n)) = x : posTrialAux (c,n) xs bool
 | otherwise = posTrialAux (c,n) xs bool


wordKeeper [] lst = lst
wordKeeper ((h,_):hs) lst = wordKeeper hs (wordKeeperAux [h] lst)

wordKeeperAux fltr lst =  filter (isSubsequenceOf fltr) (lst)


wordFilter str lst = [ x | x <- lst, null $ intersect str x]

-----------------------------------------------------------------------------------------

theBestWord contents allcontents = do
  return $ findBestWord (smartBestWord (fromBestWord (reverse (sortOn snd (wordStrength contents))) []) allcontents)


wordStrength lst =
  sumWords lst (elemFreq $ concat $ lst)


findBestWord inpt = fst (head (reverse (sortOn snd inpt)))


sumWords [] ls = []
sumWords (x:xs) ls = (x, (strSum x ls [])) : sumWords xs ls


strSum [] ls acc = 0
strSum (x:xs) ls acc
  | elem x acc = strSum xs ls acc
  | otherwise  = (findInt x ls) + strSum xs ls (x : acc)


fromBestWord [x] acc = x : acc
fromBestWord ((a,b):(c,d):xs) acc
 |b == d = fromBestWord ((c,d):xs) ((a,b):acc)
 |otherwise = (a,b):acc


smartBestWord [] allcontents = []
smartBestWord ((x,_):xs) allcontents = (x,smartBestWordAux x 0 allcontents) : smartBestWord xs allcontents


-- smartBestWordAux123 (x:xs) n word allcontents
--  | n == (length word)  = 0
--  | otherwise =  value + smartBestWordAux123 xs (n + 1) word allcontents
--     where value = findInt x (posFreq n allcontents)

smartBestWordAux [] _ _ = 0
smartBestWordAux (x:xs) n allcontents = value + smartBestWordAux xs (n + 1) allcontents
    where value = findInt x (posFreq n allcontents)

findInt c lst = fromJust $ lookup c lst

-------------------------------------------------------------------------------------------

eraseWords _ [] = []
eraseWords str (x:xs)
  | elem x str = eraseWords str xs
  | otherwise = x : eraseWords str xs

--WORDLE FUNCTIONS ------------------------------------------------------------------------

toListTuple str = zip str [0 .. (length str) - 1]

---------------------------------------------------------------------------------

removeTuple str lst = [ (c,n) | (c,n) <- lst, null $ intersect str [c]]

---------------------------------------------------------------------------------

posFilter [] [] _ = []
posFilter (x:xs) (b:bits) iteration
  | b         = (x, iteration) : (posFilter xs bits (iteration + 1))
  | otherwise = posFilter xs bits (iteration + 1)

---------------------------------------------------------------------------------

validWord [] _  = False
validWord (x:xs) userInput =
  x == userInput || validWord xs userInput

-------------------------------------------------------------------------------------------

subWord    word        []       = []
subWord    (x:xs) (b:bits)
 |                      b       = x : subWord xs bits
 | otherwise                    = ' '  : subWord xs bits


right    guess        correct      = [fst rp, removeStr (fst rp) rl]
    where
     rp = rightPosition guess correct
     rl = rwLetter guess correct True


rightPosition    []        []       = ("",[])
rightPosition guess@(x:xs)    (y:ys)
 |                x ==      y       = (x : fst (rightPosition xs ys), True : snd (rightPosition xs ys))
 | otherwise                        = (fst (rightPosition xs ys), False : snd (rightPosition xs ys))


rwLetter    []          _   _      = []
rwLetter guess@(x:xs) correct   bool
  | bool `xor` (find (==x) correct == Just x) = rwLetter xs rest bool
  | otherwise                = x : rwLetter xs rest bool
    where rest = removeChar x correct


removeStr    []        str     = str
removeStr (x:xs)       str     = removeStr xs (removeChar x str)


removeChar letter str = delete letter str

-- TESTS -------------------------------------------------------------------------------------------

test1 = TestCase $ assertEqual "step " ["arose"] (step [('a',0),('r',1),('o',2),('s',3)] [] "l" contents [])

test2 = TestCase $ assertEqual "theBestWord " ["aeros"] (theBestWord contents contents)

test3 = TestCase $ assertEqual "eraseWords " "de" (eraseWords "abc" "abcdec")

test4 = TestCase $ assertEqual "toListTuple " [('s',0),('o',1),('a',2),('r',3),('e',4)] (toListTuple "soare")

test5 = TestCase $ assertEqual "removeTuple " [] (removeTuple "arose" [('s',0),('o',1),('a',2),('r',3),('e',4)])

test6 = TestCase $ assertEqual "removeTuple " [('o',1),('r',3)] (removeTuple "spain" [('s',0),('o',1),('a',2),('r',3),('s',4)])

test7 = TestCase $ assertEqual "posFilter " [('d',0),('v',2),('d',4)] (posFilter "david" [True,False,True,False,True] 0)

test8 = TestCase $ assertEqual "validWord " True (validWord contents "soare")

test9 = TestCase $ assertEqual "subWord " "    e" (subWord "arose" (snd $ rightPosition "arose" "soare"))

test10 = TestCase $ assertEqual "right " ["e","aros"] (right "arose" "soare")

test11 = TestCase $ assertEqual "snd rightPosition " [False,False,False,False,True] (snd $ rightPosition "arose" "soare")

test12 = TestCase $ assertEqual "removeStr " "hihi" (removeStr "hi" "hihihi")

test13 = TestCase $ assertEqual "rwLetter " ("elo","hl") ((rwLetter "hello" "melon" True),(rwLetter "hello" "melon" False))

runtests = runTestTT $ TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13]
