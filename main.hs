import Control.Monad  
import Data.Char  
import System.IO
  
data State = Empty | Full deriving(Eq, Show)

main = do  
    let board = [[Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty]]
    printBoard board
    runGame board

runGame board = do
                putStr "Enter the row to drop the piece into: " 
                hFlush stdout
                moveStr <- getLine
                let move = parseMove moveStr
                let x = move !! 0
                let y = lowestEmpty board x
                let newBoard = (update board x y)
                printBoard newBoard
                if not (winningMove newBoard x y)
                   then (runGame newBoard)
                else putStr "Game over\n"

lowestEmpty :: [[State]] -> Integer -> Integer
lowestEmpty board x = lowestEmptyImpl board x 5

lowestEmptyImpl :: [[State]] -> Integer -> Integer -> Integer
lowestEmptyImpl board x y 
    | getState board x y == Empty = y
    | getState board x y == Full = lowestEmptyImpl board x (y - 1)

winningMove :: [[State]] -> Integer -> Integer -> Bool
winningMove board x y = checkHorizontal board x y || checkVertical board x y

checkVertical :: [[State]] -> Integer -> Integer -> Bool
checkVertical board x y = checkVertImpl board x 0 0

checkVertImpl :: [[State]] -> Integer -> Integer -> Integer -> Bool
checkVertImpl board x y count 
    | count >= 4                    = True
    | y > 5                         = False
    | getState board x y == Full    = checkVertImpl board x (y + 1) (count + 1)
    | getState board x y == Empty   = checkVertImpl board x (y + 1) 0

checkHorizontal :: [[State]] -> Integer -> Integer -> Bool
checkHorizontal board x y =  checkHorImpl board 0 y 0

checkHorImpl :: [[State]] -> Integer -> Integer -> Integer -> Bool
checkHorImpl board x y count
    | count >= 4                    = True
    | x > 6                         = False
    | getState board x y == Full    = checkHorImpl board (x + 1) y (count + 1)
    | getState board x y == Empty   = checkHorImpl board (x + 1) y 0

getState :: [[State]] -> Integer -> Integer -> State
getState board x y = getColValue (getRowValue board y) x

getRowValue :: [[State]] -> Integer -> [State]
getRowValue (x : xs) row 
    | row == 0  = x
    | otherwise = getRowValue xs (row - 1)
getRowValue [] row = []

getColValue :: [State] -> Integer -> State
getColValue (x : xs) col 
    | col == 0  = x
    | otherwise = getColValue xs (col - 1)
getColValue [] col = Empty

update :: [[State]] -> Integer -> Integer -> [[State]]
update (x : xs) col row
    | row == 0  = (updateRow x col) : xs
    | otherwise = x : (update xs col (row - 1))
update [] col row = []

updateRow :: [State] -> Integer -> [State]
updateRow (x : xs) i 
    | i == 0    = Full : xs
    | otherwise = x : (updateRow xs (i - 1))
updateRow [] i = []

parseMove :: String -> [Integer]
parseMove (x : xs)  
    | x == ' '  = parseMove xs
    | x == ','  = parseMove xs
    | otherwise = (charToInt x) : parseMove xs
parseMove [] = []

printLine [] = putStr "\n"
printLine (x : xs) = do 
                     putStr (stateToString x) 
                     printLine xs

printBoard [] = return()
printBoard (x : xs) = do 
                      printLine x
                      printBoard xs

stateToString :: State -> String
stateToString Empty = " "
stateToString _     = "x"

charToInt :: Char -> Integer
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6 
charToInt '7' = 7 
charToInt '8' = 8 
charToInt '9' = 9
charToInt _   = error("Cannot convert to int")
