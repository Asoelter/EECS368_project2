import Control.Monad  
import Data.Char  
import System.IO
  
data State = Empty | Player1 | Player2 deriving(Eq, Show)

main = do  
    let board = [[Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                 [Empty, Empty, Empty, Empty, Empty, Empty, Empty]]
    printBoard board
    runGame board Player1

runGame board state = do
                putStr (title state)
                putStr " Enter the row to drop the piece into: " 
                hFlush stdout
                moveStr <- getLine
                let move = parseMove moveStr
                let x = move !! 0
                let y = lowestEmpty board x
                let newBoard = (update board x y state)
                printBoard newBoard
                if not (winningMove newBoard state x y)
                   then (runGame newBoard (otherPlayer state))
                else putStr "Game over\n"

title :: State -> String
title Empty     = "Error!"
title Player1   = "Player 1"
title Player2   = "Player 2"

otherPlayer :: State -> State
otherPlayer Empty   = error("Cannot get other state of empty")
otherPlayer Player1 = Player2
otherPlayer Player2 = Player1

lowestEmpty :: [[State]] -> Integer -> Integer
lowestEmpty board x = lowestEmptyImpl board x 5

lowestEmptyImpl :: [[State]] -> Integer -> Integer -> Integer
lowestEmptyImpl board x y 
    | getState board x y == Empty = y
    | getState board x y == Player1 = lowestEmptyImpl board x (y - 1)
    | getState board x y == Player2 = lowestEmptyImpl board x (y - 1)

winningMove :: [[State]] -> State -> Integer -> Integer -> Bool
winningMove board state x y = checkHorizontal board state x y || checkVertical board state x y

checkVertical :: [[State]] -> State -> Integer -> Integer -> Bool
checkVertical board state x y = checkVertImpl board state x 0 0

checkVertImpl :: [[State]] -> State -> Integer -> Integer -> Integer -> Bool
checkVertImpl board state x y count 
    | count >= 4                    = True
    | y > 5                         = False
    | getState board x y == state   = checkVertImpl board state x (y + 1) (count + 1)
    | getState board x y /= state   = checkVertImpl board state x (y + 1) 0

checkHorizontal :: [[State]] -> State -> Integer -> Integer -> Bool
checkHorizontal board state x y =  checkHorImpl board state 0 y 0

checkHorImpl :: [[State]] -> State -> Integer -> Integer -> Integer -> Bool
checkHorImpl board state x y count
    | count >= 4                    = True
    | x > 6                         = False
    | getState board x y == state = checkHorImpl board state (x + 1) y (count + 1)
    | getState board x y /= state = checkHorImpl board state (x + 1) y 0

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

update :: [[State]] -> Integer -> Integer -> State -> [[State]]
update (x : xs) col row state
    | row == 0  = (updateRow x col state) : xs
    | otherwise = x : (update xs col (row - 1) state)
update [] col row state = []

updateRow :: [State] -> Integer -> State -> [State]
updateRow (x : xs) i state
    | i == 0    = state : xs
    | otherwise = x : (updateRow xs (i - 1) state)
updateRow [] i state = []

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
stateToString Empty     = " "
stateToString Player1   = "x"
stateToString Player2   = "o"

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
