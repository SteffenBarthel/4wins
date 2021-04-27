module Main where

import Lib
import Control.Applicative
import qualified Data.Map as Map

main :: IO ()
main =
    startGame

startGame = do
    putStrLn "Press enter to start/restart the game."
    output <- getLine
    let playerStore = defaultPlayer
    let mapStore = defaultMap
    if output == "" 
        then (game playerStore mapStore)
        else startGame  


game playerStore mapStore = do
    putStrLn "Game started."
    (gameCircle playerStore mapStore)

gameCircle playerStore mapStore = do
    putStrLn ("Spieler an der Reihe: " ++ show (printCurrentPlayer playerStore))
    (printCurrentMap mapStore)
    let mapStore2 = (playerMove playerStore mapStore)
    let playerStore2 = (switchPlayer playerStore)
    if ((checkGameStatus 1) == 1)
        then (gameCircle playerStore2 mapStore)
        else startGame

checkGameStatus x
    | x == 1 = 1
    | otherwise = 0

defaultPlayer =
    [(1, 1), (2, 0)]

defaultPlayerTwo =
    [(1, 0), (2, 1)]

defaultMap = 
    [
        [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)],
        [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)],
        [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)],
        [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)],
        [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)],
        [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)]
    ]

printCurrentPlayer playerStore =
    head [fst x | x <- playerStore, snd x == 1]

switchPlayer playerStore =
    if ((printCurrentPlayer playerStore) == 1)
        then defaultPlayerTwo
        else defaultPlayer

printCurrentMap mapStore = do
    print (concat ((translateCurrentMap (mapStore!!0)) ++ [" | "]))
    print (concat ((translateCurrentMap (mapStore!!1)) ++ [" | "]))
    print (concat ((translateCurrentMap (mapStore!!2)) ++ [" | "]))
    print (concat ((translateCurrentMap (mapStore!!3)) ++ [" | "]))
    print (concat ((translateCurrentMap (mapStore!!4)) ++ [" | "]))
    print (concat ((translateCurrentMap (mapStore!!5)) ++ [" | "]))

translateCurrentMap mapPiece = do
    [(" | " ++ (mapGuard (snd x)))| x <- mapPiece]

mapGuard x
    | x == 1 = "X"
    | x == 2 = "O"
    | otherwise = "-"

playerMove playerStore mapStore = do
    putStrLn "Chose a number between 1 and 7"
    move <- getLine
    newLines
    if (checkPlayerMove move) == 1
        then do
            return (map (\value -> if (value) == ((translatePlayerMove move), 0) then [((translatePlayerMove move), (printCurrentPlayer playerStore))] else [value]) (reverse(mapStore!!5)))
        else do
            print "Move invalid."
            return (mapStore)

checkPlayerMove move
    | move == "1" = 1
    | move == "2" = 1
    | move == "3" = 1
    | move == "4" = 1
    | move == "5" = 1
    | move == "6" = 1
    | move == "7" = 1
    | otherwise = 0

translatePlayerMove move
    | move == "1" = 1
    | move == "2" = 2
    | move == "3" = 3
    | move == "4" = 4
    | move == "5" = 5
    | move == "6" = 6
    | move == "7" = 7

newLines = do
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn ""