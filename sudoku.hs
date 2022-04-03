#!/usr/bin/env runhaskell
{-
sudoku-solver script. Copyright Matt Colligan 2022.
-}

import Control.Applicative (liftA2)
import Data.Array (Array, Ix, assocs, elems, listArray, (!), (//))
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.List (nub, sort)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import System.Environment (getArgs)

import Debug.Trace

puz p = trace (showPuzzle p) p

-- Puzzle #1
puzzle :: Puzzle
puzzle =
    asArray
        [ ((One, One), Just Five)
        , ((One, Two), Just Six)
        , ((One, Four), Just Eight)
        , ((One, Nine), Just Nine)
        , ((Two, Three), Just Three)
        , ((Two, Five), Just Two)
        , ((Two, Six), Just Six)
        , ((Two, Eight), Just One)
        , ((Three, Nine), Just Three)
        , ((Four, Six), Just Nine)
        , ((Four, Seven), Just Five)
        , ((Five, Three), Just Eight)
        , ((Five, Four), Just Five)
        , ((Five, Seven), Just One)
        , ((Six, Three), Just Four)
        , ((Six, Four), Just Three)
        , ((Seven, One), Just Three)
        , ((Seven, Seven), Just Nine)
        , ((Eight, One), Just Six)
        , ((Eight, Four), Just Two)
        , ((Eight, Five), Just One)
        , ((Eight, Six), Just Four)
        , ((Nine, Seven), Just Eight)
        , ((Nine, Nine), Just Four)
        ]

-- Converts the list of values above into an array of maybe values
asArray :: [(Index, Maybe Cell)] -> Puzzle
asArray vals = listArray ((One, One), (Nine, Nine)) (repeat Nothing) // vals

type Puzzle = Array Index (Maybe Cell)

-- This is the set of possible values, rows and columns.
data Cell = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    deriving (Eq, Ord, Enum, Ix, Show)

set :: [Cell]
set = [One .. Nine]

-- An index into the array is (row, col).
type Index = (Cell, Cell)

{-
Test if the indices specified by cartesian product of row and column indices contain the
values One to Nine, one each, and nothing else.
-}
goodSet :: Puzzle -> [Index] -> Bool
goodSet p = noRepeats . catMaybes . map ((!) p)
  where
    noRepeats cs = length cs == length (nub cs)

{-
Is the given puzzle valid, considering only the cells that do have values?

Possible improvement: don't test every single row/column/box when making a change.
-}
isValid :: Puzzle -> Bool
isValid p = all (goodSet p) blocks

blocks :: [[Index]]
blocks = concat [rows, cols, boxes]
  where
    rows :: [[Index]]
    rows = map (\r -> map ((,) r) set) set

    cols :: [[Index]]
    cols = map (\c -> map (flip (,) c) set) set

    boxes :: [[Index]]
    boxes = map makeBox [(r, c) | r <- [One, Four, Seven], c <- [One, Four, Seven]]

    makeBox :: Index -> [Index]
    makeBox (r, c) = [(r, c) | r <- [r .. succ . succ $ r], c <- [c .. succ . succ $ c]]

{-
Is this puzzle completely filled in with values?
-}
isComplete :: Puzzle -> Bool
isComplete = (==) 81 . length . catMaybes . elems

{-
Solve the given puzzle!
-}
solve :: Puzzle -> Maybe Puzzle
solve p = go [(p, (One, One), One)] (Just (One, One)) One
  where
    {-
    The first argument here is the stack of steps taken since the beginning. Each
    element of the list is the new state of the puzzle, and the index + new value
    inserted to get there. pc was put at pi to make p. The remaining 2 argments are what
    we are going to be testing this iteration.
    -}
    go :: [(Puzzle, Index, Cell)] -> Maybe Index -> Cell -> Maybe Puzzle
    -- We are finished when the maybe index is Nothing.
    go ((p, _, _):_) Nothing _ = Just p
    go pps@((p, pi, pc) : ps) (Just i) c
        -- This index is already filled - continue.
        | isJust (p ! i) = go ((puz p, pi, pc):ps) (next i) One
        -- This index is empty - let's find a possibly valid value.
        | otherwise =
            case getOptions p i c of
                [] -> Nothing
                opts -> listToMaybe . mapMaybe (\o -> go ((puz o, i, c):pps) (next i) One) $ opts

    getOptions :: Puzzle -> Index -> Cell -> [Puzzle]
    getOptions p i c = filter isValid . map (\c -> p // [(i, Just c)]) $ [c .. Nine]

    -- Maybe-get the next Index, left to right, then top to bottom
    next :: Index -> Maybe Index
    next (Nine, Nine) = Nothing
    next (row, Nine) = Just (succ row, One)
    next (row, col) = Just (row, succ col)

{-
Graphically represent the puzzle layout.
-}
showPuzzle :: Puzzle -> String
showPuzzle = (++) " " . unwords . map organise . assocs . fmap render
  where
    render Nothing = " "
    render (Just v) = show . (+) 1 . fromEnum $ v

    -- Check out this ugly stuff. It adds bars and newlines to render the puzzle.
    organise ((_, Three), c) = c ++ " |"
    organise ((_, Six), c) = c ++ " |"
    organise ((Three, Nine), c) = c ++ "\n " ++ replicate 21 '-' ++ "\n"
    organise ((Six, Nine), c) = c ++ "\n " ++ replicate 21 '-' ++ "\n"
    organise ((_, Nine), c) = c ++ "\n"
    organise (_, c) = c ++ ""

{-
Load a puzzle from file.
-}
loadPuzzle :: [Char] -> Puzzle
loadPuzzle = asArray . zip indices . map parse . filter (`elem` chars)
  where
    chars = [' '] ++ ['1'..'9']
    indices = [(r, c) | r <- set, c <- set]

    parse :: Char -> Maybe Cell
    parse ' ' = Nothing
    parse char = Just . toEnum . subtract 1 . digitToInt $ char

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ bool (head args) "puzzle_1.txt" (null args)
    let puzzle = loadPuzzle contents
    putStrLn "Puzzle:"
    putStrLn . showPuzzle $ puzzle

    case solve puzzle of
        Nothing -> 
            putStrLn "Failed! Is the puzzle valid?"
        Just solved -> do
            putStrLn "Completed:"
            putStrLn . showPuzzle $ solved
