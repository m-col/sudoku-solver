#!/usr/bin/env runhaskell

{-# LANGUAGE TupleSections #-}

{-
sudoku-solver script. Copyright Matt Colligan 2022.
-}

import Control.Applicative (liftA2)
import Data.Array (Array, Ix, assocs, elems, listArray, (!), (//))
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.List (nub, sort)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import System.Environment (getArgs)

{-
Screw dealing with IO when I can use trace. Sorry not sorry.
-}
import Debug.Trace

puz p = trace (showPuzzle p) p

type Puzzle = Array Index (Maybe Cell)

{-
This is the set of possible values, rows and columns.
-}
data Cell = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    deriving (Eq, Ord, Enum, Ix, Show)

set :: [Cell]
set = [One .. Nine]

-- An index into the array is (row, col).
type Index = (Cell, Cell)

{-
Is the given puzzle valid, considering only the cells that do have values? Possible
improvement: don't test every single row/column/box when making a change.
-}
isValid :: Puzzle -> Bool
isValid p = all (goodSet p) blocks
  where
    goodSet :: Puzzle -> [Index] -> Bool
    goodSet p = noRepeats . mapMaybe (p !)

    noRepeats cs = length cs == length (nub cs)

    blocks :: [[Index]]
    blocks = concat [rows, cols, boxes]
    rows = map (\r -> map (r,) set) set
    cols = map (\c -> map (,c) set) set
    boxes = [makeBox (r, c) | r <- [One, Four, Seven], c <- [One, Four, Seven]]

    makeBox :: Index -> [Index]
    makeBox (r, c) = [(r, c) | r <- [r .. succ . succ $ r], c <- [c .. succ . succ $ c]]

{-
Solve the puzzle!
-}
solve :: Puzzle -> Maybe Puzzle
solve p = go [(p, (One, One))] (Just (One, One))
  where
    {-
    The first argument here is the stack of steps taken since the beginning. Each
    element of the list is the new state of the puzzle, and the index that was filled to
    get there. The second argument index is where we are going to be testing this
    iteration.
    -}
    go :: [(Puzzle, Index)] -> Maybe Index -> Maybe Puzzle
    -- We are finished when the maybe index is Nothing.
    go ((p, _) : _) Nothing = Just p
    go pps@((p, pi) : ps) (Just i)
        -- This index is already filled - continue.
        | isJust (p ! i) = go ((puz p, pi) : ps) (next i)
        -- This index is empty - let's find a possibly valid value.
        | otherwise =
            case getOptions p i of
                [] -> Nothing
                opts -> listToMaybe . mapMaybe (\o -> go ((puz o, i) : pps) (next i)) $ opts

    getOptions :: Puzzle -> Index -> [Puzzle]
    getOptions p i = filter isValid . map (\c -> p // [(i, Just c)]) $ set

    -- Maybe-get the next Index, left to right, then top to bottom
    next :: Index -> Maybe Index
    next (Nine, Nine) = Nothing
    next (row, Nine) = Just (succ row, One)
    next (row, col) = Just (row, succ col)

{-
Graphically represent the puzzle layout.
-}
showPuzzle :: Puzzle -> String
showPuzzle = (<>) "\ESC[2J " . unwords . map organise . assocs . fmap render
  where
    render Nothing = " "
    render (Just v) = show . (+) 1 . fromEnum $ v

    -- Check out this ugly stuff. It adds bars and newlines to render the puzzle.
    organise ((_, Three), c) = c <> " |"
    organise ((_, Six), c) = c <> " |"
    organise ((Three, Nine), c) = c <> "\n " <> replicate 21 '-' <> "\n"
    organise ((Six, Nine), c) = c <> "\n " <> replicate 21 '-' <> "\n"
    organise ((_, Nine), c) = c <> "\n"
    organise (_, c) = c <> ""

{-
Load a puzzle from file.
-}
loadPuzzle :: [Char] -> Puzzle
loadPuzzle = asArray . zip indices . map parse . filter (`elem` chars)
  where
    chars = [' '] <> ['1' .. '9']
    indices = [(r, c) | r <- set, c <- set]

    parse :: Char -> Maybe Cell
    parse ' ' = Nothing
    parse char = Just . toEnum . subtract 1 . digitToInt $ char

    asArray :: [(Index, Maybe Cell)] -> Puzzle
    asArray vals = listArray ((One, One), (Nine, Nine)) (repeat Nothing) // vals

main :: IO ()
main = do
    args <- getArgs
    let file = bool (head args) "puzzle_1.txt" (null args)
    solved <- solve . loadPuzzle <$> readFile file
    putStrLn $
        if isNothing solved
            then "Failed! Is the puzzle valid?"
            else "Completed!"
