{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- ** YOU CAN CHANGE ME **
#define USE_FINITE_INT 1


module Main where


import Data.Char (isLetter)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment
import Text.Read (readMaybe)

#if USE_FINITE_INT
import qualified Data.IntMap.Strict as Map
#else
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
#endif

-- Installation for 'numeral' package
-- Note: This was the hash on master that I cloned from:
--   'bcc7f8ac65876bfe3be75ea24cdb3b0e15a5102e'
-- $ cd some/dir/to/install/at
-- $ git clone 'https://github.com/roelvandijk/numerals.git'
-- $ cd numerals
-- $ cabal install
-- Ready '2' (two, too, to?) go!
import Text.Numeral.Grammar (Inflection, defaultInflection)
import qualified Text.Numeral.Language.ENG as ENG


--------------------------------------------------------------------------------


data English = UK | US
    deriving (Show, Read, Eq, Ord)


-- ** YOU CAN CHANGE ME **
english :: English
english = US


#if USE_FINITE_INT
type IntType = Int
type Graph = Map.IntMap Int
#else
type IntType = Integer
type Graph = Map.Map IntType Int
#endif


chosenCardinal :: Inflection -> IntType -> Maybe Text
chosenCardinal = case english of
    UK -> ENG.gb_cardinal
    US -> ENG.us_cardinal


letterCount :: IntType -> Int
letterCount num = case chosenCardinal defaultInflection num of
    Just txt -> let
        f n c = case isLetter c of
            True -> n + 1
            False -> n
        in Text.foldl' f 0 txt
    Nothing -> error "This should not happen in English."


buildGraph :: [IntType] -> Graph -> Graph
buildGraph nums g = case nums of
    num : nums' -> let
        count = letterCount num
        (g', depth) = case Map.lookup count g of
            Just depth -> (g, depth)
            Nothing -> let
                g'' = buildGraph [count] g
                in case Map.lookup count g'' of
                    Just depth -> (g'', depth)
                    Nothing -> error "should not happen"
        in buildGraph nums' $ Map.insert num (depth + 1) $ g'
    [] -> g


compute :: [IntType] -> (IntType, Int)
compute nums = let
    g = buildGraph nums $ Map.singleton 4 1
    f num (bestNum, bestCount) = let
        count = case Map.lookup num g of
            Just c -> c
            Nothing -> error "should not happen"
        in case count >= bestCount of
            True -> (num, count)
            False -> (bestNum, bestCount)
    in foldr f (0, 0) nums


main :: IO ()
main = do
    args <- getArgs
    case args of
        [str] -> case readMaybe str of
            Just (limit :: Integer) -> case readMaybe str of
                Just (limit' :: IntType) -> case show limit == show limit' of
                    True -> let
                        nums = case limit' < 0 of
                            True -> [limit' .. 0]
                            False -> [0 .. limit']
                        in print $ compute nums
                    False -> putStrLn "Please compile program with USE_FINITE_INT defined to 0 to support supplied number."
                Nothing -> error "should never happen"
            Nothing -> putStrLn "Please enter an integer."


