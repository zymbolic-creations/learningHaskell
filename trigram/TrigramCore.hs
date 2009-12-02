{-
    TrigramCore: Generates trigram stories from a given body of text.
    Copyright (C) 2009  Mark Buer

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module TrigramCore (
    trigramMapFromWords,
    takeRandom,
    spaceGradually,
    potentialLines,
    prettify,
    takeLine
  ) where

import Data.List
import Data.Map
import System.Environment
import System.IO
import System.Random


-- given a list of words, returns a map of word tuples to lists of trailing words
trigramMapFromWords :: (Ord a) => [a] -> Map (a, a) [a]
trigramMapFromWords xs = foldr insert empty triplets
  where
    insert (e1, e2, e3) = insertWith (++) (e1, e2) [e3]
    triplets = zip3 xs (tail xs) (tail (tail xs))

-- given a starting pair of words, a map of pairs to words and a random generator,
-- returns a "story" consisting of the two given words and as many trailing words
-- as is possible (might be infinite)
story :: (RandomGen g, Ord a) => (a, a) -> Map (a, a) [a] -> g -> [a]
story pair@(word1, word2) trailingMap g = word1 : word2 : unfoldr takeAnother (pair, g)
  where
    takeAnother (pair@(_, w2), g') =
      case (randomWord pair g') of
        Nothing -> Nothing
        Just (w3, g'') -> Just (w3, ((w2, w3), g''))
    randomWord pair g' =
      case (possibleWords pair) of
        Nothing -> Nothing
        Just xs -> takeRandom xs g'
    possibleWords pair = Data.Map.lookup pair trailingMap

-- takes a random element from a list.
takeRandom :: RandomGen g => [a] -> g -> Maybe (a, g)
takeRandom [] _ = Nothing
takeRandom elements g = Just (elements !! (fst result), snd result)
  where
    result = randomR (0, (length elements) - 1) g

-- converts a list of strings to a nicely formatted string (spaces and line breaks)
prettify :: [String] -> Int -> String
prettify xs lineLength =
    (intercalate "\n" (TrigramCore.group (takeLine lineLength) xs)) ++ "\n"

-- groups items in a list
group :: ([a] -> (b, [a])) -> [a] -> [b]
group f [] = []
group f xs = (fst grouping) : TrigramCore.group f (snd grouping)
  where
    grouping = f xs

-- given a desired line length and a list of words, returns a string that is a
-- single line, and also returns the unused words
takeLine :: Int -> [String] -> (String, [String])
takeLine lineLength [] = ("", [])
takeLine lineLength xs =
    case allowedAssemblies of
      -- no line was short enough, use first element anyway
      [] -> (head xs, tail xs)
      es -> last es
  where
    allowedAssemblies = takeWhile (\(line, _) -> length line <= lineLength) assemblies
    assemblies = potentialLines xs

-- increasing line lengths alongside decreasing unused words
potentialLines :: [String] -> [(String, [String])]
potentialLines xs = zip (spaceGradually xs) (tails (tail xs))

-- returns a String that is the concatination of each String in a list,
-- with spaces inserted between each String.
spaceGradually :: [String] -> [String]
spaceGradually [] = []
spaceGradually (x:xs) = scanl (\a b -> a ++ " " ++ b) x xs


-- Generates a trigram story based on a given body of text
-- options:
--  inputFile word1 word2 maximumOutputWords seed
-- example invocation:
--  runghc trigrams.hs sherlock.txt I had 100 1234
main :: IO ()
main = do
    (file:word1:word2:sizeS:seedS:_) <- getArgs
    contents <- readFile file;
    putStr $ prettify (generateStory contents word1 word2 (read seedS) (read sizeS)) 70 
    return ()
  where
    basis contents = trigramMapFromWords (words contents)
    generateStory contents word1 word2 seed size =
      take size $ story (word1, word2) (basis contents) (mkStdGen seed)

