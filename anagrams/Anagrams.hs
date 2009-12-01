{-
    Anagrams: Determines the anagrams within a given wordlist.
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
import Data.Char
import qualified Data.List as List
import qualified Data.Map as Map
import System.Environment
import System.IO
import System.Random


hashToAnagrams :: [String] -> Map.Map String [String]
hashToAnagrams xs = Map.filter (\a -> 1 /= length a) $ foldr insert Map.empty xs
  where
    insert word = Map.insertWith (++) (hash word) [word]


hash :: String -> String
hash xs = List.sort $ List.map toLower $ List.filter isLetter xs


displayStatistics :: Map.Map String [String] -> IO ()
displayStatistics anagramHashes = do
    putStrLn $ "There are " ++ (show $ Map.size anagramHashes) ++ " sets of anagrams"
    putStrLn $ "The highest order anagrams are: " ++ (show highestOrderAnagrams)
    putStrLn $ "The largest anagrams in length are: " ++ (show highestLengthAnagrams)
    return ()
  where
    highestOrderAnagrams = snd $ foldr accumulateHighestOrderAnagrams (0, []) anagrams
    accumulateHighestOrderAnagrams anagramSet accum@(bestLength, collection)
      | length anagramSet < bestLength = accum
      | length anagramSet == bestLength = (bestLength, anagramSet : collection)
      | length anagramSet > bestLength = (length anagramSet, [anagramSet])
    anagrams = snd $ unzip $ Map.toList anagramHashes
    highestLengthAnagrams = snd $ foldr accumulateLongestAnagrams (0, []) $ Map.toList anagramHashes
    accumulateLongestAnagrams hashAnagramSetPair accum@(bestLength, collection)
      | (length $ fst hashAnagramSetPair) < bestLength = accum
      | (length $ fst hashAnagramSetPair) == bestLength = (bestLength, (snd hashAnagramSetPair) : collection)
      | (length $ fst hashAnagramSetPair) > bestLength = (length $ fst hashAnagramSetPair, [snd hashAnagramSetPair])


--
-- displays various information about anagrams
-- requires a wordlist, where each word occupies a single line
-- invocation example:
--   runghc Anagrams.hs wordlist.txt
--
main :: IO ()
main = do
    (file:_) <- getArgs
    contents <- readFile file;
    displayStatistics $ hashToAnagrams $ lines contents
    return ()
