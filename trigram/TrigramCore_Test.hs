{-
    TrigramCore_Test: Tests for the trigram generator.
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
module TrigramCore_Test where

import TrigramCore
import Test.HUnit

import Data.Map
import Data.Maybe
import System.Random


-- useful test fixtures
emptyWords :: [String]
emptyWords = []

manyWords :: [String]
manyWords = replicate 100000 "large"


trigramMapFromWordsTests = TestLabel "trigramMapFromWords tests" $ TestList [
      TestCase $ assertEqual
        "Should get empty map from empty list"
        Data.Map.empty $ trigramMapFromWords emptyWords
      ,
      TestCase $ assertEqual
        "Should not run out of stack space..."
        [(("large", "large"), drop 2 manyWords)] $ toList (trigramMapFromWords manyWords)
    ]


takeRandomTests = TestLabel "takeRandom tests" $ TestList [
      TestCase $ assertEqual
        "Should take nothing from an empty list"
        True $ isNothing $ takeRandom emptyWords (mkStdGen 1234)
      ,
      TestCase $ assertEqual
        "Should take the only string in the list"
        "only" $ fst $ fromJust $ takeRandom ["only"] (mkStdGen 1234)
      ,
      TestCase $ assertEqual
        "Should take the first string in the list"
        "one" $ fst $ fromJust $ takeRandom ["one", "two"] (mkStdGen 4467821)
      ,
      TestCase $ assertEqual
        "Should take the second string in the list"
        "two" $ fst $ fromJust $ takeRandom ["one", "two"] (mkStdGen 1234)
    ]


spaceGraduallyTests = TestLabel "spaceGradually tests" $ TestList [
      TestCase $ assertEqual
        "No strings should result in no strings"
        emptyWords $ spaceGradually emptyWords
      ,
      TestCase $ assertEqual
        "One strings should result in one strings"
        ["one"] $ spaceGradually ["one"]
      ,
      TestCase $ assertEqual
        "Two strings should result in two strings"
        ["one", "one two"] $ spaceGradually ["one", "two"]
      ,
      TestCase $ assertEqual
        "Three strings should result in three strings"
        ["one", "one two", "one two three"] $ spaceGradually ["one", "two", "three"]
    ]


potentialLinesTests = TestLabel "potentialLines tests" $ TestList [
      TestCase $ assertEqual
        "Empty list should result in empty list"
        [] $ potentialLines emptyWords
      ,
      TestCase $ assertEqual
        "List with one element should result in list of one tuple"
        [("one", [])] $ potentialLines ["one"]
      ,
      TestCase $ assertEqual
        "List with two elements should result in list with two tuples"
        [("one", ["two"]), ("one two", [])] $ potentialLines ["one", "two"]
      ,
      TestCase $ assertEqual
        "List with three elements should result in list with two three"
        [("one", ["two", "three"]), ("one two", ["three"]), ("one two three", [])] $ potentialLines ["one", "two", "three"]
    ]


takeLineTests = TestLabel "takeLine tests" $ TestList [
      TestCase $ assertEqual
        "Strings should be exactly the length of a line"
        ("", emptyWords) $ takeLine 10 []
      ,
      TestCase $ assertEqual
        "Strings should be exactly the length of a line"
        ("This is a test", emptyWords) $ takeLine 14 ["This", "is", "a", "test"]
    ]


prettifyTests = TestLabel "prettify tests" $ TestList [
      TestCase $ assertEqual
        "Should be empty string"
        "\n" $ prettify emptyWords 80
      ,
      TestCase $ assertEqual
        "String forced despite line length"
        "Hello\n" $ prettify ["Hello"] 3
      ,
      TestCase $ assertEqual
        "String (without newline) should be exactly the length of a line"
        "This is a test\n" $ prettify ["This", "is", "a", "test"] 14
      ,
      TestCase $ assertEqual
        "Line should be broken according to length"
        "This\nis a\ntest\n" $ prettify ["This", "is", "a", "test"] 6
    ]


main = runTestTT $ TestList [
      trigramMapFromWordsTests,
      takeRandomTests,
      spaceGraduallyTests,
      potentialLinesTests,
      takeLineTests,
      prettifyTests
    ]

