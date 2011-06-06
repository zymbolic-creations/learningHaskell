{-
    Poker2: Solves problem 54 of "Project Euler".
    See http://projecteuler.net/index.php?section=problems&id=54
    Copyright (C) 2011  Mark Buer

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

import qualified Data.List as List
import qualified System.Environment as Env

--
-- Data type definitions
--

data Suit =
    Hearts |
    Diamonds |
    Clubs |
    Spades
  deriving (Eq, Show)

data Face =
    Two |
    Three |
    Four |
    Five |
    Six |
    Seven |
    Eight |
    Nine |
    Ten |
    Jack |
    Queen |
    King |
    Ace
  deriving (Ord, Eq, Enum, Show)

data Card =
    Card {
      face :: Face,
      suit :: Suit
    }
  deriving (Eq, Show)

instance Ord Card
  where
    compare a b = compare (face a) (face b)

data Match =
    Match {
      player1Hand :: [Card],
      player2Hand :: [Card]
    }
  deriving (Eq, Show)

data Rank =
    HighCard { faces :: [Face] } |
    OnePair { faces :: [Face] } |
    TwoPairs { faces :: [Face] } |
    ThreeOfAKind  { faces :: [Face] } |
    Straight { faces :: [Face] } |
    Flush { faces :: [Face] } |
    FullHouse { faces :: [Face] } |
    FourOfAKind { faces :: [Face] } |
    StraightFlush { faces :: [Face] } |
    RoyalFlush { faces :: [Face] }
  deriving (Ord, Eq, Show)

--
-- Hand categorisation
--

rank :: [Card] -> Rank
rank cards

  | isFlush && isStraight && head descendingFaces == Ace
    = RoyalFlush $ descendingFaces -- Not really necessary...

  | isFlush && isStraight
    = StraightFlush $ descendingFaces

  | (fst $ head faceOccurrences) == 4
    = FourOfAKind $ map (snd) faceOccurrences

  | map (fst) faceOccurrences == [3, 2]
    = FullHouse $ descendingFaces -- this is less than optimal... but who cares?

  | isFlush
    = Flush $ descendingFaces

  | isStraight
    = Straight $ descendingFaces

  | (fst $ head faceOccurrences) == 3
    = ThreeOfAKind $ map (snd) faceOccurrences

  | map (fst) faceOccurrences == [2, 2, 1]
    = TwoPairs $ map (snd) faceOccurrences

  | (fst $ head faceOccurrences) == 2
    = OnePair $ map (snd) faceOccurrences

  | otherwise
    = HighCard $ descendingFaces

  where
    isStraight = (List.length uniqueFaces == 5) && (largestValue == smallestValue + 4)
      where
        largestValue = fromEnum $ uniqueFaces !! 0
        smallestValue = fromEnum $ uniqueFaces !! 4
    isFlush = List.length uniqueSuits == 1
    -- [(occurenceCount, Face)] sorted by descending occurenceCount then descending Face
    faceOccurrences = List.reverse $ List.sort unsortedFaceOccurrences
      where
        unsortedFaceOccurrences = map (\faces -> (List.length faces, head faces)) $ List.group descendingFaces
    uniqueFaces = List.nub $ descendingFaces
    descendingFaces = List.reverse $ List.sort $ List.map (face) cards
    uniqueSuits = List.nub $ List.map (suit) cards

--
-- Player vs player
--

matchOutcome :: Match -> Ordering
matchOutcome match = compare (rank $ player1Hand match) (rank $ player2Hand match)

player1WinCount :: [Match] -> Int
player1WinCount matches = List.length $ List.filter (== GT) matchOutcomes
  where
    matchOutcomes = List.map (matchOutcome) matches

--
-- File parsing
--

matchesFromStrings :: [String] -> [Match]
matchesFromStrings matches = map (matchFromString) matches
  where
    matchFromString :: String -> Match
    matchFromString s = matchFromHands handsFromString
      where
        matchFromHands (p1h, p2h) = Match p1h p2h
        handsFromString = List.splitAt 5 $ cardsFromString s

        cardsFromString :: String -> [Card]
        cardsFromString s = map cardFromString $ words s

        cardFromString :: String -> Card
        cardFromString (vc:sc:_) = Card (faceFromString [vc]) (suitFromString [sc])

        faceFromString :: String -> Face
        faceFromString "A" = Ace
        faceFromString "K" = King
        faceFromString "Q" = Queen
        faceFromString "J" = Jack
        faceFromString "T" = Ten
        faceFromString s = toEnum $ (read s - 2)
        suitFromString :: String -> Suit
        suitFromString "H" = Hearts
        suitFromString "D" = Diamonds
        suitFromString "C" = Clubs
        suitFromString "S" = Spades

--
-- Counts the number of winning hands of player1
-- options:
--  inputFile
-- example invocation:
--  runghc Poker2.hs poker.txt
--
main :: IO ()
main = do
    (file:_) <- Env.getArgs
    contents <- readFile file;
    putStr $ "Player 1 had " ++ (show $ wins contents) ++ " winning hands\n"
    return ()
  where
    wins tournamentString = player1WinCount $ matchesFromStrings $ lines tournamentString
