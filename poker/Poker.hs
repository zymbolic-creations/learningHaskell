{-
    PokerHand: Compares one poker hand with another.
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
import qualified Data.List as List

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
  deriving (Ord, Eq, Enum, Show, Bounded)

data Card =
    Card {
      face :: Face,
      suit :: Suit
    }
  deriving (Eq, Show)

instance Ord Card
  where
    compare a b = compare (face a) (face b)


rank :: [Card] -> [Face]

-- StraightFlush

-- FourOfAKind
rank (match [same 4 face, wild 1]
    -> Just [f, _, _, _, _]) = [face f]

-- FullHouse
rank (match [same 3 face, same 2 face]
    -> Just [t, _, _, _, _]) = [face t]

-- Flush
rank (match [same 5 suit]
    -> Just cards) = decreasingFaces cards

-- Straight
rank (match [is (\c -> face c == Ace), run 4]
    -> Just [_, _, _, _, h]) = [face h]
rank (match [run 5]
    -> Just [_, _, _, _, h]) = [face h]

-- ThreeOfAKind
rank (match [same 3 face, wild 2]
    -> Just [t, _, _, _, _]) = [face t]

-- TwoPairs
rank (match [same 2 face, same 2 face, wild 1]
    -> Just [p1, _, p2, _, s]) = [face $ max p1 p2, face $ min p1 p2, face s]

-- Pair
rank (match [same 2 face, wild 3]
    -> Just (p:_:others)) = (face p):(decreasingFaces others)

-- HighCard
rank (match [wild 5]
    -> Just cards) = decreasingFaces cards


decreasingFaces :: [Card] -> [Face]
decreasingFaces cards = List.reverse $ List.sort $ faces cards

faces :: [Card] -> [Face]
faces cards = List.map (face) cards


data Criterion =
    Joint {
      count :: Int,
      dualPredicate :: Card -> Card -> Bool
    } |
    Disjoint {
      count :: Int,
      singlePredicate :: Card -> Bool
    }

same :: (Eq a) => Int -> (Card -> a) -> Criterion
same c s = Joint c (\c1 c2 -> s c1 == s c2)

run :: Int -> Criterion
run c = Joint c (\c1 c2 -> (face c1 /= (maxBound::Face)) && (succ $ face c1 == face c2))

is :: (Card -> Bool) -> Criterion
is p = Disjoint 1 p

wild :: Int -> Criterion
wild c = Disjoint c (\_ -> True)


match :: [Criterion] -> [Card] -> Maybe [Card]
match criteria cs = List.find (predicateTest) $ permutations cs
  where
    predicateTest cards = List.and $ List.zipWith (checkCriterion) criteria splits
      where
        splits = splitAtMany (counts) cards
        counts = List.map (count) criteria

checkCriterion :: Criterion -> [Card] -> Bool
checkCriterion (Joint _ p) cards = List.and $ List.zipWith (p) cards $ tail cards
checkCriterion (Disjoint _ p) cards = List.and $ List.map (p) cards


splitAtMany :: [Int] -> [a] -> [[a]]
splitAtMany counts elems = List.unfoldr splitNext (counts, elems)
  where
    splitNext ([], _) = Nothing
    splitNext (c:cs, []) = Just ([], (cs, []))
    splitNext (c:cs, xs) = Just (fst split', (cs, snd split'))
      where
        split' = List.splitAt c xs


permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)

