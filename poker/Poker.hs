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

rank :: [Card] -> [Face]
rank ((orderBy [same 2 face, same 2 face]) -> Just [p1, _, p2, _, s]) = [face $ max p1 p2, face $ min p1 p2, face s]

--groupAs [same 3 face, same 2 face] -> Just (a, _, _, _, _)
--groupAs [same 5 suit] -> Just (a, b, c, d, e)

--groupAs [a, b, c, d, e] if face a == face b && face c == face d -> Just (a:_:c:_e)

pc :: Card -> Bool
pc Card {face = Seven} = True
pc _ = False


data Criteria =
    Joint {
      count :: Int,
      dualPredicate :: Card -> Card -> Bool
    } |
    Disjoint {
      count :: Int,
      singlePredicate :: Card -> Bool
    }


--straight :: Int -> Criteria
--straight c = Joint c (\c1 c2 -> valueOf face c1 == 1 + (valueOf face c2))

same :: (Eq a) => Int -> (Card -> a) -> Criteria
same c s = Joint c (\c1 c2 -> s c1 == s c2)

wild :: Int -> Criteria
wild c = Disjoint c (\_ -> True)

--checkSomeHand :: [Card] -> Int
--checkSomeHand (orderBy (Criteria 3 (\_ _ -> True)) -> 6) = 8
--checkSomeHand (orderBy (same 4 face) -> 6) = 8


orderBy :: [Criteria] -> [Card] -> Maybe [Card]
orderBy criterium cs = List.find (predicateTest) $ permutations cs
  where
    predicateTest cards = List.and $ List.zipWith (applyCriteria) criterium splits
      where
        splits = splitAtMany (counts) cards
        counts = List.map (count) criterium

applyCriteria :: Criteria -> [Card] -> Bool
applyCriteria (Joint _ p) cards = List.and $ List.zipWith (p) cards $ tail cards
applyCriteria (Disjoint _ p) cards = List.and $ List.map (p) cards

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


checkStraight :: [Card] -> Maybe Card
checkStraight cards =
  case (List.sort cards) of
    [Card Two _, Card Three _, Card Four _, highest@(Card Five _), Card Ace _] -> Just highest
    [Card Two _, Card Three _, Card Four _, Card Five _, highest@(Card Six _)] -> Just highest
    [Card Three _, Card Four _, Card Five _, Card Six _, highest@(Card Seven _)] -> Just highest
    [Card Four _, Card Five _, Card Six _, Card Seven _, highest@(Card Eight _)] -> Just highest
    [Card Five _, Card Six _, Card Seven _, Card Eight _, highest@(Card Nine _)] -> Just highest
    [Card Six _, Card Seven _, Card Eight _, Card Nine _, highest@(Card Ten _)] -> Just highest
    [Card Seven _, Card Eight _, Card Nine _, Card Ten _, highest@(Card Jack _)] -> Just highest
    [Card Eight _, Card Nine _, Card Ten _, Card Jack _, highest@(Card Queen _)] -> Just highest
    [Card Nine _, Card Ten _, Card Jack _, Card Queen _, highest@(Card King _)] -> Just highest
    [Card Ten _, Card Jack _, Card Queen _, Card King _, highest@(Card Ace _)] -> Just highest
    otherwise -> Nothing
