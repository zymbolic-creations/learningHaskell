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

data Card = Card {
      face :: Face,
      suit :: Suit
    } deriving (Eq, Show)

instance Ord Card
  where
    compare a b = compare (face a) (face b)

