{-
    RingOfDeath: Puzzle solution where every nth person (circular) is shot.
    Copyright (C) 2010  Mark Buer

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
module RingOfDeath (
    findSoleSurvivor,
    findSurvivors
  ) where

-- This solution is longer than need be on account of the error checking.

removeCyclicNth :: [a] -> Int -> [a]
removeCyclicNth xs n = take (length xs - 1) $ drop n $ cycle xs

findSoleSurvivor :: [a] -> Int -> a
findSoleSurvivor [] _ = error "There must be some people initially!"
findSoleSurvivor _ n
    | n < 1 = error "The number of people to skip must be positive!"
findSoleSurvivor [x] _ = x
findSoleSurvivor xs n = findSoleSurvivor (removeCyclicNth xs n) n

findSurvivors :: [a] -> Int -> [a]
findSurvivors xs n
    | length xs < n = xs
    | otherwise = findSurvivors (removeCyclicNth xs n) n
