module Game where

import Text.Read

-- This module acts as a barebones shell of chess. Soley focused on things like legality of moves.
-- No "intelligence" here.

-- We should think about eventually abstracting out an interface for boards, so that we can swap in
-- more efficient implementations, like something with bitboards.

data Piece = Pawn | Rook | Knight | Bishop | Queen | King | Empty
data Player = Black | White

data Board = Board { currentPlayer :: Player
                   , blackHasCastled :: Bool
                   , whiteHasCastled :: Bool
                   , blackEnPassantable :: [Bool] -- Corresponds to pawn in each column
                   , whiteEnPassantable :: [Bool]
                   , board :: [[Piece]]
                   }

pieceAtSquare :: String -> Board -> Maybe Piece
pieceAtSquare square board =
  if length square == 2 && head square `elem` ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
                        && (elem <$> (readMaybe [square !! 1] :: Maybe Int) <*> Just [1..8]) == Just True then
    let x = head $ map fst $ filter (\(i, c) -> head square == c) $ zip [0..7] ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
        y = Just ((readMaybe [square !! 1] :: Maybe Int) - 1)
    in x + y
  else
    Nothing
