module ChessBoard where

data ChessBoardLetter = A | B | C | D | E | F | G | H

chessBoardNumber :: [Int]
chessBoardNumber = [1..8]

chessBoardLetters :: [ChessBoardLetter]
chessBoardLetters = [A,B,C,D,E,F,G,H]

type ChessBoardCoordinate = (Int, ChessBoardLetter)

nextLetter :: Maybe ChessBoardCoordinate-> Maybe ChessBoardCoordinate
nextLetter Nothing = Nothing
nextLetter (Just (c, A)) = Just (c, B)
nextLetter (Just (c, B)) = Just (c, C)
nextLetter (Just (c, C)) = Just (c, D)
nextLetter (Just (c, D)) = Just (c, E)
nextLetter (Just (c, E)) = Just (c, F)
nextLetter (Just (c, F)) = Just (c, G)
nextLetter (Just (c, G)) = Just (c, H)
nextLetter _ = Nothing

previousLetter :: Maybe ChessBoardCoordinate -> Maybe ChessBoardCoordinate
previousLetter Nothing = Nothing
previousLetter (Just (c, H)) = Just (c, G)
previousLetter (Just (c, G)) = Just (c, F)
previousLetter (Just (c, F)) = Just (c, E)
previousLetter (Just (c, E)) = Just (c, D)
previousLetter (Just (c, D)) = Just (c, C)
previousLetter (Just (c, C)) = Just (c, B)
previousLetter (Just (c, B)) = Just (c, A)
previousLetter _ = Nothing

nextNumber :: Maybe ChessBoardCoordinate -> Maybe ChessBoardCoordinate
nextNumber Nothing = Nothing
nextNumber (Just (c, l))
            | c == 8 = Nothing
            | otherwise = Just (c+1, l)

previousNumber :: Maybe ChessBoardCoordinate -> Maybe ChessBoardCoordinate
previousNumber Nothing = Nothing
previousNumber (Just (c, l))
            | c == 0 = Nothing
            | otherwise = Just (c-1, l)


chessBoard :: [ChessBoardCoordinate]
chessBoard = [(j,i) | i <- chessBoardLetters, j <- chessBoardNumber]

moveDiagonallyUpRight :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveDiagonallyUpRight coordenateBegin = nextNumber (nextLetter (Just coordenateBegin))

moveDiagonallyUpLeft :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveDiagonallyUpLeft coordenateBegin = nextNumber (previousLetter (Just coordenateBegin))

moveDiagonallyDownRight :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveDiagonallyDownRight coordenateBegin = previousNumber (nextLetter (Just coordenateBegin))

moveDiagonallyDownLeft :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveDiagonallyDownLeft coordenateBegin = previousNumber (previousLetter (Just coordenateBegin))

moveUp :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveUp coordenateBegin = nextNumber (Just coordenateBegin)

moveDown :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveDown coordenateBegin = previousNumber (Just coordenateBegin)

moveLeft :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveLeft coordenateBegin = previousLetter (Just coordenateBegin)

moveRight :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveRight coordenateBegin = nextLetter (Just coordenateBegin)

moveKnightVerticalUpRight :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveKnightVerticalUpRight coordenateBegin =  nextLetter (nextNumber (nextNumber (Just coordenateBegin)))

moveKnightVerticalUpLeft :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveKnightVerticalUpLeft coordenateBegin =  previousLetter (nextNumber (nextNumber (Just coordenateBegin)))

moveKnightVerticalDownRight :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveKnightVerticalDownRight coordenateBegin = nextLetter (previousNumber (previousNumber (Just coordenateBegin)))

moveKnightVerticalDownLeft :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveKnightVerticalDownLeft coordenateBegin =  previousLetter (nextNumber (nextNumber (Just coordenateBegin)))


moveKnightHorizontalUpRight :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveKnightHorizontalUpRight coordenateBegin =  nextLetter (nextLetter (nextNumber (Just coordenateBegin)))

moveKnightHorizontalUpLeft :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveKnightHorizontalUpLeft coordenateBegin =  previousLetter (previousLetter (nextNumber (Just coordenateBegin)))

moveKnightHorizontalDownRight :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveKnightHorizontalDownRight coordenateBegin = nextLetter (nextLetter (previousNumber (Just coordenateBegin)))

moveKnightHorizontalDownLeft :: ChessBoardCoordinate -> Maybe ChessBoardCoordinate
moveKnightHorizontalDownLeft coordenateBegin =  previousLetter (previousLetter (nextNumber (Just coordenateBegin)))

keepMoving :: (ChessBoardCoordinate -> Maybe ChessBoardCoordinate) -> ChessBoardCoordinate -> [ChessBoardCoordinate]
keepMoving f x = case f x of
                    Just a -> a:keepMoving f a
                    Nothing -> []

