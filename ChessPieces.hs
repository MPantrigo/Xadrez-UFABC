module ChessPieces where
import ChessBoard
import Data.Maybe

data ChessPiece = King | Queen | Bishop | Knight | Rook | Pawn
        deriving (Eq)
data PieceColour = Black | White

-- IsValidMove :: ChessPiece -> ChessBoardCoordinate -> ChessBoardCoordinate -> Bool
-- IsValidMove piece, coordinateBegin, coordinateEnd = 


getAllPossibleMoves :: ChessPiece -> ChessBoardCoordinate -> [ChessBoardCoordinate]
getAllPossibleMoves piece 
                | piece == King = getKingMoves  
                | piece == Queen = getQueenMoves
                | piece == Bishop = getBishopMoves
                | piece == Knight = getKnightMoves
                | piece == Rook = getRookMoves
                | piece == Pawn = getPawnMoves 


getKingMoves :: ChessBoardCoordinate -> [ChessBoardCoordinate]
getKingMoves coordinate = catMaybes basicMoves
                        where basicMoves = [moveUp coordinate, moveDown coordinate, moveLeft coordinate, moveRight coordinate, moveDiagonallyDownLeft coordinate,
                                                moveDiagonallyDownRight coordinate, moveDiagonallyUpLeft coordinate, moveDiagonallyUpRight coordinate]


getQueenMoves :: ChessBoardCoordinate -> [ChessBoardCoordinate]
getQueenMoves coordinate = keepMoving moveUp coordinate ++ 
                           keepMoving moveDown coordinate ++
                           keepMoving moveLeft coordinate ++ 
                           keepMoving moveRight coordinate ++ 
                           keepMoving moveDiagonallyDownLeft coordinate ++
                           keepMoving moveDiagonallyDownRight coordinate ++ 
                           keepMoving moveDiagonallyUpLeft coordinate ++ 
                           keepMoving moveDiagonallyUpRight coordinate


getRookMoves :: ChessBoardCoordinate -> [ChessBoardCoordinate]
getRookMoves coordinate = keepMoving moveUp coordinate ++ 
                           keepMoving moveDown coordinate ++
                           keepMoving moveLeft coordinate ++ 
                           keepMoving moveRight coordinate



getBishopMoves :: ChessBoardCoordinate -> [ChessBoardCoordinate]
getBishopMoves coordinate = keepMoving moveDiagonallyDownLeft coordinate ++
                           keepMoving moveDiagonallyDownRight coordinate ++ 
                           keepMoving moveDiagonallyUpLeft coordinate ++ 
                           keepMoving moveDiagonallyUpRight coordinate

getKnightMoves :: ChessBoardCoordinate -> [ChessBoardCoordinate]
getKnightMoves coordinate = catMaybes basicMoves
                        where basicMoves = 
                                [moveKnightHorizontalDownLeft coordinate,
                                 moveKnightHorizontalDownRight coordinate,
                                 moveKnightHorizontalUpLeft coordinate,
                                 moveKnightHorizontalUpRight coordinate,
                                 moveKnightVerticalDownLeft coordinate,
                                 moveKnightVerticalDownRight coordinate,
                                 moveKnightVerticalUpLeft coordinate,
                                 moveKnightVerticalDownLeft coordinate]

getPawnMoves :: ChessBoardCoordinate -> [ChessBoardCoordinate]
getPawnMoves coordinate = catMaybes basicMoves
                        where basicMoves = 
                                [moveUp coordinate]


