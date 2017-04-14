module Chess where

import qualified Data.Map as M

data Type = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Piece = White Type | Black Type 

instance Show Piece where
  show (White Pawn  ) = "♙"
  show (Black Pawn  ) = "♟"
  show (White Knight) = "♘"
  show (Black Knight) = "♞"
  show (White Bishop) = "♗"
  show (Black Bishop) = "♝"
  show (White Rook  ) = "♖"
  show (Black Rook  ) = "♜"
  show (White Queen ) = "♕"
  show (Black Queen ) = "♛"
  show (White King  ) = "♔"
  show (Black King  ) = "♚"

type Position = (Int, Int)
type Board = M.Map Position Piece

data Game = Game { gBoard :: Board, gTurn :: String , gLog :: [String]}

instance Show Game where
  show (Game {gBoard = b, gTurn = t, gLog = l}) =
    foldr
      (\i s ->
        let rank = 8-(div (i-1) 8)
            index = ((mod (i-1) 8) + 1, rank)
            hasPiece = M.member index b
        in
          if even (i + (div (i-1) 8)) then
            let square = if hasPiece then (show $ b M.! index) ++ " " else "▓▓" 
            in
              if mod i 8 == 0 then
                square ++ (show rank ++ "\n") ++ s
              else
                square ++ s
          else 
            let square = if hasPiece then (show $ b M.! index) ++ " " else "  " 
            in
              if mod i 8 == 0 then
                square ++ (show rank ++ "\n") ++ s
              else
                square ++ s
      ) "" [1..64] ++
    foldr (\i s -> (i:" ") ++ s) 
          "" ['a'..'h'] ++ "\n" ++ "\n" ++
    foldr (\(i, e) s -> (show i) ++ ". " ++ e ++ "\n" ++ s) 
          "" (zip [1..] $ reverse l)


main :: IO ()
main = do
 let game = Game {gBoard = initializeBoard, gTurn = "White", gLog = []}
 putStrLn $ show 
                 $ move (2,8) (3,6) 
                 $ move (3,1) (7,5) 
                 $ move (4,7) (4,6) 
                 $ move (4,2) (4,3) 
                 game 

initializeBoard :: Board
initializeBoard = M.fromList [
  ((1,8), Black Rook),
  ((2,8), Black Knight),
  ((3,8), Black Bishop),
  ((4,8), Black Queen),
  ((5,8), Black King),
  ((6,8), Black Bishop),
  ((7,8), Black Knight),
  ((8,8), Black Rook),
  ((1,7), Black Pawn),
  ((2,7), Black Pawn),
  ((3,7), Black Pawn),
  ((4,7), Black Pawn),
  ((5,7), Black Pawn),
  ((6,7), Black Pawn),
  ((7,7), Black Pawn),
  ((8,7), Black Pawn),

  ((1,1), White Rook),
  ((2,1), White Knight),
  ((3,1), White Bishop),
  ((4,1), White Queen),
  ((5,1), White King),
  ((6,1), White Bishop),
  ((7,1), White Knight),
  ((8,1), White Rook),
  ((1,2), White Pawn),
  ((2,2), White Pawn),
  ((3,2), White Pawn),
  ((4,2), White Pawn),
  ((5,2), White Pawn),
  ((6,2), White Pawn),
  ((7,2), White Pawn),
  ((8,2), White Pawn)
  ]

takePiece :: Board -> Position -> Maybe Piece
takePiece b p = M.lookup p b

move :: Position -> Position -> Game -> Game
move from to g = case (takePiece b from) of
  Nothing -> Game { gBoard = b, gTurn = t, gLog = (("invalid location "++show from):l)}
  Just (White typ)  ->
    if t=="White" && validateMove b (White typ) from to
      then Game { gBoard = M.insert to (White typ) (M.delete from b), 
                  gTurn = "Black", 
                  gLog = genLog (White typ)}
      else Game { gBoard = b, gTurn = t, gLog = ("not white's turn":l)}
  Just (Black typ)  ->
    if t=="Black" && validateMove b (Black typ) from to
      then Game { gBoard = M.insert to (Black typ) (M.delete from b), 
                  gTurn = "White", 
                  gLog = genLog (Black typ)}
      else Game { gBoard = b, gTurn = t, gLog = ("not black's turn":l)}
  where 
    genLog p = ((show p ++ " " ++ ((['a'..'h']!!(c0-1)) : show r0) ++ " "++ show p ++ " " ++ ((['a'..'h']!!(c1-1)) : show r1) ++ ""):l)
    b = gBoard g
    t = gTurn g
    l = gLog g
    (c1, r1) = to
    (c0, r0) = from

validateMove :: Board -> Piece -> Position -> Position -> Bool
validateMove b (White Pawn)   from to = from/=to && isInsideBoard to && isPawnMove b from to "white"
validateMove b (Black Pawn)   from to = from/=to && isInsideBoard to && isPawnMove b from to "black"
validateMove b (White Knight) from to = from/=to && isInsideBoard to && isLShaped from to
validateMove b (Black Knight) from to = from/=to && isInsideBoard to && isLShaped from to
validateMove b (White Bishop) from to = from/=to && isInsideBoard to && isDiagonal from to 8 && isFree b from to
validateMove b (Black Bishop) from to = from/=to && isInsideBoard to && isDiagonal from to 8 && isFree b from to
validateMove b (White Rook)   from to = from/=to && isInsideBoard to && isStraight from to 8 && isFree b from to
validateMove b (Black Rook)   from to = from/=to && isInsideBoard to && isStraight from to 8 && isFree b from to
validateMove b (White Queen)  from to = from/=to && isInsideBoard to && isStraight from to 8 || isDiagonal from to 8 && isFree b from to
validateMove b (Black Queen)  from to = from/=to && isInsideBoard to && isStraight from to 8 || isDiagonal from to 8 && isFree b from to
validateMove b (White King)   from to = from/=to && isInsideBoard to && isStraight from to 1 || isDiagonal from to 1
validateMove b (Black King)   from to = from/=to && isInsideBoard to && isStraight from to 1 || isDiagonal from to 1


isInsideBoard :: Position -> Bool
isInsideBoard (c,r)= r>=1 && r<= 8 && c>=1 && c<=8

isPawnMove :: Board -> Position -> Position -> String -> Bool
isPawnMove b (c0,r0) (c1,r1) color = True

isStraight :: Position -> Position -> Int -> Bool
isStraight (c0, r0) (c1,r1) span = (c0==c1 && Prelude.foldr (\x y -> x==c1 || y) False [c0-span..c0+span]) || -- horizontal
                                   (r0==r1 && Prelude.foldr (\x y -> x==r1 || y) False [r0-span..r0+span])    -- vertical

isDiagonal :: Position -> Position -> Int -> Bool
isDiagonal (c0, r0) (c1,r1) span = abs (c1-c0) == abs (r1-r0) && abs (c1-c0)<=span

isLShaped :: Position -> Position -> Bool
isLShaped (c0, r0) (c1,r1) = abs (c1-c0) + abs (r1-r0) == 3

isFree :: Board -> Position -> Position -> Bool
isFree b from to = True
