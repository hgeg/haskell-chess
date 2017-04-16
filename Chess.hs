module Chess where

import qualified Data.Map as M

data Type = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Piece = White Type | Black Type deriving Eq

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

data Col = A | B | C | D | E | F | G | H deriving Eq
type Position = (Col, Int)
type Pos_q = (Int, Int)
type Board = M.Map Pos_q Piece

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
    foldr (\(i, e) s -> (show i) ++ "." ++ e ++ " " ++ s) 
          "" (zip [1..] $ reverse l)


main :: IO ()
main = do
 let game = Game {gBoard = initializeBoard, gTurn = "White", gLog = []}
 putStrLn $ show 
                 $ move (D,8) (D,6)
                 $ move (G,2) (H,3)
                 $ move (C,8) (H,3)
                 $ move (F,1) (C,4)
                 $ move (F,6) (D,5)
                 $ move (E,4) (D,5)
                 $ move (G,8) (F,6)
                 $ move (E,2) (E,4)
                 $ move (D,7) (D,5)
                 $ move (D,2) (D,4)
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

takePiece :: Board -> Pos_q -> Maybe Piece
takePiece b p = M.lookup p b

move :: Position -> Position -> Game -> Game
move (c0, r0) (c1, r1) g = move' (cnum c0, r0) (cnum c1, r1) g
  where cnum c = case c of A -> 1
                           B -> 2
                           C -> 3
                           D -> 4
                           E -> 5
                           F -> 6
                           G -> 7
                           H -> 8

move' :: Pos_q -> Pos_q -> Game -> Game
move' from to g = case (takePiece b from) of
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
    genLog p = ((show p ++ " " ++ ((['a'..'h']!!(c0-1)) : show r0) ++ ""++ show p ++ " " ++ ((['a'..'h']!!(c1-1)) : show r1) ++ ""):l)
    b = gBoard g
    t = gTurn g
    l = gLog g
    (c1, r1) = to
    (c0, r0) = from

validateMove :: Board -> Piece -> Pos_q -> Pos_q -> Bool
validateMove b (White Pawn)   from to = from/=to && isInsideBoard to && isPawnMove b from to
validateMove b (Black Pawn)   from to = from/=to && isInsideBoard to && isPawnMove b from to
validateMove b (White Knight) from to = from/=to && isInsideBoard to && isLShaped from to && isFree b from to 
validateMove b (Black Knight) from to = from/=to && isInsideBoard to && isLShaped from to && isFree b from to 
validateMove b (White Bishop) from to = from/=to && isInsideBoard to && isDiagonal from to 8 && isFree b from to
validateMove b (Black Bishop) from to = from/=to && isInsideBoard to && isDiagonal from to 8 && isFree b from to
validateMove b (White Rook)   from to = from/=to && isInsideBoard to && isStraight from to 8 && isFree b from to
validateMove b (Black Rook)   from to = from/=to && isInsideBoard to && isStraight from to 8 && isFree b from to
validateMove b (White Queen)  from to = from/=to && isInsideBoard to && (isStraight from to 8 || isDiagonal from to 8) && isFree b from to
validateMove b (Black Queen)  from to = from/=to && isInsideBoard to && (isStraight from to 8 || isDiagonal from to 8) && isFree b from to
validateMove b (White King)   from to = from/=to && isInsideBoard to && (isStraight from to 1 || isDiagonal from to 1) && isFree b from to
validateMove b (Black King)   from to = from/=to && isInsideBoard to && (isStraight from to 1 || isDiagonal from to 1) && isFree b from to


isInsideBoard :: Pos_q -> Bool
isInsideBoard (c,r)= r>=1 && r<= 8 && c>=1 && c<=8

isPawnMove :: Board -> Pos_q -> Pos_q -> Bool
isPawnMove b (c0,r0) (c1,r1) = case takePiece b (c0,r0) of
  Nothing -> False
  Just (White Pawn) -> (c0==c1 && r1==r0+1) ||       --regular
                       (c0==c1 && r0==2 && r1==4) || --2 rank
                       (abs (c0-c1)==1 && r1==r0+1 && cpt "white")  --capture
  Just (Black Pawn) -> (c0==c1 && r1==r0-1) ||       --regular
                       (c0==c1 && r0==7 && r1==5) || --2 rank
                       (abs (c0-c1)==1 && r1==r0-1 && cpt "black")  --capture
  where
    cpt color = case takePiece b (c1, r1) of
      Just (White _) -> color == "black"
      Just (Black _) -> color == "white"
      otherwise      -> False

isStraight :: Pos_q -> Pos_q -> Int -> Bool
isStraight (c0, r0) (c1,r1) span = (c0==c1 && foldr (\x y -> x==c1 || y) False [c0-span..c0+span]) || -- horizontal
                                   (r0==r1 && foldr (\x y -> x==r1 || y) False [r0-span..r0+span])    -- vertical

isDiagonal :: Pos_q -> Pos_q -> Int -> Bool
isDiagonal (c0, r0) (c1,r1) span = abs (c1-c0) == abs (r1-r0) && abs (c1-c0)<=span

isLShaped :: Pos_q -> Pos_q -> Bool
isLShaped (c0, r0) (c1,r1) = abs (c1-c0) + abs (r1-r0) == 3

isFree :: Board -> Pos_q -> Pos_q -> Bool
isFree b from to = case takePiece b from of
  Just (White Pawn  ) -> isMovable b to "white"
  Just (Black Pawn  ) -> isMovable b to "black"
  Just (White Knight) -> isMovable b to "white"
  Just (Black Knight) -> isMovable b to "black"
  Just (White Bishop) -> diagonalCheck "white" && isMovable b to "white"
  Just (Black Bishop) -> diagonalCheck "black" && isMovable b to "black"
  Just (White Rook  ) -> straightCheck "white" && isMovable b to "white"
  Just (Black Rook  ) -> straightCheck "black" && isMovable b to "black"
  Just (White Queen ) -> (diagonalCheck "white" || straightCheck "white") && isMovable b to "white"
  Just (Black Queen ) -> (diagonalCheck "black" || straightCheck "black") && isMovable b to "black"
  Just (White King  ) -> (diagonalCheck "white" || straightCheck "white") && isMovable b to "white"
  Just (Black King  ) -> (diagonalCheck "black" || straightCheck "black") && isMovable b to "black"
  where 
    (r0, c0) = from
    (r1, c1) = to
    difc = (c0 - c1) + 1
    dirc = div difc (abs difc) 
    difr = (r0 - r1) + 1
    dirr = div difr (abs difr) 
    isMovable b pos color = case takePiece b pos of
      Nothing        -> True
      Just (White _) -> color == "black"
      Just (Black _) -> color == "white"
    straightCheck color = if r0==r1 
                            then foldr (\i r -> r && isMovable b (c0+i,r0) color) True [0,dirc..difc] 
                            else foldr (\i r -> r && isMovable b (c0,r0+i) color) True [0,dirr..difc] 
    diagonalCheck color = foldr (\(ic, ir) r -> r && isMovable b (c0+ic, r0+ir) color) True (zip [0,dirc..difc] [0,dirr..difr])
