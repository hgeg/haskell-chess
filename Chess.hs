module Chess where

import qualified Data.Map as M
import Data.List
import Logging

data Type = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Color = Black | White deriving (Show, Eq)
data Piece = P Color Type deriving Eq

instance Show Piece where
  show (P White Pawn  ) = "♙"
  show (P Black Pawn  ) = "♟"
  show (P White Knight) = "♘"
  show (P Black Knight) = "♞"
  show (P White Bishop) = "♗"
  show (P Black Bishop) = "♝"
  show (P White Rook  ) = "♖"
  show (P Black Rook  ) = "♜"
  show (P White Queen ) = "♕"
  show (P Black Queen ) = "♛"
  show (P White King  ) = "♔"
  show (P Black King  ) = "♚"

data Col = A | B | C | D | E | F | G | H deriving Eq
type Position = (Col, Int)
type Pos_q = (Int, Int)
type Board = M.Map Pos_q Piece

data Game = Game { gBoard :: Board, gTurn :: Color , gLog :: [String]}

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

initializeBoard :: Board
initializeBoard = M.fromList [
  ((1,8), P Black Rook),
  ((2,8), P Black Knight),
  ((3,8), P Black Bishop),
  ((4,8), P Black Queen),
  ((5,8), P Black King),
  ((6,8), P Black Bishop),
  ((7,8), P Black Knight),
  ((8,8), P Black Rook),
  ((1,7), P Black Pawn),
  ((2,7), P Black Pawn),
  ((3,7), P Black Pawn),
  ((4,7), P Black Pawn),
  ((5,7), P Black Pawn),
  ((6,7), P Black Pawn),
  ((7,7), P Black Pawn),
  ((8,7), P Black Pawn),

  ((1,1), P White Rook),
  ((2,1), P White Knight),
  ((3,1), P White Bishop),
  ((4,1), P White Queen),
  ((5,1), P White King),
  ((6,1), P White Bishop),
  ((7,1), P White Knight),
  ((8,1), P White Rook),
  ((1,2), P White Pawn),
  ((2,2), P White Pawn),
  ((3,2), P White Pawn),
  ((4,2), P White Pawn),
  ((5,2), P White Pawn),
  ((6,2), P White Pawn),
  ((7,2), P White Pawn),
  ((8,2), P White Pawn)
  ]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x ys -> if p x then x:ys else [x]) []

takePiece :: Board -> Pos_q -> Maybe Piece
takePiece b p = M.lookup p b

findPiece :: Board -> Piece -> Maybe Pos_q
findPiece b piece = fmap fst $ find ((==piece) . snd) $ M.toList b

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
  Just piece  ->
    let (P color _) = piece in
      case mconcat [ doesTurnMatch t color, validateMove b piece from to, isKingSafe b color from to] of
        Ok -> Game { gBoard = makeMove b piece from to, 
                    gTurn = if color == Black then White else Black, 
                    gLog = genLog piece}
        Fail msg -> Game { gBoard = b, gTurn = t, gLog = (msg:l)}
  where 
    genLog p = ((show p ++ " " ++ ((['a'..'h']!!(c0-1)) : show r0) ++ ""++ show p ++ " " ++ ((['a'..'h']!!(c1-1)) : show r1) ++ ""):l)
    b = gBoard g
    t = gTurn g
    l = gLog g
    (c1, r1) = to
    (c0, r0) = from

makeMove :: Board -> Piece -> Pos_q -> Pos_q -> Board
makeMove b piece from to = if isRook' b from to
  then makeRookMove b piece from to
  else if isEnPasse' b from to
    then makeEnPasseMove b piece from to
    else M.insert to piece $ M.delete from b

makeRookMove :: Board -> Piece -> Pos_q -> Pos_q -> Board
makeRookMove b (P c King) from to = undefined
makeRookMove b _ _ _ = b

makeEnPasseMove :: Board -> Piece -> Pos_q -> Pos_q -> Board
makeEnPasseMove b pawn from to =  M.insert to pawn $ M.delete from $ M.delete (c1,r0) b
  where (c0, r0) = from
        (c1, r1) = to

doesTurnMatch :: Color -> Color -> Log
doesTurnMatch t c = mkLog (t==c) $ "not " ++ (show c) ++ "'s turn"

validateMove :: Board -> Piece -> Pos_q -> Pos_q -> Log
validateMove b (P _ Pawn)   from to = mconcat [diffCheck from to, isInsideBoard to, isPawnMove b from to]
validateMove b (P _ Knight) from to = mconcat [diffCheck from to, isInsideBoard to, isLShaped from to, isFree b from to]
validateMove b (P _ Bishop) from to = mconcat [diffCheck from to, isInsideBoard to, isDiagonal from to 8, isFree b from to]
validateMove b (P _ Rook)   from to = mconcat [diffCheck from to, isInsideBoard to, isStraight from to 8, isFree b from to]
validateMove b (P _ Queen)  from to = mconcat [diffCheck from to, isInsideBoard to, (isStraight from to 8 <|> isDiagonal from to 8), isFree b from to]
validateMove b (P _ King)   from to = mconcat [diffCheck from to, isInsideBoard to, (isStraight from to 1 <|> isDiagonal from to 1 <|> isRook b from to), isFree b from to]

diffCheck :: Pos_q -> Pos_q -> Log
diffCheck from to = mkLog (from/=to) "non-move"

isInsideBoard :: Pos_q -> Log
isInsideBoard (c,r)= mkLog (r>=1 && r<= 8 && c>=1 && c<=8) "out of the board"

isPawnMove :: Board -> Pos_q -> Pos_q -> Log
isPawnMove b (c0, r0) (c1, r1) = case takePiece b (c0,r0) of
  Nothing -> Fail "empty cell"
  Just (P c Pawn) -> mkLog (
    (c0==c1 && r1==r0-1 && c==Black) ||                  --regular (black)
    (c0==c1 && r1==r0+1 && c==White) ||                  --regular (white)
    (c0==c1 && r0==7 && r1==5 && c==Black) ||            --2 rank (black)
    (c0==c1 && r0==2 && r1==4 && c==White) ||            --2 rank (white)
    (abs (c0-c1)==1 && r1==r0-1 && cpt c && c==Black) || --capture (black)
    (abs (c0-c1)==1 && r1==r0+1 && cpt c && c==White) || --capture (white)
    (isEnPasse' b (c0,r0) (c1, r1))
    ) "not a pawn move"
  where
    cpt color = case takePiece b (c1, r1) of
      Just (P c _) -> color /= c
      otherwise    -> False

isStraight :: Pos_q -> Pos_q -> Int -> Log
isStraight (c0, r0) (c1,r1) span = mkLog (
  (c0==c1 && foldr (\x y -> x==c1 || y) False [c0-span..c0+span]) || -- horizontal
  (r0==r1 && foldr (\x y -> x==r1 || y) False [r0-span..r0+span])    -- vertical
  ) "this piece can only move on a straight line"

isDiagonal :: Pos_q -> Pos_q -> Int -> Log
isDiagonal (c0, r0) (c1,r1) span = mkLog (
  abs (c1-c0) == abs (r1-r0) && abs (c1-c0)<=span
  ) "this piece can only move on a diagonal line"

isLShaped :: Pos_q -> Pos_q -> Log
isLShaped (c0, r0) (c1,r1) = mkLog (
  abs (c1-c0) + abs (r1-r0) == 3
  ) "this piece can only move in L shape"

--special moves
isEnPasse :: Board -> Pos_q -> Pos_q -> Log
isEnPasse b from to = mkLog (isEnPasse' b from to) "invalid en passe"

isEnPasse' :: Board -> Pos_q -> Pos_q -> Bool
isEnPasse' b (c0, r0) (c1,r1) = case takePiece b (c0,r0) of
  Nothing -> False
  Just (P White Pawn) -> (r0==5 || r0==6) && (r1==r0+1) && (takePiece b (c1,r0))==(Just (P Black Pawn)) 
  Just (P Black Pawn) -> (r0==3 || r0==4) && (r1==r0-1) && (takePiece b (c1,r0))==(Just (P White Pawn)) 
  otherwise -> False

isRook :: Board -> Pos_q -> Pos_q -> Log
isRook b from to = mkLog (
  isRook' b from to
  ) "invalid rook"

isRook' :: Board -> Pos_q -> Pos_q -> Bool
isRook' b from to = case takePiece b from of 
  Just (P c King) -> nextToRook b from to
  otherwise -> False
  where nextToRook b f t = False

isFree :: Board -> Pos_q -> Pos_q -> Log
isFree b from to = mkLog (
  isFree' b from to
  ) "move is obstructed"

isFree' :: Board -> Pos_q -> Pos_q -> Bool
isFree' b from to = case takePiece b from of
  Just (P c Pawn  ) -> canMove b to c
  Just (P c Knight) -> canMove b to c
  Just (P c Bishop) -> diagonalCheck c && canMove b to c
  Just (P c Rook  ) -> straightCheck c && canMove b to c
  Just (P c Queen ) -> (diagonalCheck c || straightCheck c) && canMove b to c
  Just (P c King  ) -> (diagonalCheck c || straightCheck c) && canMove b to c
  where 
    (r0, c0) = from
    (r1, c1) = to
    difc = (c0 - c1)
    dirc = if difc<0 then -1 else 1
    difr = (r0 - r1)
    dirr = if difr<0 then -1 else 1
    canMove b pos color = case takePiece b pos of
      Nothing   -> True
      Just (P c t) -> c /= color
    straightCheck color = if r0==r1 
                            then foldr (\i r -> r && canMove b (c0+i,r0) color) True [0,dirc..difc] 
                            else foldr (\i r -> r && canMove b (c0,r0+i) color) True [0,dirr..difc] 
    diagonalCheck color = foldr (\(ic, ir) r -> r && canMove b (c0+ic, r0+ir) color) True (zip [0,dirc..difc] [0,dirr..difr])

isKingSafe :: Board -> Color -> Pos_q -> Pos_q -> Log
isKingSafe b turn from to = mkLog (
  isKingSafe' b turn from to
  ) $ (show turn) ++ " king is not safe"

isKingSafe' :: Board -> Color -> Pos_q -> Pos_q -> Bool
isKingSafe' b turn from to = case (takePiece b from) of
  Nothing -> False -- non-existent case: validateMove ensures this.
  Just movingPiece -> iterDiagonal Queen myKing && 
                      iterStraight Queen myKing && 
                      iterLShaped        myKing
    where nb = makeMove b movingPiece from to --possible next state
          myKing = findPiece nb (P turn King)
          opponent = if turn == White then Black else White
          iterDiagonal t (Just (c, r)) = True
          iterStraight t (Just (c, r)) = and $ map ((/=(Just (P opponent t))).(takePiece nb)) $ 
            map (\cx -> (cx,r)) (
              (takeWhile' (\cx -> canMove nb (cx,r)) [(c+1)..8]) ++ 
              (takeWhile' (\cx -> canMove nb (cx,r)) [(c-1),(c-2)..0]) ) ++
            map (\rx -> (c,rx)) (
              (takeWhile' (\rx -> canMove nb (c,rx)) [(r+1)..8]) ++ 
              (takeWhile' (\rx -> canMove nb (c,rx)) [(r-1),(r-2)..0]) )
          iterLShaped  (Just (c, r)) = and $ map ((/=(Just (P opponent Knight))).(takePiece nb)) [(c-2,r-1), (c-2,r+1), (c+2,r-1), (c-2, r+1), (c-1,r-2), (c-1,r+2), (c+1,r-2), (c+1,r+2)]
          canMove b pos = case takePiece b pos of
            Nothing   -> True
            otherwise -> False
