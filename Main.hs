import Chess

main :: IO ()
main = do
 let game = Game {gBoard = initializeBoard, gTurn = White, gLog = []}
 putStrLn $ show 
                 $ move (F,8) (B,4)
                 $ move (A,5) (B,6)
                 $ move (E,7) (E,6)
                 $ move (A,4) (A,5)
                 $ move (B,8) (C,6)
                 $ move (A,3) (A,4)
                 $ move (B,7) (B,5)
                 $ move (D,1) (E,2)
                 $ move (E,6) (E,5)
                 $ move (F,3) (E,5)
                 $ move (D,6) (E,6)
                 $ move (A,2) (A,3)
                 $ move (D,5) (C,3)
                 $ move (G,1) (F,3)
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

