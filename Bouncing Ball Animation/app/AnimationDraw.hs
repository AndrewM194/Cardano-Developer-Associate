module AnimationDraw where

import AnimationState
import Control.Concurrent (threadDelay)
import System.Process (system)

drawNext :: Int -> Int -> Env -> Status -> IO ()
drawNext r c (Env (width, height)) (Status (posX, posY) (dirX, dirY)) = do {
  if (r == posX) && (c == posY) then putStr("*") else putStr(" ");
  putStr("|");
  if (c == width) then putStr("\n") else (return ());
  (nr, nc) <- return (if (c == width) then (r + 1, 1) else (r, c + 1));
  if (nr == (height + 1)) then (return ()) else drawNext nr nc (Env (width, height)) (Status (posX, posY) (dirX, dirY))
}

draw :: Env -> Status -> IO ()
draw env status = do {
  system "cls";
  drawNext 1 1 env status;
  threadDelay 1000000
}
