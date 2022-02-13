module Animation where

import AnimationState
import AnimationDraw
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State.Lazy

animate :: Int -> ReaderT Env (StateT Status (WriterT String IO)) ()
animate elapsed = if elapsed == 0 then do { return () } else do {
    env                              <- ask;
    status                           <- get;
    Env (width, height)              <- return env;
    Status (posX, posY) (dirX, dirY) <- return status;
    (x1,dx1)                         <- return $ (posX + dirX, dirX);
    (y1,dy1)                         <- return $ (posY + dirY, dirY);
    (x2,dx2)                         <- return $ if (x1 < 1) then (1 - x1,-dx1) else (x1,dx1);
    (y2,dy2)                         <- return $ if (y1 < 1) then (1 - y1,-dy1) else (y1,dy1);
    (x3,dx3)                         <- return $ if (x2 > width)  then ((2 * width + 1)  - x2,-dx2) else (x2,dx2);
    (y3,dy3)                         <- return $ if (y2 > height) then ((2 * height + 1) - y2,-dy2) else (y2,dy2);
    put (Status (x3, y3) (dx3, dy3));
    liftIO $ draw env status;
    tell (show (posX,posY));
    tell ("-->");
    tell (show (x3,y3));
    tell "\n";
    animate (elapsed - 1);
    return ()
}