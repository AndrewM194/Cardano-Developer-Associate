module Main where

import Animation
import AnimationState
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

main :: IO ()
main = cli

cli :: IO ()
cli = do {
    env    <- return $ Env (20, 20);
    status <- return $ Status (2, 3) (-1, 1);
    (_,log) <- runWriterT (runStateT (runReaderT (animate 100) env) status);
    writeFile "log.txt" log;
    return ();
}
