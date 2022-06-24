module Utils where

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ cond action = go
  where
    go = do
      c <- cond
      if c then action >> go else return ()

untilM_ :: Monad m => m Bool -> m a -> m ()
untilM_ cond = whileM_ (not <$> cond)

