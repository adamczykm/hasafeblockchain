 module Log where


class Monad m => MonadLog m where
  logStr :: String -> m ()


logPrint :: (MonadLog m, Show a) => a -> m ()
logPrint v = logStr (show v)
