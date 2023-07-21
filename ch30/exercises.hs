readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n*2)

allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f x = x >>= (\n -> return (f n))

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp f x = f >>= (\n -> x >>= (\m -> return (n m)))
