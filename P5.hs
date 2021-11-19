module P5 where
import P2
import P4
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

askRoots :: (Double, Double, Double) -> IO ()
askRoots poly = do
  let correct = roots poly
  putStrLn $ "What are the roots of: " <> pprint poly <> "? Write '-' for none."
  putStr ">"
  hFlush stdout
  ans <- getLine
  case readRoots ans of
    Nothing -> putStrLn $ "could not parse: \"" <> ans <> "\""
    Just root -> 
      if root == correct 
        then putStrLn "Well done!" 
        else putStrLn $ "Bummer, the roots are" <> show correct
  return ()

pprint :: (Double, Double, Double) -> String
pprint (a,b,c) = show a <> "x^2 + " <> show b <> "x + " <> show c

readRoots :: String -> Maybe Root
readRoots "-" = Just None
readRoots s = do
  xs <- mapM readMaybe (words s)
  case xs of
    [r1] -> return (One r1)
    [r1, r2] -> return (Two r1 r2)
    _ -> Nothing
