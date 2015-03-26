-- | Main entry point to the application.
module Main where

-- | The main entry point.
main :: IO ()
main = do
    nim


nim :: IO ()

nim = do
  putStrLn "Gra nim"
  rows <- [5,4,3,2,1]
  gameRound rows

gameRound :: [Int] -> IO ()

gameRound rows = do
  putStrLn ""
  rows1 <- playerMove rows
  if empty rows1 then putStrLn "Wygrales!"
  else do
      rows2 <- cpuMove rows
      if empty rows2 then putStrLn "Komputer wygrywa!"
      else gameRound rows

printState :: [Int] -> IO ()
printState (x:xs) = do
    print x
    printState xs
printState [] = return

empty :: [Int] -> Bool
empty list = foldr list (+) 0 == 0
