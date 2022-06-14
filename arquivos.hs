import System.IO

{-
leitura :: IO()
leitura = do
  linha <- getLine
  if linha = "fim" then return() else do
    putStrLn linha
    leitura
-}

countWords :: String -> [Int]
countWords input = map (length.words) (lines input)

showCPF :: String -> [String]
showCPF = words

leitura :: IO ()
leitura = do
   {-
   myFile <- openFile "cpfs.txt" ReadMode
   hClose myFile
   putStrLn "done!"
   -}
   content <- readFile "cpfs.txt"
   --mapM_ putStrLn $ showCPF content
   mapM_ putStrLn $ showCPF content