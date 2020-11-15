import Control.Monad (when)

printDash :: Int -> IO ()
printDash num = putStr (replicate num '─')

calculateLineWidth :: Num a => a -> a
calculateLineWidth n = (n -1) * 4 + 3

printLine :: Int -> Int -> Int -> IO ()
printLine currentLine totalNum lineWidth
  | currentLine == 1 = printFirstLine lineWidth
  | currentLine == totalNum = printLastLine lineWidth
  | otherwise =
    if currentLine <= (totalNum `div` 2)
      then printMidLine 1 lineWidth currentLine totalNum printTopLine topPredicate
      else printMidLine 1 lineWidth currentLine totalNum printBottomLine bottomPredicate

printFirstLine :: Int -> IO ()
printFirstLine lineWidth = do
  printTopLine (lineWidth -2)
  putStrLn ""

printLastLine :: Int -> IO ()
printLastLine lineWidth = do
  printBottomLine (lineWidth -2)
  putStrLn ""

printTopLine :: Int -> IO ()
printTopLine width = do
  putStr "┌"
  printDash width
  putStr "┐"

printBottomLine :: Int -> IO ()
printBottomLine width = do
  putStr "└"
  printDash width
  putStr "┘"

topPredicate :: (Eq a, Num a) => a -> a -> p -> Bool
topPredicate position lineNum _ = position == (2 * lineNum -1)

bottomPredicate :: (Eq a, Num a) => a -> a -> a -> Bool
bottomPredicate position lineNum totalNum = position == (2 * (totalNum - lineNum) + 1)

printMidLine :: Integral a1 => a1 -> a1 -> t1 -> t2 -> (a1 -> IO a2) -> (a1 -> t1 -> t2 -> Bool) -> IO ()
printMidLine position lineWidth lineNum totalNum func pred
  | position == lineWidth = putStrLn "│"
  | pred position lineNum totalNum = do
    func (lineWidth - position * 2)
    printMidLine (lineWidth - position + 2) lineWidth lineNum totalNum func pred
  | otherwise = do
    if position `mod` 2 == 1 then putStr "│" else putStr " "
    printMidLine (position + 1) lineWidth lineNum totalNum func pred

helper :: Int -> Int -> IO ()
helper n currentLine =
  when (currentLine <= rowsNum) $ do
    printLine currentLine rowsNum lineWidth
    helper n (currentLine + 1)
  where
    rowsNum = n * 2
    lineWidth = calculateLineWidth n

squares n = helper n 1