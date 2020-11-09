-- Nichole Maldonado
-- Lab 3 - Main.hs
-- November 1, 2020
-- Dr. Cheon, CS3360

-- Board representation that performs all the logic such as creating the board,
-- representing the board as a string, and determining if a move was a
-- winning move.
module Board where
--  (isWonBy
--  , isSlotOpen
--  , dropInSlot
--  , isFull
--  , numSlot
--  , mkPlayer
--  , mkOpponent
--  , mkBoard
--  , boardToStr
--  ) where

  -- Player One representation
  mkPlayer :: Int
  mkPlayer = 1

  -- Player Two Representation
  mkOpponent :: Int
  mkOpponent = 2

  -- Empty slot representation
  mkEmpty :: Int
  mkEmpty = 0

  -- Creates a 2D board with m columns and n rows
  mkBoard :: Int -> Int -> [[Int]]
  mkBoard m n = [[mkEmpty | x <- [1..n]] | x<-[1..m]]

  -- Drops player p's disc in a slot (column) i of a board bd. Assume that the
  -- slot has an empty place to hold the dropped disc.
  dropInSlot :: [[Int]] -> Int -> Int -> [[Int]]
  dropInSlot [] _ _ = []
  dropInSlot (x:xs) col player
    | col == 0 = (fillSlot x player) : xs
    | otherwise = x : dropInSlot xs (col - 1) player
  
  -- Helper function that inserts the player in the column.
  fillSlot :: [Int] -> Int -> [Int]
  fillSlot [] _ = []
  fillSlot (x:[]) player = player : []
  fillSlot (x:xs:xss) player
    | xs /= mkEmpty = player : otherHalf
    | otherwise = x : fillSlot otherHalf player
    where otherHalf = xs : xss

  -- Finds first non empty col
--  findCol :: [[Int]] -> Int -> Int
--  findCol board col
--    | board == [] || h!!col /= mkEmpty = -1
--    | otherwise = 1 + (findCol t col)
--      where (h:t) = board

  -- Returns True if the board at column i has an empty slot (denoted) by a 0
  -- otherwise, returns true.
  isSlotOpen :: [[Int]] -> Int -> Bool
  isSlotOpen bd i
    | bd == [] || bd == [[]] || i < 0 || i >= (numSlot bd) = False
    | otherwise = head (bd!!i) == mkEmpty

  -- Returns the number of columns of a board bd
  numSlot :: [[Int]] -> Int
  numSlot bd = length bd

  -- Returns true if the board is full (no 0 pieces), otherwise false.
  isFull :: [[Int]] -> Bool
  isFull [] = True
  isFull (h:t)
    | head h == mkEmpty = False
    | otherwise = isFull t

  -- Determines is the player p won the game by looking at the board bd.
  isWonBy :: [[Int]] -> Int -> Bool
  isWonBy bd p
    | findVertical bd p || findHorizontal bd numRows 0 p = True
    | otherwise = searchDiagonal bd numRows p
   where numRows = length (head bd)

  -- Finds Vertical Move
  findVertical :: [[Int]] -> Int -> Bool
  findVertical [] _ = False
  findVertical (x:xs) p = searchCol x 0 || findVertical xs p
    where
        searchCol _ 4 = True
        searchCol [] _ = False
        searchCol (x:xs) count
            | x == p = searchCol xs (count + 1)
            | count > 0 = False -- Found matching, but we just found a piece that doees not match
            | otherwise = searchCol xs count
  
  -- Finds Horizontal Move
  -- width should be length (head bd) and currCol should be 0
  findHorizontal :: [[Int]] -> Int -> Int -> Int -> Bool 
  findHorizontal bd width currCol p
    | currCol >= width = False
    | bd == [] || bd == [[]] = False
    | otherwise = (searchRow bd 0) || findHorizontal bd width (currCol + 1) p
    where
        searchRow _ 4 = True
        searchRow [] _ = False
        searchRow (x:xs) count
            | x!!currCol == p = searchRow xs (count + 1)
            | otherwise = searchRow xs 0 -- Reset count to 0 if not matching.   
  
  callLowerDiag bd p w h t = lowerDiag bd colRowCheck p
    where 
        colRowCheck = if t == 'R' then \x y -> x >= w || y >= h else \x y -> x < 0 || y >= h 
        
  callUpperDiag bd p w h t = upperDiag bd colRowCheck nextPts p 0
    where
        colRowCheck = if t == 'R' then \x y -> x < 0 || y < 0 else \x y -> x >= w || y < 0 || y >= h
        nextPts = if t == 'R' then \x y -> (x - 1, y - 1) else \x y -> (x + 1, y - 1)

  -- Determines if a Diagonal exists
  searchDiagonal :: [[Int]] -> Int -> Int -> Bool
  searchDiagonal bd width p
    | findDiag bd 0 (height, width) (lowerDiagCall 'R') (upperDiagCall 'R') rNextPts = True
    | otherwise = findDiag bd 0 (height, width) (lowerDiagCall 'L') (upperDiagCall 'L') lNextPts
    where
        lowerDiagCall t = callLowerDiag bd p width height t (if t == 'R' then rNextPts else lNextPts)
        upperDiagCall t = callUpperDiag bd p width height t
        rNextPts x y = (x + 1, y + 1)
        lNextPts x y = (x - 1, y + 1)
        height = numSlot bd
      
  findCol [] = 0
  findCol (x:xs)
    | x == mkEmpty = 1 + findCol xs
    | otherwise = 0

  -- Determines if 4 pieces diagonally can be found. Uses a strategy for
  -- the lowerDiagCall and upperDiagCall which only need the current
  -- row, column and count. This function is used to find both the
  -- the left and right diag.
  findDiag :: [[Int]] -> Int -> (Int, Int) -> ((Int, Int) -> Int -> Bool) ->
    ((Int, Int) -> Int) -> (Int -> Int -> (Int, Int)) -> Bool
  findDiag bd row (height, width) lowerDiagCall upperDiagCall nextPts
    | row >= height = False
    | col >= width = callAgain
    | count >= 4 = True
    | count == 0 = callAgain
    | otherwise = lowerDiagCall (nextPts col row) (4 - count) || callAgain
      where
        callAgain = findDiag bd (row + 1) (height, width) lowerDiagCall upperDiagCall nextPts
        count = upperDiagCall (col, row)
        col = findCol (bd!!row)

  -- Checks the upper diagonal to see if a valid win exists. The colRowCheck
  -- checks to see if the current (col, row) are in the bounds of the board (bd).
  -- The nextPoints provides a function on how to navigate to the next point
  -- The count is the number of same color p pieces that have been found.
  -- Returns the same number of p pieces (<= 4)
  upperDiag :: [[Int]] -> (Int -> Int -> Bool) -> (Int -> Int -> (Int, Int)) ->
    Int -> Int -> (Int, Int) -> Int
  upperDiag bd colRowCheck nextPoints p count (col, row)
    | colRowCheck col row || count >= 4 = count
    | bd!!row!!col == p = newUpperDiag
    | otherwise = count
      where newUpperDiag = upperDiag bd colRowCheck nextPoints p (count + 1) (nextPoints col row)

  -- Checks the lower diagonal to see if a valid win exists. The colRowCheck
  -- checks to see if the current (col, row) are in the bounds of the board (bd).
  -- The nextPoints provides a function on how to navigate to the next point
  -- The expectedDount is the number of same color p pieces that need to be
  -- found. Returns true if the expectedCount of pieces were found.
  lowerDiag :: [[Int]] -> (Int -> Int -> Bool) -> Int -> (Int -> Int ->
    (Int, Int)) -> (Int, Int) -> Int -> Bool
  lowerDiag bd colRowCheck p nextPoints (col, row) expectedCount
    | expectedCount <= 0 = True
    | colRowCheck col row = False
    | bd!!row!!col == p = newLowerDiag (expectedCount - 1)
    | otherwise = False
      where newLowerDiag = lowerDiag bd colRowCheck p nextPoints (nextPoints col row)

  -- Represents the board as a string with the correct char representations of
  -- the pieces.
  boardToStr :: (Int -> Char) -> [[Int]] -> [Char]
  boardToStr playerToChar bd = goThroughRows 0 (length (head bd))
    where 
        goThroughRows currRow maxRow
            | currRow >= maxRow = ""
            | otherwise = (rowToStr bd currRow playerToChar) ++ "\n" ++ goThroughRows (currRow + 1) maxRow

  -- Converts the row into the correct char representation.
  rowToStr :: [[Int]] -> Int -> (Int -> Char) -> [Char]    
  rowToStr [] _ _ = ""
  rowToStr [x] currRow playerToChar = [playerToChar (x!!currRow)]
  rowToStr (x:xs) currRow playerToChar = rowToStr xs currRow playerToChar ++ " " ++ [playerToChar (x!!currRow)]