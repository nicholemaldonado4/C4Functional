-- Nichole Maldonado
-- Lab 3 - Main.hs
-- November 1, 2020
-- Dr. Cheon, CS3360

-- Board representation that performs all the logic such as creating the board,
-- representing the board as a string, and determining if a move was a
-- winning move.
module Board
  (isWonBy
  , isSlotOpen
  , dropInSlot
  , isFull
  , numSlot
  , mkPlayer
  , mkOpponent
  , mkBoard
  , boardToStr
  ) where

  -- Player One representation
  mkPlayer :: Int
  mkPlayer = 1

  -- Player Two Representation
  mkOpponent :: Int
  mkOpponent = 2

  -- Empty slot representation
  mkEmpty :: Int
  mkEmpty = 0

  -- Creates an m x n board with m columns and n rows
  mkBoard :: Int -> Int -> [[Int]]
  mkBoard m n = [[mkEmpty | x <- [1..n]] | x<-[1..m]]

  -- Drops player p's disc in a slot (column) i of a board bd. 
  -- Assume that the slot has an empty place to hold the dropped disc.
  dropInSlot :: [[Int]] -> Int -> Int -> [[Int]]
  dropInSlot [] _ _ = []
  dropInSlot (x:xs) i p
    | i < 0 = x:xs
    | i == 0 = (fillSlot x p) : xs
    | otherwise = x : dropInSlot xs (i - 1) p
    
  -- Drops a player's checker denoted by p in a column of
  -- a Connect Four Board, where 0 represents an empty place.
  -- The top of the column is at the head of the list.
  -- column, player, result 
  fillSlot :: [Int] -> Int -> [Int]
  fillSlot [] _ = []
  fillSlot [x] p = if x == mkEmpty then [p] else [x]
  fillSlot (x:xs:xss) p
    | x == mkEmpty && xs /= mkEmpty = p : tailX
    | otherwise = x : fillSlot tailX p
    where tailX = xs:xss

  -- Returns True if the board at column i has an empty slot 
  -- (denoted) by a 0 otherwise, returns true.
  isSlotOpen :: [[Int]] -> Int -> Bool
  isSlotOpen bd i
    | bd == [] || bd == [[]] || i < 0 || i >= (numSlot bd) || bd!!i == [] = False
    | otherwise = head (bd!!i) == mkEmpty

  -- Returns the number of columns of a board bd
  numSlot :: [[Int]] -> Int
  numSlot bd = length bd

  -- Returns true if the board is full (no 0 pieces), otherwise false.
  -- NOTE: We only check the top of each column. Even if for some
  -- strange reason (ex. function used by itself), a 0 appeared beneath
  -- other checker pieces, we cannot add a piece under already added pieces
  -- So even if we had (top of col -> [[1,0,1]]), the function would 
  -- return true because the drop function cannot put pieces in 
  -- between existing pieces in a column.
  isFull :: [[Int]] -> Bool
  isFull [] = True
  isFull (x:xs)
    | x /= [] && head x == mkEmpty = False
    | otherwise = isFull xs

  -- Determines is the player p won the game by looking at the board bd.
  -- Assumes that the newest moves are at the top of each column and the
  -- game has already been checked for moves in previous rounds.
  -- For both horizontal and vertical moves - just checks the whole board
  -- because it is be faster (see methods for detailed reasons). For
  -- diagonal, to save time we only look at the diagonals starting from
  -- the top of each column.
  isWonBy :: [[Int]] -> Int -> Bool
  isWonBy bd p
    | findVertical bd p || findHorizontal bd numRows 0 p = True
    | otherwise = searchDiagonal bd numRows p
   where numRows = length (head bd)

  -- Finds Vertical Move
  -- If player p won vertically, returns true, false otherwise.
  -- Since the array stores the columns, we look at each column entry.
  -- NOTE: Only looks at the top 4 player pieces.
  findVertical :: [[Int]] -> Int -> Bool
  findVertical [] _ = False
  findVertical (x:xs) p = searchCol x 0 || findVertical xs p
    where
        searchCol _ 4 = True
        searchCol [] _ = False
        searchCol (x:xs) count
            | x == p = searchCol xs (count + 1)
            -- Found matching, but we just found a piece that does not match
            | count > 0 || x /= mkEmpty = False 
            | otherwise = searchCol xs count
  
  -- Finds Horizontal Move
  -- width should be length (head bd) and currCol should intially be 0.
  -- Assume all columns have length width
  findHorizontal :: [[Int]] -> Int -> Int -> Int -> Bool 
  findHorizontal bd width currCol p
    | currCol >= width = False
    | otherwise = (searchRow bd 0) || findHorizontal bd width (currCol + 1) p
    where
        searchRow _ 4 = True
        searchRow [] _ = False
        searchRow (x:xs) count
            | x /= [] && x!!currCol == p = searchRow xs (count + 1)
            -- Reset count to 0 if not matching.   
            | otherwise = searchRow xs 0 
  
  -- Calls lower diag and passes in the appropriate row checks
  -- based on whether t the type is 'R' or other ('L'). bd is
  -- the board, width and height are the dimensions of bd,
  -- and p is the player
  callLowerDiag bd p width height t = lowerDiag bd colRowCheck p
    where 
        colRowCheck = if t == 'R' then \x y -> x >= width || y >= height else \x y -> x < 0 || y >= height 

  -- Calls upper diag and passes in the appropriate row checks 
  -- and next point move based on whether t the type is 'R' or 
  -- other ('L'). bd is the board, width and height are the 
  -- dimensions of bd, and p is the player
  callUpperDiag bd p width height t = upperDiag bd colRowCheck nextPts p 0
    where
        colRowCheck = if t == 'R' then \x y -> x < 0 || y < 0 else \x y -> x >= width || y < 0 || y >= height
        nextPts = if t == 'R' then \x y -> (x - 1, y - 1) else \x y -> (x + 1, y - 1)

  -- Determines if a diagonal exists given the board bd, 
  -- the width of the board, and the player p that we are checking for.
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

  -- Finds the first column of a row that is empty.
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

  -- Returns a string representation of a board bd. playerToChar is a 
  -- function that converts a player to a character representation.
  -- Assume that the board has the players 1 or 2 or 0 for empty.
  boardToStr :: (Int -> Char) -> [[Int]] -> [Char]
  boardToStr playerToChar bd = goThroughCols 0 (length (head bd))
    -- Goes col by col or the bd which is really goes row by row of the bd represented.
    where 
        goThroughCols currCol maxCol
            | currCol >= maxCol = ""
            | otherwise = (colToStr bd currCol playerToChar) ++ "\n" ++ goThroughCols (currCol + 1) maxCol

  -- Converts a row of a 2d matrix to a string. The 2d matrix represents
  -- the board bd so the string will actually represent a row of the bd.
  -- currCol is the column of the matrix that will be converted to a string.
  -- playerToChar is a function that converts a player to a character representation.
  colToStr :: [[Int]] -> Int -> (Int -> Char) -> [Char]    
  colToStr [] _ _ = ""
  colToStr [x] currCol playerToChar = [playerToChar (x!!currCol)]
  colToStr (x:xs) currCol playerToChar = colToStr xs currCol playerToChar ++ " " ++ [playerToChar (x!!currCol)]