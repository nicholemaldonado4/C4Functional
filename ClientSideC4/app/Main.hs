-- Nichole Maldonado
-- Lab 3 - Main.hs
-- October 30, 2020
-- Dr. Cheon, CS3360

-- Main application of the Connect 4 Game. Runs the game and collects user
-- input denoting the next move. Offers Multiplayer or against computer mode.
module Main where
  import Board
  import System.IO
  import System.Random

  -- Maps a player to its char representation.
  playerToChar :: Int -> Char
  playerToChar 0 = '.'
  playerToChar 1 = 'O'
  playerToChar _ = 'X'

  -- Returns a tuple that embeds the origInput in an error message.
  invalidInputTuple :: String -> (Int, String)
  invalidInputTuple origInput = (-2, "Invalid input: " ++ origInput)

  -- Checks if a piece can be placed in the parsed slot for bd.
  -- If the user decides to quit, (-1, "") will be returned. If an error occurred
  -- it will be denoted with -2. Otherwise, the parse was successful.
  checkValidSlot :: [[Int]] -> [(Int, String)] -> String -> (Int, String)
  checkValidSlot bd parsed origInput
    | result == -2 || result == - 1 = (result, msg)
    | not (isSlotOpen bd result) = (-2, "The current slot is full")
    | otherwise = (result, msg)
    where 
        (result, msg) = getInRange parsed width origInput
        width = numSlot bd
    

  -- Checks if the parsedInput is in the range [1, maxBound].
  -- If the user decides to quit, (-1, "") will be returned. If an error occurred
  -- it will be denoted with -2. Otherwise, the parse was successful.
  getInRange :: [(Int, String)] -> Int -> String -> (Int, String)
  getInRange [] _ ":q" = (-1, "")
  getInRange [] _ origInput = invalidInputTuple origInput
  getInRange [(parsedInt, remaining)] maxBound origInput
    | parsedInt < 1 || parsedInt > maxBound || remaining /= [] = invalidInputTuple origInput
    | otherwise = (maxBound - parsedInt, "")

  -- Gets the next slot from the user.
  -- If the user decides to quit, -1 is returned, otherwise a slot in the
  -- board's range will be returned.
  readSlot :: [[Int]] -> Int -> IO Int
  readSlot bd p = do
    line <- getSingleLine ("Enter a slot [1, " ++ (bdWidth bd) ++ "] for player " ++ player p ++ ": ")
    putStrLn ""
    let parsed = reads line :: [(Int, String)] in
      let (x, y) = checkValidSlot bd parsed line in
        if x == -2
        then retry y
        else return x
    where
      player p = show p
      bdWidth bd = show (numSlot bd)
      retry msg = do
        putStrLn msg
        readSlot bd p

  -- Gets a random move in the bd's range.
  getRandMove :: [[Int]] -> Int -> IO Int
  getRandMove bd p = do
    x <- randomRIO (0, 6 :: Int)
    if (isSlotOpen bd x)
    then printAndReturn x
    else getRandMove bd p
    where
      printAndReturn slotNum = do
        putStrLn ("Player " ++ show p ++ " moved: " ++ show (numSlot bd - slotNum))
        return slotNum

  -- Determines the next strategy based on the player type.
  selectStrat :: Int -> ([[Int]] -> Int -> IO Int)
  selectStrat selection
    | selection == 1 = readSlot
    | otherwise = getRandMove

  -- Returns a tuple that denotes (skipsOk, skipIt)
  -- If skipsOk, then we are playing against the computer and we do not need
  -- to print a separate board for the computer's move.  skipIt denotes whether
  -- or not we skip printing this board.
  getSkippedPair :: Int -> (Bool, Bool)
  getSkippedPair 1 = (False, False)
  getSkippedPair _ = (True, False)

  -- Prints msg and gets the input.
  -- Has to flush output since putStr is used (otherwise prints occur later)
  getSingleLine :: String -> IO String
  getSingleLine msg = do
      putStr msg
      hFlush stdout
      getLine

  -- Gets the moveSelection from the user. 1 if Multiplayer, 2 if Computer.
  moveSelection :: IO Int
  moveSelection = do
    putStrLn "Select a playing strategy"
    putStrLn "1. Multiplayer"
    putStrLn "2. Computer"
    line <- getSingleLine "Select 1 or 2: "
    return 5
    let parsed = reads line :: [(Int, String)] in
      let (x, y) = getInRange parsed 2 line in
        if x < -1
        then retry y
        else return x
    where
      retry msg = do
        putStrLn msg
        moveSelection

  -- Prints the welcome and rules of the game.
  printIntro :: IO ()
  printIntro = do
    putStrLn "Welcome to Connect 4!"
    putStrLn "---------------------"
    putStrLn "Rules of the Game: "
    putStrLn "> Player 1 will be O tokens"
    putStrLn "> Player 2 will be X tokens*"
    putStrLn "> Enter :q to quit at anytime"
    putStrLn " * If computer mode selected, then the computer will be player 2\n"

  -- Main method that runs the connect 4 application.
  main :: IO ()
  main = do
    printIntro
    selection <- moveSelection
    if selection == -1
    then return ()
    else playGame (mkBoard 7 6) mkPlayer (selectStrat selection) (getSkippedPair selection)

  -- Toggles player.
  switchPlayer :: Int -> Int
  switchPlayer p
    | p == mkPlayer = mkOpponent
    | otherwise = mkPlayer

  -- Checks if the game resulted in a win or draw.
  -- If a win or draw (True, msg) will be returned. Otherwise (False, "")
  -- returned.
  checkWinOrDraw :: [[Int]] -> Int -> (Bool, String)
  checkWinOrDraw bd p
    | isWonBy bd p = (True, "Player " ++ show p ++ " won!!!")
    | isFull bd = (True, "The game was a draw!")
    | otherwise = (False, "")

  -- Creates a String indices from a low to high value.
  createIndices :: Int -> Int -> String
  createIndices curr high
    | curr > high =  "\n"
    | otherwise = convertToStr ++ " " ++ createIndices (curr + 1) high
    where convertToStr = show curr

  -- Applies the moveStrat or readSlot and returns the slot value.
  -- If the p is mkPlayer, then readSlot is called, otherwise the moveStrat
  -- passed in is called.
  getSlot :: [[Int]] -> Int -> ([[Int]] -> Int -> IO Int) -> IO Int
  getSlot bd p moveStrat
    | p == mkPlayer = readSlot bd p
    | otherwise = moveStrat bd p

  -- Sets the skipIt tuple based on the current player p.
  nextSkipIt :: Int -> (Bool, Bool) -> (Bool, Bool)
  nextSkipIt p (skipsOk, skipIt)
    | p == mkPlayer = (skipsOk, True)
    | otherwise = (skipsOk, False)

  -- Prints the bd.
  showBoard :: [[Int]] -> IO ()
  showBoard bd = do
    putStr (boardToStr playerToChar bd)
    putStrLn (createIndices 1 7)

  -- Prints board if skipsOk and skipIt is true.
  showBoardBasedOnSkip :: [[Int]] -> (Bool, Bool) -> IO ()
  showBoardBasedOnSkip bd (skipsOk, skipIt)
    | skipsOk && skipIt = do return ()
    | otherwise = showBoard bd

  -- Plays the game. Gets the slot and prints the board on each recursive call.
  playGame :: [[Int]] -> Int -> ([[Int]] -> Int -> IO Int) -> (Bool, Bool) -> IO ()
  playGame bd p moveStrat skippedPair = do
    showBoardBasedOnSkip bd skippedPair
    slot <- getSlot bd p moveStrat
    if slot == -1
    then return ()
    else let newBd = dropInSlot bd slot p; (x, y) = checkWinOrDraw newBd p in
      if x == True
      then showEnding y newBd
      else playGame newBd (switchPlayer p) moveStrat (nextSkipIt p skippedPair)
    where
      showEnding msg board = do
        showBoard board
        putStrLn msg