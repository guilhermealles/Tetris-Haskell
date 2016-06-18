module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Data.Time.Clock

width, height, offset, fps :: Int
width = 300     -- 30 * 10
height = 660   -- 30 * 22
offset = 10
fps = 1

type Position = (Int,Int)

data GameState = State {
    -- List of blocks currently settled in the game
    blocks :: [TetrisBlock],
    -- Block currently moving
    movingBlock :: TetrisBlock,
    -- Next block to be spawned
    nextBlock :: TetrisBlock
}

data TetrisBlock = Block {
    -- The relative Position in GRID coordinates of the 4 squareUnits that make a block
    squareUnitPositions :: [Position],
    -- The color of the block
    blockColor :: Color,
    -- The position of the whole block in GRID coordinates
    blockGridPosition :: Position
}

-- | Given a list of blocks and a position, returns whether the position collides with some block in the list
checkCollisionWithAllBlocks :: [TetrisBlock] -> Position -> Bool
checkCollisionWithAllBlocks blocks position = any (checkCollisionWithBlock position) blocks

-- | Given a position and a block, returns whether the position collides with the block
checkCollisionWithBlock :: Position -> TetrisBlock -> Bool
checkCollisionWithBlock position block = any (\x -> x == position) (squareUnitGridPositions block)

-- | Steps the game state once
stepGameState :: Float -> GameState -> GameState
stepGameState seconds currentState =
    if finished currentState then
        currentState
    else
        if possibleToMove (movingBlock currentState) currentState then 
            moveMovingBlockDown currentState
        else
            applyGravity (removeCompleteRows (currentState {    blocks = (movingBlock currentState):(blocks currentState), 
                                                                movingBlock = (nextBlock currentState),
                                                                nextBlock = (getRandomBlock currentState) }))

    where
        -- | Returns True if the game is finished, False otherwise
        finished :: GameState -> Bool
        finished currentState = False

        -- | Returns true if it is possible to move the given block downwards on a given game state
        possibleToMove :: TetrisBlock -> GameState  -> Bool
        possibleToMove block currentState = (not (any (\(x,y) -> y<0) (nextPositionsDownList block)))
                                         && (not (any (checkCollisionWithAllBlocks (blocks currentState)) (nextPositionsDownList block)))

        -- | Given a tetris block, return the next positions of the squareUnits of the block if it was moved downward
        nextPositionsDownList :: TetrisBlock -> [Position]
        nextPositionsDownList block = map (\(x,y) -> (x,y-1)) (squareUnitGridPositions block)

        -- | Given a game state, check and remove complete rows
        removeCompleteRows :: GameState -> GameState
        removeCompleteRows state = recRemoveCompleteRows 0 state

        -- | Recursively delete all complete rows in a game state
        recRemoveCompleteRows :: Int -> GameState -> GameState
        recRemoveCompleteRows 22 state = state
        recRemoveCompleteRows row state = 
            if checkCompleteRow state row then
                recRemoveCompleteRows (row+1) state { blocks = (removeRow row (blocks state)) }
            else
                recRemoveCompleteRows (row+1) state

        -- | Given a row and the list of TetrisBlock, remove the squareUnits that match the row
        removeRow :: Int -> [TetrisBlock] -> [TetrisBlock]
        removeRow row state = recRemoveRow state row 0

        recRemoveRow :: [TetrisBlock] -> Int -> Int -> [TetrisBlock]
        recRemoveRow blocks _ 10 = blocks
        recRemoveRow blocks row column = recRemoveRow (recRemoveSquareUnit blocks row column) row (column+1)

        -- | Given a list of blocks and a coordinate, remove the squareUnit from the block that matches the coordinate
        recRemoveSquareUnit :: [TetrisBlock] -> Int -> Int -> [TetrisBlock]
        recRemoveSquareUnit [] _ _ = []
        recRemoveSquareUnit blocks row column = (removeSquareUnitFromBlock (head blocks) (column, row)):(recRemoveRow (tail blocks) row column)

        -- | Removes the squareUnit that matches the position on the TetrisBlock
        removeSquareUnitFromBlock :: TetrisBlock -> Position -> TetrisBlock
        removeSquareUnitFromBlock block (x,y) = block { 
            squareUnitPositions = (filter 
                (filterFunction (blockGridPosition block) (x,y)) 
                (squareUnitPositions block)) }
            where
                -- | This is the filter function
                filterFunction :: Position -> Position -> Position -> Bool
                filterFunction (blockPosX, blockPosY) (targetPosX, targetPosY) (x,y) = 
                    (blockPosX+x, blockPosY+y)/=(targetPosX,targetPosY)

        -- | Moves the moving block down.
        moveMovingBlockDown :: GameState -> GameState
        moveMovingBlockDown currentState = currentState { movingBlock = ((movingBlock currentState) {  blockGridPosition = (x,y') }) } 
            where
                (x,y) = (blockGridPosition (movingBlock currentState))
                y' = y-1

        -- | Given a game state, move all possible blocks down. This function should be called 
        -- after deleting a row
        applyGravity :: GameState -> GameState
        applyGravity currentState = 
            currentState {
                blocks = (map (moveDown currentState) (blocks currentState))
            }

        moveDown :: GameState -> TetrisBlock -> TetrisBlock
        moveDown state block = 
            if (filteredPossibleToMove (blocks state) block) then
                block { blockGridPosition = (x,y') }
            else 
                block
            where
                (x,y) = (blockGridPosition block)
                y' = y-1
        
        -- | Given a list of blocks and a block, filter the block off of the list (don't consider)
        -- the block itself then check for collisions
        filteredPossibleToMove :: [TetrisBlock] -> TetrisBlock -> Bool
        filteredPossibleToMove blocksList block =      (not (any (\(x,y) -> y<0) (nextPositionsDownList block)))
                                                &&  (not (any (checkCollisionWithAllBlocks filteredList) (nextPositionsDownList block)))
            where
                filteredList = filter (filterFunction block) blocksList

                filterFunction :: TetrisBlock -> TetrisBlock -> Bool
                filterFunction block1 block2 = 
                    ((blockGridPosition block1) /= (blockGridPosition block2))

        -- | Given a game state, return a "random" number
        randomFromGameState :: GameState -> Int
        randomFromGameState state = recRandomFromBlocks (blocks state)
                
        recRandomFromBlocks :: [TetrisBlock] -> Int
        recRandomFromBlocks [] = 0
        recRandomFromBlocks blocks = (x + y + (length (squareUnitPositions (head blocks)))) + (recRandomFromBlocks (tail blocks))
            where
                (x,y) = (blockGridPosition (head blocks))

        -- | Given a game state, use this state to generate a pseudorandom number and get a random block from that number
        getRandomBlock :: GameState -> TetrisBlock
        getRandomBlock state = 
            if ((randomNumber `mod` 7) == 0) then
                iBlock
            else if ((randomNumber `mod` 7) == 1) then
                lBlock
            else if ((randomNumber `mod` 7) == 2) then
                jBlock
            else if ((randomNumber `mod` 7) == 3) then
                oBlock
            else if ((randomNumber `mod` 7) == 4) then
                sBlock
            else if ((randomNumber `mod` 7) == 5) then
                zBlock
            else
                tBlock
            where
                randomNumber = (randomFromGameState state)

-- | Returns True if the row can be deleted
checkCompleteRow :: GameState -> Int -> Bool
checkCompleteRow state row = recCheckCompleteRow state 0 row
    where
        -- | Recursively checks every column in the line.
        recCheckCompleteRow :: GameState -> Int -> Int -> Bool
        recCheckCompleteRow state 9 row   = gridTileOccupied (blocks state) (9, row)
        recCheckCompleteRow state x row   = (gridTileOccupied (blocks state) (x,row)) && (recCheckCompleteRow state (x+1) row)

 -- | Returns a boolean indicating if the specified tile is occupied or not
gridTileOccupied :: [TetrisBlock] -> Position -> Bool
gridTileOccupied [] _ = False
gridTileOccupied blocks position = 
    if (any (\x -> x == position) (squareUnitGridPositions (head blocks))) then
        True
    else
        gridTileOccupied (tail blocks) position

squareUnitGridPositions :: TetrisBlock -> [Position]
squareUnitGridPositions block = map (anonymousFunc (blockGridPosition block)) (squareUnitPositions block)
    where
        anonymousFunc :: Position -> Position -> Position
        anonymousFunc (x,y) (x',y') = (x+x', y+y')

main :: IO()
main = play window background fps initialState renderState keyboardCallbacks stepGameState
    where
        -- | Respond to keyboard events
        keyboardCallbacks :: Event -> GameState -> GameState
        -- R key -> Reset the game
        keyboardCallbacks (EventKey (Char 'r') (Down) _ _) _ = initialState
        -- Arrow keys
        keyboardCallbacks (EventKey (SpecialKey KeyLeft) (Down) _ _) state = 
            state { movingBlock = ((movingBlock state) { blockGridPosition = (x',y) }) }
            where
                (x,y) = (blockGridPosition (movingBlock state))
                x' = if (not (any (\(x,y) -> x < 0) (nextPositionsGeneric (-1,0) (movingBlock state)))) && 
                        (not (any (checkCollisionWithAllBlocks (blocks state)) (nextPositionsGeneric (-1,0) (movingBlock state))))
                        then 
                            x-1 
                        else 
                            x

        keyboardCallbacks (EventKey (SpecialKey KeyRight) (Down) _ _) state = 
            state { movingBlock = ((movingBlock state) { blockGridPosition = (x',y) }) }
            where
                (x,y) = (blockGridPosition (movingBlock state))
                x' = if (not (any (\(x,y) -> x > 9) (nextPositionsGeneric (1,0) (movingBlock state)))) && 
                        (not (any (checkCollisionWithAllBlocks (blocks state)) (nextPositionsGeneric (1,0) (movingBlock state))))
                        then 
                            x+1 
                        else 
                            x

        keyboardCallbacks (EventKey (SpecialKey KeyDown) (Down) _ _) state = 
            state { movingBlock = ((movingBlock state) { blockGridPosition = (x,y') }) }
            where
                (x,y) = (blockGridPosition (movingBlock state))
                y' = if (not (any (\(x,y) -> y < 0) (nextPositionsGeneric (0,-1) (movingBlock state)))) && 
                        (not (any (checkCollisionWithAllBlocks (blocks state)) (nextPositionsGeneric (0,-1) (movingBlock state))))
                        then 
                            y-1 
                        else 
                            y

        keyboardCallbacks (EventKey (SpecialKey KeyUp) (Down) _ _) state = 
            state { movingBlock = rotateBlockClockwise state (movingBlock state) }

        keyboardCallbacks _ state = state

        -- | Given a Position (direction) in which to move the block, returns the list with
        -- the positions of the squareUnits of the block
        nextPositionsGeneric :: Position -> TetrisBlock -> [Position]
        nextPositionsGeneric (dirX,dirY) block = map (\(x,y) -> (x+dirX,y+dirY)) (squareUnitGridPositions block)

         -- | Given a game state and a block, rotate the block clockwise IF IT IS POSSIBLE
        rotateBlockClockwise :: GameState -> TetrisBlock -> TetrisBlock
        rotateBlockClockwise state block =
            if  (any (checkCollisionWithAllBlocks filteredList) (squareUnitGridPositions rotatedBlock)) || 
                (any (\(x,y) -> y<0) (squareUnitGridPositions rotatedBlock)) ||
                (any (\(x,y) -> x<0) (squareUnitGridPositions rotatedBlock)) ||
                (any (\(x,y) -> x>9) (squareUnitGridPositions rotatedBlock)) then
                    block
            else
                rotatedBlock
            where
                rotatedBlock = block { squareUnitPositions = rotatedPositions $ squareUnitPositions block }

                rotatedPositions :: [Position] -> [Position]
                rotatedPositions positions = map (\(x,y) -> (y,-1*x)) positions

                filteredList = filter (filterFunction block) (blocks state)
                filterFunction :: TetrisBlock -> TetrisBlock -> Bool
                filterFunction block1 block2 = 
                    ((blockGridPosition block1) /= (blockGridPosition block2))

-- | Background of the game
background :: Color
background = black

-- | Window of the game
window :: Display
window = InWindow "Tetris" (width, height) (offset, offset)

-- | Given a game state, draw the grid on the screen
renderState :: GameState -> Picture
renderState state = pictures (  (makeTetrisBlockRec (movingBlock state)) :
                                (map makeTetrisBlockRec (blocks state))    )

-- | Given a block, draw it on the screen
makeTetrisBlockRec :: TetrisBlock -> Picture
makeTetrisBlockRec block = pictures (map (drawOffsetBlock (blockColor block) (x,y)) (squareUnitPositions block))
    where
        (x,y) = (blockGridPosition block)

        -- | Given a color, an offset and a position on the grid, draw the scquare of the specified color at the position with the offset
        drawOffsetBlock :: Color -> Position -> Position -> Picture
        drawOffsetBlock col (x, y) (offsetX, offsetY) = pictures [
            translate gx gy $ translate grid0x grid0y $ color col $ rectangleSolid squareUnit squareUnit,
            translate gx gy $ translate grid0x grid0y $ color white $ rectangleWire squareUnit squareUnit ]
            where
                -- | Translates the (0,0) coordinates on the screen to the (0,0) coordinates on the grid
                grid0x :: Float
                grid0x = -(squareUnit*4.5)
                grid0y :: Float
                grid0y = -(squareUnit*10.5)

                -- | Converts grid coordinates into screen coordinates for translation purposes
                gx :: Float
                gx = ((fromIntegral (x+offsetX)) * squareUnit)
                gy :: Float
                gy = ((fromIntegral (y+offsetY)) * squareUnit)

        -- | Side of the square unit that makes all the blocks
        squareUnit :: Float
        squareUnit = fromIntegral width / 10 

-- | A sample state
initialState :: GameState
initialState = State {
    blocks = [ ],
    movingBlock = oBlock { blockGridPosition = (5,22) },
    nextBlock = iBlock { blockGridPosition = (5,22) }
}

-- The I block
iBlock :: TetrisBlock
iBlock = Block {
    squareUnitPositions = [(0,0), (0,-1), (0,1), (0,2)],
    blockColor = dark cyan,
    blockGridPosition = (5,22) -- A block starts centered at the top
}

-- The L block
lBlock :: TetrisBlock
lBlock = Block {
    squareUnitPositions = [(0,0), (1,0), (0,1), (0,2)],
    blockColor = orange,
    blockGridPosition = (5,22) -- A block starts centered at the top
}

-- The J block
jBlock :: TetrisBlock
jBlock = Block {
    squareUnitPositions = [(0,0), ((-1),0), (0,1), (0,2)],
    blockColor = blue,
    blockGridPosition = (5,22) -- A block starts centered at the top
}

-- The O block
oBlock :: TetrisBlock
oBlock = Block {
    squareUnitPositions = [(0,0), (0,(-1)), (1,0), (1,(-1))],
    blockColor = dark yellow,
    blockGridPosition = (5,22) -- A block starts centered at the top
}

-- The S block
sBlock :: TetrisBlock
sBlock = Block {
    squareUnitPositions = [(0,0), (1,0), (0,(-1)), ((-1),(-1))],
    blockColor = dark green,
    blockGridPosition = (5,22) -- A block starts centered at the top
}

-- The Z block
zBlock :: TetrisBlock
zBlock = Block {
    squareUnitPositions = [(0,0), ((-1),0), (0,(-1)), (1,(-1))],
    blockColor = red,
    blockGridPosition = (5,22) -- A block starts centered at the top
}

-- The T block
tBlock :: TetrisBlock
tBlock = Block {
    squareUnitPositions = [(0,0), (1,0), ((-1),0), (0,1)],
    blockColor = dark magenta,
    blockGridPosition = (5,22) -- A block starts centered at the top
}







