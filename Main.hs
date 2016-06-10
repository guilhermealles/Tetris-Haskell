module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

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
    squarePositions :: (Position, Position, Position, Position),
    -- The color of the block
    blockColor :: Color,
    -- The position of the whole block in GRID coordinates
    blockGridPosition :: Position
}

-- | Steps the game state once
stepGameState :: Float -> GameState -> GameState
stepGameState seconds currentState =
    if finished currentState then
        currentState
    else
        moveDown currentState
    where
        -- | Returns True if the game is finished, False otherwise
        finished :: GameState -> Bool
        -- TODO
        finished currentState = False

        -- | Moves the moving block down. If it is not possible to move it,
        -- then place the moving block on the blocks list and spawn a new block
        moveDown :: GameState -> GameState
        moveDown currentState =
            if possibleToMove currentState then
                currentState { movingBlock = ((movingBlock currentState) { blockGridPosition = (x,y') }) }
            else
                currentState {  blocks = (movingBlock currentState):(blocks currentState), 
                                movingBlock = (nextBlock currentState),
                                nextBlock = oBlock } 
            where
                (x,y) = (blockGridPosition (movingBlock currentState))
                y' = y-1

                -- | Returns true if it is possible to move the moving block downwards
                possibleToMove :: GameState -> Bool
                possibleToMove currentState = True;


main :: IO()
main = simulate window background fps sampleState renderState viewPortIgnore

-- | viewPortIgnore
viewPortIgnore :: ViewPort -> Float -> GameState -> GameState
viewPortIgnore _ = stepGameState

-- | Background of the game
background :: Color
background = black

-- | Window of the game
window :: Display
window = InWindow "Tetris" (width, height) (offset, offset)

-- | Given a game state, draw the grid on the screen
renderState :: GameState -> Picture
renderState state = pictures (  (makeTetrisBlock (movingBlock state)) :
                                (map makeTetrisBlock (blocks state))    )


-- | Given a block, draw it on the screen
makeTetrisBlock :: TetrisBlock -> Picture
makeTetrisBlock block = pictures [
    makeGridUnitBlock (blockColor block) (px1+x) (py1+y), --(x + ((px1+x)*(squ+y)areUnit)) (y + (py1*squareUnit)),
    makeGridUnitBlock (blockColor block) (px2+x) (py2+y), --(x + ((px2+x)*(squ+y)areUnit)) (y + (py2*squareUnit)),
    makeGridUnitBlock (blockColor block) (px3+x) (py3+y), --(x + ((px3+x)*(squ+y)areUnit)) (y + (py3*squareUnit)),
    makeGridUnitBlock (blockColor block) (px4+x) (py4+y) ] --(x + ((px4+x)*(squ+y)areUnit)) (y + (py4*squareUnit))  ]
    where
        ((px1,py1), (px2,py2), (px3,py3), (px4,py4)) = (squarePositions block)

        (x,y) = (blockGridPosition block)

        -- | Makes a single unit block of color color centered at position (x,y) ON THE GRID
        makeGridUnitBlock :: Color -> Int -> Int -> Picture
        makeGridUnitBlock col x y = pictures [
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
                gx = ((fromIntegral x) * squareUnit)
                gy :: Float
                gy = ((fromIntegral y) * squareUnit)

        -- | Side of the square unit that makes all the blocks
        squareUnit :: Float
        squareUnit = fromIntegral width / 10    

-- | A sample state
sampleState :: GameState
sampleState = State {
    blocks = [  iBlock { blockGridPosition = (0,0) },
                lBlock { blockGridPosition = (2,3) },
                jBlock { blockGridPosition = (3,5) } ],
    movingBlock = oBlock { blockGridPosition = (5,22) },
    nextBlock = oBlock

}

-- The I block
iBlock :: TetrisBlock
iBlock = Block {
    squarePositions = ((0,0), (0,1), (0,2), (0,3)),
    blockColor = dark cyan,
    blockGridPosition = (5,5) -- A block starts centered at the top
}

-- The L block
lBlock :: TetrisBlock
lBlock = Block {
    squarePositions = ((0,0), (1,0), (0,1), (0,2)),
    blockColor = orange,
    blockGridPosition = (5,5) -- A block starts centered at the top
}

-- The J block
jBlock :: TetrisBlock
jBlock = Block {
    squarePositions = ((0,0), ((-1),0), (0,1), (0,2)),
    blockColor = blue,
    blockGridPosition = (5,5) -- A block starts centered at the top
}

-- The O block
oBlock :: TetrisBlock
oBlock = Block {
    squarePositions = ((0,0), (0,(-1)), (1,0), (1,(-1))),
    blockColor = dark yellow,
    blockGridPosition = (5,5) -- A block starts centered at the top
}

-- The S block
sBlock :: TetrisBlock
sBlock = Block {
    squarePositions = ((0,0), (1,0), (0,(-1)), ((-1),(-1))),
    blockColor = dark green,
    blockGridPosition = (5,5) -- A block starts centered at the top
}

-- The Z block
zBlock :: TetrisBlock
zBlock = Block {
    squarePositions = ((0,0), ((-1),0), (0,(-1)), (1,(-1))),
    blockColor = red,
    blockGridPosition = (5,5) -- A block starts centered at the top
}

-- The T block
tBlock :: TetrisBlock
tBlock = Block {
    squarePositions = ((0,0), (1,0), ((-1),0), (0,1)),
    blockColor = dark magenta,
    blockGridPosition = (5,5) -- A block starts centered at the top
}






