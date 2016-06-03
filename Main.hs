module Main(main) where

import Graphics.Gloss

width, height, offset, fps :: Int
width = 500     -- 50 * 10
height = 1100   -- 50 * 22
offset = 10
fps = 60

type Position = (Float,Float)

data TetrisBlock = Block {
    -- The relative Position of the 4 squareUnits that make a block
    squarePositions :: (Position, Position, Position, Position),
    -- The color of the block
    blockColor :: Color,
    -- The position of the whole block
    blockPosition :: Position
}

-- The I block
iBlock :: TetrisBlock
iBlock = Block {
    squarePositions = ((0,0), (0,1), (0,2), (0,3)),
    blockColor = dark cyan,
    blockPosition = (0,12) -- A block starts centered at the top
}

-- The L block
lBlock :: TetrisBlock
lBlock = Block {
    squarePositions = ((0,0), (1,0), (0,1), (0,2)),
    blockColor = orange,
    blockPosition = (0,12) -- A block starts centered at the top
}

-- The J block
jBlock :: TetrisBlock
jBlock = Block {
    squarePositions = ((0,0), ((-1),0), (0,1), (0,2)),
    blockColor = blue,
    blockPosition = (0,12) -- A block starts centered at the top
}

-- The O block
oBlock :: TetrisBlock
oBlock = Block {
    squarePositions = ((0,0), (0,(-1)), (1,0), (1,(-1))),
    blockColor = dark yellow,
    blockPosition = (0,12) -- A block starts centered at the top
}

-- The S block
sBlock :: TetrisBlock
sBlock = Block {
    squarePositions = ((0,0), (1,0), (0,(-1)), ((-1),(-1))),
    blockColor = dark green,
    blockPosition = (0,12) -- A block starts centered at the top
}

-- The Z block
zBlock :: TetrisBlock
zBlock = Block {
    squarePositions = ((0,0), ((-1),0), (0,(-1)), (1,(-1))),
    blockColor = red,
    blockPosition = (0,12) -- A block starts centered at the top
}

-- The T block
tBlock :: TetrisBlock
tBlock = Block {
    squarePositions = ((0,0), (1,0), ((-1),0), (0,1)),
    blockColor = dark magenta,
    blockPosition = (0,12) -- A block starts centered at the top
}

main :: IO()
main = display window background (pictures [ 
    (makeTetrisBlock (iBlock) 0 0),
    (makeTetrisBlock (lBlock) 100 100),
    (makeTetrisBlock (jBlock) 200 300),
    (makeTetrisBlock (oBlock) (-100) (-100)),
    (makeTetrisBlock (sBlock) (-150) (-230)),
    (makeTetrisBlock (zBlock) (100) (-100)),
    (makeTetrisBlock (tBlock) (-150) 200)  ])

-- | Background of the game
background :: Color
background = black

-- | Window of the game
window :: Display
window = InWindow "Tetris" (width, height) (offset, offset)

-- | Side of the square unit that makes all the blocks
squareUnit :: Float
squareUnit = fromIntegral width / 10

-- | Makes a single unit block of color color centered at position (x,y)
makeUnitBlock :: Color -> Float -> Float -> Picture
makeUnitBlock col x y = pictures [
    translate x y $ color col $ rectangleSolid squareUnit squareUnit,
    translate x y $ color white $ rectangleWire squareUnit squareUnit ]

-- | Given a block, draw it on the screen centered at the position (x,y)
makeTetrisBlock :: TetrisBlock -> Float -> Float -> Picture
makeTetrisBlock block x y = pictures [
    makeUnitBlock (blockColor block) (x + (px1*squareUnit)) (y + (py1*squareUnit)),
    makeUnitBlock (blockColor block) (x + (px2*squareUnit)) (y + (py2*squareUnit)),
    makeUnitBlock (blockColor block) (x + (px3*squareUnit)) (y + (py3*squareUnit)),
    makeUnitBlock (blockColor block) (x + (px4*squareUnit)) (y + (py4*squareUnit))  ]
    where
        ((px1,py1), (px2,py2), (px3,py3), (px4,py4)) = (squarePositions block)