module Main where

import Data.Maybe
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game

-- segment composed by a point and a direction vector
type Segment = (Point, Vector)
type Box = (Segment, Segment, Segment, Segment)

-- position and velocity for an object
type Coords = (Point, Vector)
type Level = [Block]

-- objects available in this game
data Window = Window Point       -- game window starting from a point (bottom left corner)
data Block = Block Point Int     -- block starting from a point with remaining "lives"
data Bar = Bar Coords            -- bar with position/velocity
data Ball = Ball Coords          -- ball with position/velocity


-- types of intersections between the ball and objects
data IntersectionType = Horizontal
    | Vertical
    deriving (Eq)

-- gamestate is the Ball, the Bar and a list of non moving blocks (the window is included here)
data GameState = GameState Ball Bar Window [Block] [Level]

fps, height, weight :: Int
fps = 60
maxX = 400
maxY = 300
weight = 1024
height = 800


ballRadius, blockWeight, blockHeight, barWeight, barHeight :: Float
ballRadius = 10
blockDist = 2
blockWeight = 50
blockHeight = 20
barWeight = 200
barHeight = 20
barSpeed = 200

-- default 4 colors for the blocks
blockColors :: [Color]
blockColors = [yellow, green, orange, red]

makeWindow :: Point -> Window
makeWindow = Window

makeBlock :: Point -> Int -> Block
makeBlock = Block

makeRandomBlock :: Point -> IO Block
makeRandomBlock p = do
    life <- randomLife
    return (Block p life)

makeBar :: Point -> Bar
makeBar p = Bar (p, (0, 0))

-- create "tradition" level with 10 blocks per row increasing block lives per column
makeLevel :: Int -> Level
makeLevel 1 = [makeBlock (-300 + x * (blockWeight + blockDist), 0) 1| x <- [0..10]]
makeLevel n = 
    [makeBlock (-300 + x * (blockWeight + blockDist), fromIntegral (n - 1) * (blockHeight + blockDist)) n | x <- [0..10]] ++ makeLevel (n - 1)

-- create random levels with random blocks in all positions (even unbreakable ones)
makeRandomLevel :: Int -> [IO Block]
makeRandomLevel 1 =
    [makeRandomBlock (-300 + x * (blockWeight + blockDist), 0) | x <- [0..10]]
makeRandomLevel n =
    [makeRandomBlock (-300 + x * (blockWeight + blockDist), fromIntegral (n - 1) * (blockHeight + blockDist)) | x <- [0..10]] ++ makeRandomLevel (n - 1)

-- random block generation - 1/27 chance to be a unbreakable one
randomLife :: IO Int
randomLife = do
    a <- randomRIO(0, 26)
    if a == 0 then return 100 else return ((a `mod` 4) + 1)

draw :: GameState -> Picture
draw (GameState ball bar window blocks _)  =
    pictures (drawBall ball : drawBar bar : drawWindow window : map drawBlock blocks)

drawBall :: Ball -> Picture
drawBall (Ball ((x, y), _)) = translate x y $ color blue ball

drawBar :: Bar -> Picture
drawBar (Bar ((x, y), _)) = translate x y $ color white bar

drawWindow :: Window -> Picture
drawWindow (Window (x, y)) = translate x y $ color red window

drawBlock :: Block -> Picture
drawBlock (Block (x, y) l)
    | l < 5 = translate x y $ color (blockColors !! (l - 1)) block
    | otherwise = translate x y $ color (greyN 0.5) block

ball, block, window :: Picture
ball = circleSolid ballRadius
block = polygon [(0, 0), (blockWeight, 0), (blockWeight, blockHeight), (0, blockHeight)]
window = line [(0, 0), (maxX * 2, 0), (maxX * 2, maxY * 2), (0, maxY * 2), (0, 0)]
bar = polygon [(0, 0), (barWeight, 0), (barWeight, barHeight), (0, barHeight)]

initialBall :: IO Coords
initialBall = do
    x <- randomRIO (- maxX, 0)
    return ((x, -maxY + barHeight * 2), (200, 200))

update :: Float -> GameState -> GameState
update dt gs = 
    move dt (detectCollisions dt gs)

-- function to detect and update collisions
-- a collision with the bottom segment of the window makes the level reset
-- a collision with the last breakable block of the level makes it to swap to the next one
detectCollisions :: Float -> GameState -> GameState
detectCollisions dt (GameState b bar window others (level:levels))
    | isNothing b' = GameState (resetBall b) bar window level (level:levels)
    | otherwise = if remaining == 0
        then GameState (fromJust b') bar window level levels
        else GameState (fromJust b') bar window others' (level:levels)
    where
        intersected = map updateBlock [obj | obj <- others, isJust (intersectTypeBlock dt b obj)]
        nonIntersected = [obj | obj <- others, isNothing (intersectTypeBlock dt b obj)]
        others' = nonIntersected ++ filter isAlive intersected
        remaining = length (filter isDestructible others')
        b' = updateBall dt b bar window intersected

-- checks if the block is still alive
isAlive :: Block -> Bool
isAlive (Block _ 0) = False
isAlive _ = True

-- updates block after a hit
updateBlock :: Block -> Block
updateBlock (Block p l)
    | l < 5 = Block p (l - 1)
    | otherwise = Block p l

-- checks if a block is "brekable"
isDestructible :: Block -> Bool
isDestrutible (Block p 100) = False
isDestructible _ = True

-- moves the whole world according to the fraction of the time spent (dt)
move:: Float -> GameState -> GameState
move dt (GameState b bar window blocks levels) = GameState (moveBall dt b) (moveBar dt bar) window blocks levels

-- moves ball after dt time
moveBall :: Float -> Ball -> Ball
moveBall dt (Ball ((x, y), (vx, vy))) = Ball ((x + vx * dt, y + vy * dt), (vx, vy))

-- moves bar after dt time
moveBar :: Float -> Bar -> Bar
moveBar dt (Bar ((x, y), (vx, vy)))
    | x + vx * dt + barWeight >= maxX = Bar ((maxX - barWeight, y + vy * dt), (vx, vy))
    | x + vx * dt <= -maxX = Bar ((-maxX, y + vy * dt), (vx, vy))
    | otherwise = Bar ((x + vx * dt, y + vy * dt), (vx, vy))

-- updating ball according to collisions
updateBall :: Float -> Ball -> Bar -> Window -> [Block] -> Maybe Ball
updateBall dt ball bar window blocks
    | isNothing first = Nothing
    | otherwise = Just (updateFromBlocks dt second blocks)
        where
            first = updateFromWindow  dt ball window
            second = updateFromBar dt (fromJust first) bar
-- update from window (special case where ball can touch the bottom horiziontal wall)
updateFromWindow :: Float -> Ball -> Window -> Maybe Ball
updateFromWindow  dt (Ball (p, (vx, vy))) window
    | intType == Just Vertical && vy < 0 = Nothing
    | otherwise = Just newBall
    where 
        coords = (p, (vx, vy))
        intType = intersectTypeWindow dt (Ball coords) window
        newBall = getUpdatedBall (Ball coords) intType

updateFromBar :: Float -> Ball -> Bar -> Ball
updateFromBar dt ball bar =
    getUpdatedBall ball (intersectTypeBar dt ball bar)

updateFromBlocks :: Float -> Ball -> [Block] -> Ball
updateFromBlocks dt ball [] = ball
updateFromBlocks dt ball (x:xs) = updateFromBlocks dt (getUpdatedBall ball (intersectTypeBlock dt ball x)) xs

getUpdatedBall :: Ball -> Maybe IntersectionType -> Ball
getUpdatedBall (Ball (p, (vx, vy))) (Just Vertical) = Ball (p, (vx, -vy))
getUpdatedBall (Ball (p, (vx, vy))) (Just Horizontal) = Ball (p, (-vx, vy))
getUpdatedBall ball Nothing = ball

-- resets a ball after a failed attempt to defent with the bar (level is reset afterwards)
resetBall :: Ball -> Ball
resetBall (Ball (p, (vx, vy))) = Ball (p, (vx, -vy))

-- gets intersection type between two objects (ball and another for now)
intersectTypeBar :: Float -> Ball -> Bar -> Maybe IntersectionType
intersectTypeBar dt (Ball coord) (Bar ((px, py), (vx, vy))) =
    intersectType dt coord (makeSegments ((px + dt * vx, py + dt * vy), (barWeight, barHeight)))

intersectTypeWindow :: Float -> Ball -> Window -> Maybe IntersectionType
intersectTypeWindow dt (Ball coord) (Window p) =
    intersectType dt coord (makeSegments (p, (2 * maxX, 2 * maxY)))

intersectTypeBlock :: Float -> Ball -> Block -> Maybe IntersectionType
intersectTypeBlock dt (Ball coord) (Block p l) =
    intersectType dt coord (makeSegments (p, (blockWeight, blockHeight)))

intersectType :: Float -> Coords -> Box -> Maybe IntersectionType
intersectType dt coord (s1, s2, s3, s4)
    | intersectsSegment dt coord s1 || intersectsSegment dt coord s3 = Just Vertical
    | intersectsSegment dt coord s2 || intersectsSegment dt coord s4 = Just Horizontal
    | otherwise = Nothing

-- checks if the a circle intersects a segment
intersectsSegment :: Float -> Coords -> Segment -> Bool
intersectsSegment dt ((x, y), (vx, vy)) ((a, b), (c, d)) =
    not ( magV (px, py) > magV (c, d) || not (sameQuad (px, py) (c, d))) && --case when the point is outside the segment
    magV (x + dx - (a + px), y + dy - (b + py)) <= ballRadius
    where 
        (px, py) = projectVector (x + dx - a, y + dy - b) (c, d)
        dx = vx * dt
        dy = vy * dt

-- makes 4 segments(box) from a point and the diagonal vector
makeSegments :: Coords -> Box
makeSegments ((px, py), (vx, vy)) = (((px, py), (vx, 0)),
                                    ((px + vx, py), (0, vy)),
                                    ((px + vx, py + vy), (-vx, 0)),
                                    ((px, py + vy), (0, -vy)))

-- project a into b
projectVector :: Vector -> Vector -> Vector
projectVector a b = mulSV scalar (normalizeV b)
    where scalar = dotV a (mulSV (1 / magV b) b)

-- checks if a and b are in same quad, axis count for either quad
sameQuad :: Vector -> Vector -> Bool
sameQuad (a, b) (c, d) = sameSignal a c && sameSignal b d

-- checks if two floats have the same signal
sameSignal :: Float -> Float -> Bool
sameSignal 0 b = True
sameSignal a 0 = True
sameSignal a b = a / abs a == b / abs b

-- event handler function (left and right to move bar and f1 to skip to next level)
handle :: Event -> GameState -> GameState 
handle (EventKey (SpecialKey KeyLeft) Down _ _) (GameState b (Bar (p, (vx, vy))) window blocks levels) = 
    GameState b (Bar (p, (-barSpeed, 0))) window blocks levels
handle (EventKey (SpecialKey KeyRight) Down _ _) (GameState b (Bar (p, _)) window blocks levels) = 
    GameState b (Bar (p, (barSpeed, 0))) window blocks levels
handle (EventKey (SpecialKey KeyLeft) Up _ _) (GameState b (Bar (p, _)) window blocks levels) = 
        GameState b (Bar (p, (0, 0))) window blocks levels
handle (EventKey (SpecialKey KeyRight) Up _ _) (GameState b (Bar (p, _)) window blocks levels) = 
        GameState b (Bar (p, (0, 0))) window blocks levels
handle (EventKey (SpecialKey KeyF1) Down _ _) (GameState b bar window blocks (level:levels)) =
        GameState b bar window level levels
handle ev gs = gs

-- initial levels are a cycle between 4 default and 4 random levels
initialState :: IO GameState
initialState = do
    ballCoords <- initialBall
    randomLevel <- sequence [sequence (makeRandomLevel x) | x <- [1..4]]
    return (GameState (Ball ballCoords) bar window (head levels) (cycle (levels ++ randomLevel)))
    where
        window = makeWindow (-maxX, -maxY)
        levels = [makeLevel x | x <- [1..4]]
        bar = makeBar (-barWeight / 2, -maxY)

gameWindow :: Display
gameWindow = InWindow "breakout" (weight, height) (0, 0)

main :: IO ()
main = do
    initial <- initialState
    play gameWindow black fps initial draw handle update