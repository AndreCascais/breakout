module Main where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.Vector


type Circle = (Point, Vector)
type Box = (Segment, Segment, Segment, Segment)
type Segment = (Point, Vector)


type Obj = (Shape, Coords)
type Coords = (Point, Vector)

data Shape = Window Box
    | Block Box
    | Bar Box
    | Ball
    deriving (Show)

data IntersectionType = Corner
    | Horizintal
    | Vertical
    | Null
    deriving (Eq, Show)

data GameState = GameState Obj [Obj]
    deriving (Show)

fps, height, weight :: Int
fps = 60
maxX = 150
maxY = 150
height = 800
weight = 600

ballRadius, precision :: Float
ballRadius = 10
precision = 0.001

makeWindow :: Float -> Float -> Point -> Obj
makeWindow x y (px, py) = (Window box, ((0, 0), (0, 0)))
    where box = (((0 + px, 0 + py), (x, 0)), ((x + px, 0 + py), (0, y)), ((x + px, y + py), (-x, 0)), ((0 + px, y + py), (0, -y)))

draw :: GameState -> Picture
draw (GameState ball others)  =
    pictures (map drawObj (ball : others))

drawObj :: Obj -> Picture
drawObj (shape, ((x, y), _)) = translate x y $ drawShape shape

drawShape :: Shape -> Picture
drawShape Ball = color blue ball
drawShape (Window _) = Blank

ball :: Picture
ball = circleSolid ballRadius


initialBall :: IO Circle
initialBall = do
    --x <- randomRIO (- maxX, maxY)
    --y <- randomRIO (- maxY, maxY)
    return ((-200, 0), (100, 100))


update :: Float -> GameState -> GameState
update dt (GameState b segList) = GameState b' segList
    where b' = updateObj dt b segList

updateObj :: Float -> Obj -> [Obj] -> Obj
updateObj dt (Ball, coords) segList = (Ball, updateBall dt coords (head segList))


updateBall :: Float -> Circle -> Obj -> Circle 
updateBall dt ((x, y), (vx, vy)) obj
    | intType == Horizintal = ((x - dx, y + dy), (-vx, vy))
    | intType == Vertical = ((x + dx, y - dy), (vx, -vy))
    | otherwise = ((x + dx, y + dy), (vx, vy))
    where
        intType = intersectType dt ((x, y), (vx, vy)) obj
        dx = vx * dt
        dy = vy * dt


--intersectCorner :: Float -> Ball -> Obj -> Bool
--intersectCorner dt ((x, y), (vx, vy)) (Block )

intersectType :: Float -> Circle -> Obj -> IntersectionType
intersectType dt ball (Window (s1, s2, s3, s4), (pos, _))
        | intersectsSegment dt ball s1 = Vertical
        | intersectsSegment dt ball s2 = Horizintal
        | intersectsSegment dt ball s3 = Vertical
        | intersectsSegment dt ball s4 = Horizintal
        | otherwise = Null

intersectsSegment :: Float -> Circle -> Segment -> Bool
intersectsSegment dt ((x, y), (vx, vy)) ((a, b), (c, d)) =
    not (px > c || py > d) && --case when the point is outside the segment, will catch that with corner
    magV (x + vx - (a + px), y + vy - (b + py)) <= ballRadius
    where (px, py) = projectVector (x + vx - a, y + vy - b) (c, d)
    --where (px, py) = projectVector (x + vx - a, y + vy - b) (a + c, b + d)

-- project a into b
projectVector :: Vector -> Vector -> Vector
projectVector a b = mulSV scalar (normalizeV b)
    where scalar = dotV a (mulSV (1 / magV b) b)
            

initialState :: IO GameState
initialState = do
    ballCoords <- initialBall
    return (GameState (Ball, ballCoords) [makeWindow (fromIntegral 100) (fromIntegral 100) (0, 0)])--(myZip (windowR maxX maxY)))

window :: Display
window = InWindow "breakout" (height, weight) (0, 0)

main :: IO ()
main = do
    initial <- initialState
    simulate window black fps initial draw (\_ -> update)