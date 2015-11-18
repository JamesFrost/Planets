module Demo where
import Animation

svgHeight :: Double
svgHeight = 1000

svgWidth :: Double
svgWidth = 1000

--individual planet config
--format: size, speed, orbit radius, colour, OrbitDirection
sunConfig :: ( Double, Double, Double, Colour, OrbitDirection )
sunConfig = (25.0, 0.0, 0.0, yellow, CounterClockwise)

earthConfig :: ( Double, Double, Double, Colour, OrbitDirection )
earthConfig = (12.5, 0.25, 150, blue, CounterClockwise)

allPlanetConfig :: [(Double, Double, Double, Colour, OrbitDirection)]
allPlanetConfig = [sunConfig, earthConfig]

	

pic :: Animation
pic = allPlanets

allPlanets :: Animation
allPlanets = joinShapes [ getPlanetConfig n | n <- allPlanetConfig ]

getPlanetConfig :: ( Double, Double, Double, Colour, OrbitDirection ) -> Animation
getPlanetConfig (size, speed, orbitradius, colour, orbitDirection) = translate (getTranslationConfig orbitradius speed orbitDirection) (withPaint (always colour) (getCircleShape size))

getTranslationConfig :: Double -> Double -> OrbitDirection-> Varying (Double, Double)
getTranslationConfig orbitradius speed orbitDirection
	| orbitradius == 0 = always (xCenter, yCenter)
	| otherwise = cycleSmooth speed (getCircleCoordinates orbitradius orbitDirection)

getCircleCoordinates :: Double -> OrbitDirection -> [ (Double, Double) ]
getCircleCoordinates radius orbitDirection = [ (getCoordinateOnCircumference angle radius xCenter cos, getCoordinateOnCircumference angle radius yCenter sin) | angle <- getAngleList orbitDirection ]

--Need refactor 
getAngleList :: OrbitDirection -> [Double]
getAngleList orbitDirection 
	| orbitDirection == Clockwise = buildAngleList [0..364]
	| otherwise = reverse (buildAngleList [0..364])

buildAngleList :: [Double] -> [Double]
buildAngleList [] = []
buildAngleList (x:xs) = x:x+0.25:x+0.5:x+0.75:buildAngleList xs
--Need refactor ^^^^^^

getCoordinateOnCircumference :: Double -> Double -> Double -> (Double -> Double) -> Double
getCoordinateOnCircumference angle radius circleMidPoint trigFunc = (circleMidPoint + radius *(trigFunc angle) )

getCircleShape :: Double -> Animation
getCircleShape radius = circle (always radius)

--need to refactor this
joinShapes :: [Animation] -> Animation
joinShapes shapeList
	| (length shapeList) > 1 = (head shapeList) `plus` joinShapes (tail shapeList)
	| (length shapeList) == 1 = head shapeList	

xCenter :: Double
xCenter = svgWidth/2

yCenter :: Double
yCenter = svgHeight/2

data OrbitDirection = Clockwise | CounterClockwise deriving (Enum, Eq)

make :: IO ()
make = writeFile "test.svg" (svg svgHeight svgWidth pic)
