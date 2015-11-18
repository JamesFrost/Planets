module Demo where
import Animation

svgHeight :: Double
svgHeight = 750

svgWidth :: Double
svgWidth = 750

--individual planet config
--format: size, speed, orbit radius, colour, OrbitDirection, Moons
sunConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
sunConfig = (25.0, 0.0, 0.0, yellow, CounterClockwise, [])

mercuryConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
mercuryConfig = (5, 0.5, 60, purple, CounterClockwise, [])

venusConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
venusConfig = (11, 0.35, 115, green, CounterClockwise, [])

earthConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
earthConfig = (12.5, 0.25, 150, blue, CounterClockwise, [(5, 0.25, 25, black, CounterClockwise)])

allPlanetConfig :: [(Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)]) ]
allPlanetConfig = [sunConfig, earthConfig, mercuryConfig, venusConfig]
--allPlanetConfig = [earthConfig]
--config end	

pic :: Animation
pic = allPlanets

allPlanets :: Animation
allPlanets = combine [ buildPlanet n | n <- allPlanetConfig ]

buildPlanet :: ( Double, Double, Double, Colour, OrbitDirection,  [(Double, Double, Double, Colour, OrbitDirection)] ) -> Animation
buildPlanet (size, speed, orbitRadius, colour, orbitDirection, moonConfig) = 
	combine ((getOrbitOutline orbitRadius) : (getPlanetShape (size, speed, orbitRadius, colour, orbitDirection)) : (getMoons moonConfig orbitRadius orbitDirection))

getMoons :: [(Double, Double, Double, Colour, OrbitDirection)] -> Double -> OrbitDirection -> [Animation]
getMoons moonConfig orbitRadius orbitDirection
	| (length moonConfig) == 0 = []
	| (length moonConfig) > 1 = (getMoonShape (head moonConfig) (getCircleCoordinates orbitRadius orbitDirection) ) : (getMoons (tail moonConfig) orbitRadius orbitDirection)
	| (length moonConfig) == 1 = [getMoonShape (head moonConfig) (getCircleCoordinates orbitRadius orbitDirection)]

getMoonShape :: ( Double, Double, Double, Colour, OrbitDirection ) -> [(Double, Double)] -> Animation
getMoonShape (size, speed, orbitRadius, colour, orbitDirection) planetCoordinates = 
	translate (getMoonTranslationConfig orbitRadius speed orbitDirection planetCoordinates)
		(withPaint (always colour)
			(getCircleShape size))

getMoonTranslationConfig :: Double -> Double -> OrbitDirection -> [(Double, Double)] -> Varying (Double, Double)
getMoonTranslationConfig orbitRadius speed orbitDirection planetCoordinates = cycleSmooth speed (getMoonOrbitCoordinates 365 orbitRadius speed orbitDirection planetCoordinates)

getMoonOrbitCoordinates :: Double -> Double -> Double -> OrbitDirection -> [(Double, Double)] -> [(Double, Double)]
getMoonOrbitCoordinates currentAngle orbitRadius speed orbitDirection planetCoordinates 
	| length planetCoordinates == 0 = []
	| otherwise = 
		(getCoordinateOnCircumference (getNextAngle currentAngle) orbitRadius (getXCoord (head planetCoordinates)) cos,
			getCoordinateOnCircumference (getNextAngle currentAngle) orbitRadius (getYCoord (head planetCoordinates)) sin)  : 
				(getMoonOrbitCoordinates (getNextAngle currentAngle) orbitRadius speed orbitDirection (tail planetCoordinates) )

getNextAngle :: Double -> Double
getNextAngle currentAngle
	| currentAngle > 365  = 0
	| otherwise = currentAngle+0.25

getXCoord :: (Double, Double) -> Double
getXCoord (x, y) = x;

getYCoord :: (Double, Double) -> Double
getYCoord (x, y) = y;

getPlanetShape :: ( Double, Double, Double, Colour, OrbitDirection ) -> Animation
getPlanetShape (size, speed, orbitRadius, colour, orbitDirection) = 
	translate (getPlanetTranslationConfig orbitRadius speed orbitDirection) 
		(withPaint (always colour) 
			(getCircleShape size))

getOrbitOutline :: Double -> Animation
getOrbitOutline radius = 
	translate (always (xCenter, yCenter)) 
		(withBorder (always grey) (always 2.5) 
			(withoutPaint (circle (always radius))))

getPlanetTranslationConfig :: Double -> Double -> OrbitDirection-> Varying (Double, Double)
getPlanetTranslationConfig orbitradius speed orbitDirection
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

xCenter :: Double
xCenter = svgWidth/2

yCenter :: Double
yCenter = svgHeight/2

data OrbitDirection = Clockwise | CounterClockwise deriving (Enum, Eq)

make :: IO ()
make = writeFile "test.svg" (svg svgHeight svgWidth pic)
