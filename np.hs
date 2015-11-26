module Demo where
import Animation

svgHeight :: Double
svgHeight = 600

svgWidth :: Double
svgWidth = 800

--individual planet config
--format: size, speed, orbit radius, colour, OrbitDirection, Moons

--planetMoonsConfig :: [()]

--sunConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
--sunConfig = (25.0, 0.0, 0.0, yellow, CounterClockwise, [])

--mercuryConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
--mercuryConfig = (7.5, 0.5, 60, green, CounterClockwise, [])

--venusConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
--venusConfig = (11, 0.35, 115, maroon, CounterClockwise, [])

--earthConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
--earthConfig = (12.5, 0.25, 160, blue, CounterClockwise, [(5, 0.25, 25, black, CounterClockwise)])

--marsConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
--marsConfig = (10, 0.4, 210, red, CounterClockwise, [])

--jupiterConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
--jupiterConfig = (20, 0.29, 260, gray, CounterClockwise, [])

--saturnConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
--saturnConfig = (16, 0.65, 300, silver, CounterClockwise, [])

--uranusConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
--uranusConfig = (14, 0.60, 335, teal, CounterClockwise, [])

--neptuneConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
--neptuneConfig = (14, 0.69, 375, navy, CounterClockwise, [])

--plutoConfig :: ( Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)] )
--plutoConfig = (5, 0.8, 450, aqua, CounterClockwise, [])

--allPlanetConfig :: [(Double, Double, Double, Colour, OrbitDirection, [(Double, Double, Double, Colour, OrbitDirection)]) ]
--allPlanetConfig = [sunConfig, earthConfig, mercuryConfig, venusConfig, marsConfig, jupiterConfig, saturnConfig, uranusConfig, neptuneConfig, plutoConfig]
--config end	


planetShapeConfig :: [(Double, Colour)]
planetShapeConfig = [(25, yellow), (7.5, green)]

planetMovementConfig :: [(Double, OrbitDirection, Double)]
planetMovementConfig = [(0, CounterClockwise, 0), (0.05, CounterClockwise, 50)]

--size, colour, radius, orbit direction
--moonConfig :: [ [(Double, Colour, Double, OrbitDirection)] ]
--moonConfig = [[], [(5, black, 25, CounterClockwise)]]

pic :: Animation
pic = allPlanets

allPlanets :: Animation
allPlanets = combine (zipWith buildPlanet planetShapeConfig planetMovementConfig)

buildPlanet :: (Double, Colour) -> (Double, OrbitDirection, Double) -> Animation
buildPlanet shapeConfig movementConfig = 
	getOrbitShape movementConfig
		`plus` 
			getPlanetAnimation shapeConfig movementConfig 

getOrbitShape :: (Double, OrbitDirection, Double) -> Animation
getOrbitShape (speed, orbitDirection, radius) =
	translate (always (xCenter, yCenter)) 
		(withBorder (always grey) (always 2.5) 
			(withoutPaint (circle (always radius))))

getPlanetAnimation :: (Double, Colour) -> (Double, OrbitDirection, Double) -> Animation
getPlanetAnimation shapeConfig movementConfig =
	translate (getPlanetTranslation movementConfig)
		(getPlanetShape shapeConfig)

getPlanetTranslation :: (Double, OrbitDirection, Double) -> Varying (Double, Double)
getPlanetTranslation (speed, orbitDirection, radius) 
	| orbitDirection == Clockwise = cycleSmooth speed (getPlanetOrbitCoordinates radius)
	| otherwise = cycleSmooth speed ( reverse (getPlanetOrbitCoordinates radius) )

getPlanetOrbitCoordinates :: Double -> [(Double, Double)]
getPlanetOrbitCoordinates radius =
	map (\(x, y) -> ( (radius*x)+xCenter, (radius*y)+yCenter) ) circleCircumferenceCoordinates

circleCircumferenceCoordinates :: [(Double, Double)]
circleCircumferenceCoordinates = 
	map getCoordinatePairOnCircumference [0.0,0.1..359]

getCoordinatePairOnCircumference :: Double -> (Double, Double)
getCoordinatePairOnCircumference angle =
	(getCoordinateOnCircumference angle 1 0 cos, getCoordinateOnCircumference angle 1 0 sin)

getCoordinateOnCircumference :: Double -> Double -> Double -> (Double -> Double) -> Double
getCoordinateOnCircumference angle radius circleMidPoint trigFunc = (circleMidPoint + radius *(trigFunc angle) )

getPlanetShape :: (Double, Colour) -> Animation
getPlanetShape (size, colour) =
	withPaint (always colour)
		(circle (always size))









--use map to build moon
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

--getPlanetShape :: ( Double, Double, Double, Colour, OrbitDirection ) -> Animation
--getPlanetShape (size, speed, orbitRadius, colour, orbitDirection) = 
--	translate (getPlanetTranslationConfig orbitRadius speed orbitDirection) 
--		(withPaint (always colour) 
--			(getCircleShape size))

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

getAngleList :: OrbitDirection -> [Double]
getAngleList orbitDirection 
	| orbitDirection == Clockwise = angles
	| otherwise = reverse angles

angles :: [Double]
angles = [0.0,0.1..359]

getCircleShape :: Double -> Animation
getCircleShape radius = circle (always radius)

xCenter :: Double
xCenter = svgWidth/2

yCenter :: Double
yCenter = svgHeight/2

data OrbitDirection = Clockwise | CounterClockwise deriving (Enum, Eq)

make :: IO ()
make = writeFile "test.svg" (svg svgHeight svgWidth pic)
