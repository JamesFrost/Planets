module Demo where
import Animation

svgHeight :: Double
svgHeight = 600

svgWidth :: Double
svgWidth = 800

--size, colour
planetShapeConfig :: [(Double, Colour)]
planetShapeConfig = 
	[
		(25, yellow), 
		(7.5, green), 
		(11, maroon), 
		(12.5, blue),
		(10, red),
		(20, gray)
	]

--speed, direction, radius
planetMovementConfig :: [(Double, OrbitDirection, Double)]
planetMovementConfig = 
	[
		(0, CounterClockwise, 0), 
		(0.5, CounterClockwise, 50), 
		(0.35, CounterClockwise, 100), 
		(0.3, Clockwise, 135),
		(0.25, CounterClockwise, 175),
		(0.20, Clockwise, 215)
	]

--size, colour, radius, orbit direction
moonConfig :: [ [(Double, Colour, Double, OrbitDirection)] ]
moonConfig = 
	[
		[], 
		[], 
		[], 
		[(5, black, 20, CounterClockwise)],
		[],
		[(7.5, purple, 35, Clockwise)]
	]

pic :: Animation
pic = allPlanets

allPlanets :: Animation
allPlanets = combine (zipWith3 buildPlanet planetShapeConfig planetMovementConfig moonConfig)

buildPlanet :: (Double, Colour) -> (Double, OrbitDirection, Double) -> [(Double, Colour, Double, OrbitDirection)]-> Animation
buildPlanet shapeConfig movementConfig moonConfig = 
	getOrbitShape movementConfig
		`plus` 
			getPlanetAnimation shapeConfig movementConfig 
				`plus`
					getAllMoonAnimations moonConfig movementConfig 

getAllMoonAnimations :: [(Double, Colour, Double, OrbitDirection)] -> (Double, OrbitDirection, Double) -> Animation
getAllMoonAnimations moonConfig movementConfig = 
	combine ( map moonAnimation moonConfig )
	where
		moonAnimation = getMoonAnimation movementConfig

getMoonAnimation :: (Double, OrbitDirection, Double) -> (Double, Colour, Double, OrbitDirection) -> Animation
getMoonAnimation movementConfig (size, colour, radius, orbitDirection) = 
	translate ( getMoonTranslation movementConfig (size, colour, radius, orbitDirection) )
		(getPlanetShape (size, colour))

getMoonTranslation :: (Double, OrbitDirection, Double) -> (Double, Colour, Double, OrbitDirection) -> Varying (Double, Double)
getMoonTranslation (speed, orbitDirection, radius) (size, colour, moonRadius, moonOrbitDirection) = 
	cycleSmooth speed 
		(getMoonCoordinates moonRadius size 0 moonOrbitDirection
			( getPlanetOrbitCoordinates radius orbitDirection ) )

getMoonCoordinates :: Double -> Double -> Double -> OrbitDirection-> [(Double, Double)] -> [(Double, Double)]
getMoonCoordinates moonRadius moonSize angle moonOrbitDirection [] = []
getMoonCoordinates moonRadius moonSize angle moonOrbitDirection ( (x,y):xs ) =	
	(getCoordinateOnCircumference angle moonRadius x cos, getCoordinateOnCircumference angle moonRadius y sin) : 
		getMoonCoordinates moonRadius moonSize (getNextOrbitAngle angle moonOrbitDirection) moonOrbitDirection xs

getNextOrbitAngle :: Double -> OrbitDirection -> Double
getNextOrbitAngle angle orbitDirection
	| orbitDirection == Clockwise = angle + 0.25
	| otherwise = angle - 0.25

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
getPlanetTranslation (speed, orbitDirection, radius) =
	cycleSmooth speed (getPlanetOrbitCoordinates radius orbitDirection)

getPlanetOrbitCoordinates :: Double -> OrbitDirection -> [(Double, Double)]
getPlanetOrbitCoordinates radius orbitDirection
	| orbitDirection == Clockwise = orbitCoordinates
	| otherwise = reverse orbitCoordinates
	where
		orbitCoordinates =
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

xCenter :: Double
xCenter = svgWidth/2

yCenter :: Double
yCenter = svgHeight/2

data OrbitDirection = Clockwise | CounterClockwise deriving (Enum, Eq)

make :: IO ()
make = writeFile "planets.svg" (svg svgHeight svgWidth pic)
