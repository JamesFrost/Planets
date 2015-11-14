module Demo where
import Animation

pic :: Animation
pic = getAll 10

getAll :: Double -> Animation
getAll numberRectangles = joinShapes ( [getRectangleConfig n | n <- [1..numberRectangles] ] )

--need to refactor this
joinShapes :: [Animation] -> Animation
joinShapes shapeList
	| (length shapeList) > 1 = (head shapeList) `plus` joinShapes (tail shapeList)
	| (length shapeList) == 1 = head shapeList	

getRectangleConfig :: Double -> Animation
getRectangleConfig rectNumber = translate (getTranlationConfig rectNumber) ( withPaint (getPaintConfig rectNumber) (rotate(always 25) (getRectangleShape rectNumber)) )

getPaintConfig :: Double -> Varying Colour
getPaintConfig rectNumber
	| isDivisable (round rectNumber) 3 = always blue
	| isDivisable (round rectNumber) 5 = always black
	| isDivisable (round rectNumber) 2 = always green
	| otherwise = always red

getTranlationConfig :: Double -> Varying (Double, Double)
getTranlationConfig rectNumber = cycleSmooth 0.01 (getTranslationCoordinates rectNumber)

getTranslationCoordinates :: Double -> [ (Double, Double) ]
getTranslationCoordinates rectNumber = [ (x, y) | x <- [0..500], y <-[20*rectNumber] ]

getRectangleShape :: Double -> Animation
getRectangleShape rectNumber = rect (always 250) (always (20))

test :: IO ()
test = writeFile "test.svg" (svg 800 600 pic)

isDivisable :: Integer->Integer->Bool
isDivisable x y = x `mod` y == 0
