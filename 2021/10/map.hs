module Map (Map, VentLine, initializeMap, addLine, filterBy) where

type Map = [[Int]]
type VentLine = ((Int, Int), (Int, Int))

initializeMap :: Map
initializeMap = replicate 1000 $ replicate 1000 0

addLine :: Map -> VentLine -> Map
addLine input line 
    | x1 == x2 && y1 == y2 = newMap

    | x1 < x2 && y1 < y2 = addLine newMap ((x1+1,y1+1),(x2,y2))
    | x1 < x2 && y1 > y2 = addLine newMap ((x1+1,y1-1),(x2,y2))
    | x1 > x2 && y1 < y2 = addLine newMap ((x1-1,y1+1),(x2,y2))
    | x1 > x2 && y1 > y2 = addLine newMap ((x1-1,y1-1),(x2,y2))

    | x1 < x2 = addLine newMap ((x1+1,y1),(x2,y2))
    | x1 > x2 = addLine newMap ((x1-1,y1),(x2,y2))
    | y1 < y2 = addLine newMap ((x1,y1+1),(x2,y2))
    | y1 > y2 = addLine newMap ((x1,y1-1),(x2,y2))

    | otherwise = error "unexpected case"

    where 
        ((x1,y1),(x2,y2)) = line
        newMap = updateMatrixValue (+ 1) x1 y1 input

updateMatrixValue :: (a -> a) -> Int -> Int -> [[a]] -> [[a]]
updateMatrixValue function x = updateListValue (updateListValue function x)

updateListValue :: (a -> a) -> Int -> [a] -> [a]
updateListValue _ _ [] = []
updateListValue function 0 (x:xs) = function x:xs
updateListValue function index (x:xs) = x : updateListValue function (index - 1) xs

filterBy :: Map -> (Int -> Bool) -> Map
filterBy input predicate = map (filter predicate) input
