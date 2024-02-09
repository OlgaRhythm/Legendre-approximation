import System.IO
import Data.List (sort, nubBy, transpose)
import Text.Read (readMaybe)
import Control.Monad (forM_)

-- Функция чтения точек из файла 
readPointsFromFile :: FilePath -> IO [(Double, Double)]
readPointsFromFile filename = do
    contents <- readFile filename
    let pairs = map ((\[x, y] -> (read x, read y)) . words) (lines contents)
    return pairs

-- Функция удаления повторов и сортировка списка пар точек
removeDuplicatesAndSort :: [(Double, Double)] -> [(Double, Double)]
removeDuplicatesAndSort points = sort $ nubBy (\(x1, _) (x2, _) -> x1 == x2) points

-- Функция чтения порядка полинома из консоли
readDegree :: Int -> Int -> IO Int
readDegree lowerBound upperBound = do
    putStrLn $ "\nВведите порядок полинома в диапазоне от " ++ show lowerBound ++ " до " ++ show upperBound ++ ":"
    input <- getLine
    case readMaybe input of
        Just number ->
            if number >= lowerBound && number <= upperBound
                then return number
                else do
                    putStrLn "Ошибка! Введите число в указанном диапазоне."
                    readDegree lowerBound upperBound
        Nothing -> do
            putStrLn "Ошибка! Введите корректное число."
            readDegree lowerBound upperBound

-- Функция для вычисления значения полинома Лежандра
legendre :: Int -> Double -> Double
legendre n x = case n of
    0 -> 1.0
    1 -> x
    _ -> (fromIntegral (2 * n - 1) * x * legendre (n - 1) x - fromIntegral (n - 1) * legendre (n - 2) x) / fromIntegral n

-- Функция построения матрицы дизайна
designMatrix :: [Double] -> Int -> [[Double]]
designMatrix xs degree = transpose [map (legendre n) xs | n <- [0..degree]]

-- Функция для умножения двух матриц
multiplyMatrices :: [[Double]] -> [[Double]] -> [[Double]]
multiplyMatrices a b = 
    let b' = transpose b
    in [[sum $ zipWith (*) ar bc | bc <- b'] | ar <- a]

-- Функция для формирования системы уравнений
makeMatrixForGaussianMethod :: [[Double]] -> [[Double]] -> [[Double]]
makeMatrixForGaussianMethod design transformedY = zipWith (++) (multiplyMatrices (transpose design) design) (multiplyMatrices (transpose design) transformedY)

-- Аппроксимация
approximation :: [Double] -> Double -> Int -> Double
approximation coefficientC x degree = 
    sum $ zipWith (\c n -> c * legendre n x) (take (degree + 1) coefficientC) [0..degree]

-- Метод Гаусса: приведение к ступенчатой форме
gaussianReduce :: [[Double]] -> [[Double]]
gaussianReduce matrix = fixlastrow $ foldl reduceRow matrix [0..length matrix-1] where
    -- чтобы ведущий элемент в строке не был нулевым
    swap xs a b
        | a > b = swap xs b a
        | a == b = xs
        | a < b = let
        (p1,p2) = splitAt a xs
        (p3,p4) = splitAt (b-a-1) (tail p2)
        in p1 ++ [xs!!b] ++ p3 ++ [xs!!a] ++ (tail p4)

    reduceRow matrix1 r = let
        firstnonzero = head $ filter (\x -> matrix1 !! x !! r /= 0) [r..length matrix1-1]
        matrix2 = swap matrix1 r firstnonzero
        row = matrix2 !! r
        -- к 1
        row1 = map (\x -> x / (row !! r)) row
        -- -= row1 * nr (обнуление)
        subrow nr = let k = nr!!r in zipWith (\a b -> k*a - b) row1 nr
        nextrows = map subrow $ drop (r+1) matrix2
        in take r matrix2 ++ [row1] ++ nextrows

    fixlastrow matrix' = let
        a = init matrix'; row = last matrix'; z = last row; nz = last (init row)
        in a ++ [init (init row) ++ [1, z / nz]]

-- Метод Гаусса: обратная подстановка
substitute :: [[Double]] -> [Double]
substitute matrix = foldr next [last (last matrix)] (init matrix) where
    next row found = let
        subpart = init $ drop (length matrix - length found) row
        solution = last row - sum (zipWith (*) found subpart)
        in solution : found

-- Метод Гаусса: полное решение системы уравнений
gaussianMethod :: [[Double]] -> [Double]
gaussianMethod = substitute . gaussianReduce

-- Функция для вывода координат точек в столбик
writePoints :: [Double] -> [Double] -> IO()
writePoints [] [] = return ()
writePoints xs ys = do
    putStrLn $ show (head xs) ++ " \t" ++ show (head ys)
    writePoints (tail xs) (tail ys)

----------------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Аппроксимация методом наименьших квадратов на полиномах Лежандра"

    -- Чтение точек из файла
    points <- readPointsFromFile "input.txt"

    -- Удаление повторов и сортируем
    let uniqueSortedPoints = removeDuplicatesAndSort points

    -- Разделение точек на два списка
    let xPoints = map fst uniqueSortedPoints
        yPoints = map snd uniqueSortedPoints

    putStrLn "Даны координаты точек:"
    if null xPoints || null yPoints
        then putStrLn "Список координат пуст!"
        else writePoints xPoints yPoints
    
    -- Чтение порядка полинома (от 1 до N)
    degree <- readDegree 1 ((length xPoints)-1)
    
    let design = designMatrix xPoints degree

    let transformedY = map (\y -> [y]) yPoints

    let newMatr = makeMatrixForGaussianMethod design transformedY

    -- Результат решения системы уравнений методом Гаусса
    let coefficientC = gaussianMethod newMatr
    
    -- Результат в виде точек для графика в файле output.txt
    let fileName = "output.txt"
    handle <- openFile fileName WriteMode
    
    let startRange = minimum xPoints
    let endRange = maximum xPoints
    let step = 0.2

    mapM_ (\x -> do
        let y = approximation coefficientC x degree
        hPutStrLn handle (show x ++ " " ++ show y)  -- Запись в файл
        ) [startRange, startRange + step .. endRange]
        
    hClose handle
    putStrLn $ "Файл " ++ fileName ++ " записан успешно!"

    
