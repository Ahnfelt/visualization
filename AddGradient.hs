module Main where

import Data.Array.Diff

delta = 0.1
size = 100

main = do
    content <- readFile "image_data"
    let ls = lines content
    let vs = map read ls :: [Double]
    let a = listArray ((0, 0), (size, size)) vs :: DiffArray (Int, Int) Double
    let gs = map (uncurry (gradient a)) (indices a)
    writeFile "gradient_data_x" (unlines $ map (\(x, y, z) -> show x ++ " " ++ show y ++ " " ++ show z) gs)
    


gradient m y x | x == size && y == size = gradient m (y-1) (x-1) 
gradient m y x | x == size = gradient m y (x-1) 
gradient m y x | y == size = gradient m (y-1) x
gradient m y x =  
    let gx = ((m ! (y, x+1)) - (m ! (y, x))) / delta in
    let gy = ((m ! (y+1, x)) - (m ! (y, x))) / delta in
    (gx, gy, 0)
