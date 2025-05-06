maior4 :: Int -> Int -> Int -> Int -> Int
-- Recebe 4 números e chama a função maior2 que retorna o maior de 2 números
maior4 a b c d = maior2 (maior2 a b) (maior2 c d)

maior2 :: Int -> Int -> Int
maior2 a b
    | a > b = a
    | otherwise = b

main = do
    putStrLn $ show (maior4 4 4 3 4)
    putStrLn $ show (maior4 (-1) (-2) (-3) (-4)) -- É necessário usar parênteses nos números negativos
    putStrLn $ show (maior4 (-1) (-2) 0 (-3))
    putStrLn $ show (maior4 (-1) (-2) 0 10)
