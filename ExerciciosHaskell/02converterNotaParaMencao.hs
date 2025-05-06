converterNotaParaMencao :: Float -> String
converterNotaParaMencao x
    | (x <= 10) && (x >= 9) = "SS"
    | (x <   9) && (x >= 7) = "MS"
    | (x <   7) && (x >= 5) = "MM"
    | (x <   5) && (x >= 3) = "MI"
    | otherwise = "Nota inválida."
    
main = do
    putStrLn $ converterNotaParaMencao 10
    putStrLn $ converterNotaParaMencao 8.99
    putStrLn $ converterNotaParaMencao 5.0
    putStrLn $ converterNotaParaMencao 3.01
    putStrLn $ converterNotaParaMencao 2.99
    putStrLn $ converterNotaParaMencao (-1.01)
    putStrLn $ converterNotaParaMencao 11
