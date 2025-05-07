inverteLista :: (Eq t) => [t] -> [t]
inverteLista (a:as)
    | as == [] = [a]
    | otherwise = (inverteLista as) ++ [a] -- O elemento tem que ser retornado em forma de lista

main = do
    putStrLn $ show(inverteLista [0,1,2,3])
    putStrLn $ show(inverteLista [1,2,3])
    putStrLn $ show(inverteLista ["A","B","C","D"])
    putStrLn $ show(inverteLista [1])
