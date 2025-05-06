ultimoElemento :: (Eq t) => [t] -> Maybe t
ultimoElemento [] = Nothing
ultimoElemento (a:as)
    | as == [] = Just a
    | otherwise = ultimoElemento as

main = do
    putStrLn $ show(ultimoElemento [2,4,5,1,2,7])
    putStrLn $ show(ultimoElemento [1,1,1,1])
    putStrLn $ show(ultimoElemento [1])
    putStrLn $ show(ultimoElemento [])
