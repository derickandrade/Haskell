ordDec :: Ord t => [t] -> [t]
ordDec [] = []
-- Escolhe o primeiro elemento x, à esquerda ficam todos os elementos maiores que x, à direita todos os menores.
-- De ambos os lados chama a função novamente para ordená-los.
ordDec (a:as) = (ordDec (maiores a as)) ++ [a] ++ (ordDec (menores a as))

menores :: Ord t => t -> [t] -> [t]
menores _ [] = []
menores x (a:as)
    | a < x = a : menores x as
    | otherwise = menores x as

maiores :: Ord t => t -> [t] -> [t]
maiores _ [] = []
maiores x (a:as)
    | a > x = a : maiores x as
    | otherwise = maiores x as

main = putStrLn $ show(ordDec [0,1,2,3,2,8,4,5])
