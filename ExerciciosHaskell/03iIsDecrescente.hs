import Data.List

-- Retorna se uma lista é decrescente, feita com recursão
isDecRec :: Ord t => [t] -> Bool
isDecRec [] = True
isDecRec [x] = True
isDecRec (a:as)
    | a < head as = False
    | otherwise = isDecRec as
    
-- Retorna se uma lista é decrescente, feita com zip, map e foldr
isDecFoldMapZip :: Ord t => [t] -> Bool
isDecFoldMapZip [] = True
isDecFoldMapZip [x] = True
-- Zip vai associar o primeiro elemento com o segundo, o segundo com o terceiro, etc.
-- map vai retornar uma lista de booleanos, usando a função "dado um par (a,b) retorna se a é maior que b", aplicando essa função em cada par feito pelo zip
-- foldr vai retornar um booleano se todos os elementos do map forem true. (Uma lista [a,b,c], o foldr faz: a && (b && (c && True)))
isDecFoldMapZip (a:b:as) = foldr (&&) True (map (\(a,b) -> a > b) (zip (a:b:as) (b:as)))

-- Retorna se uma lista é decrescente, feita com sort
isDecSort :: Ord t => [t] -> Bool
isDecSort as = (as == reverse (sort as))

main = do
    putStrLn $ show(isDecRec [0,1,2,3,2,8,4,5])
    putStrLn $ show(isDecRec [5,4,3,2,1,0])
    putStrLn $ show(isDecFoldMapZip [0,1,2,3,2,8,4,5])
    putStrLn $ show(isDecFoldMapZip [5,4,3,2,1,0])
    putStrLn $ show(isDecSort [0,1,2,3,2,8,4,5])
    putStrLn $ show(isDecSort [5,4,3,2,1,0])
