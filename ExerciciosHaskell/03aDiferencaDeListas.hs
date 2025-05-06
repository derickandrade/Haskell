diferencaListas :: (Eq t) => [t] -> [t] -> [t]
diferencaListas [] _ = []
diferencaListas (a:as) (b:bs)
    -- Se é o último elemento da primeira lista e ele faz parte da segunda lista, então retorna a segunda lista sem o elemento
    | as == [] && (elem a (b:bs)) = removeElementoRepetido a (b:bs)
    -- Se é o último elemento da primeira lista e ele não faz parte da segunda lista, então retorna ele junto com a segunda lista
    | as == [] && not(elem a (b:bs)) = a : (b:bs)
    -- Se o elemento atual da primeira lista não fizer parte da segunda, inclui ele e continua a iteração
    | not(elem a (b:bs)) = a : (diferencaListas as (b:bs))
    -- Se ele fizer parte, remove todas as ocorrências desse elemento de ambas listas e continua analisando as diferenças
    | otherwise = diferencaListas (removeElementoRepetido a as) (removeElementoRepetido a (b:bs))

removeElementoRepetido:: (Eq t) => t -> [t] -> [t]
removeElementoRepetido x (y:ys)
    -- Se for o último elemento sendo analisado e ele não é repetido, retorna esse elemento
    | ys == [] && x /= y = [y]
    -- Se for o último elemento sendo analisado e ele é repetido, retorna a lista vazia
    | ys == [] && x == y = []
     -- Se o elemento for diferente do elemento atual sendo iterado, inclui o elemento atual na lista e continua analisando se o elemento se repete
    | x /= y = y : (removeElementoRepetido x ys)
    | otherwise = removeElementoRepetido x ys

main = do
    putStrLn $ show(diferencaListas [1,2,3,6] [2,4,5,1,2,7])
    putStrLn $ show(diferencaListas [1,1,1] [1,1,1,1])
    putStrLn $ show(diferencaListas [1,1,1,1] [1])
    putStrLn $ show(diferencaListas [1,1,1,1,3] [1,2])
