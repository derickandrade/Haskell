removeElementoRepetido:: (Eq t) => t -> [t] -> [t]
removeElementoRepetido x (y:ys)
    -- Se for o último elemento sendo analisado e ele não é repetido, retorna esse elemento
    | ys == [] && x /= y = [y]
    -- Se for o último elemento sendo analisado e ele é repetido, retorna a lista vazia
    | ys == [] && x == y = []
     -- Se o elemento for diferente do elemento atual sendo iterado, inclui o elemento atual na lista e continua analisando se o elemento se repete
    | x /= y = y : (removeElementoRepetido x ys)
    | otherwise = removeElementoRepetido x ys
    
intersecaoListas :: (Eq t) => [t] -> [t] -> [t]
-- Se alguma das listas é vazia, não existe interseção
intersecaoListas [] _ = []
intersecaoListas _ [] = []
intersecaoListas (a:as) (b:bs)
    -- Se o elemento atual pertence a segunda lista, inclui ele na lista final e continua iterando as listas, removendo esse elemento de ambas, já que ele já é uma interseção
    | elem a (b:bs) = a : intersecaoListas (removeElementoRepetido a as) (removeElementoRepetido a (b:bs))
    | otherwise = intersecaoListas as (b:bs)

main = do
    putStrLn $ show(intersecaoListas [1,2,3,6] [2,4,5,1,2,7])
    putStrLn $ show(intersecaoListas [1,1,1] [1,1,1,1])
    putStrLn $ show(intersecaoListas [1,1,1,1] [1])
    putStrLn $ show(intersecaoListas [1,1,1,1,3] [1,2])
