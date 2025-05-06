removeElementoRepetido:: (Eq t) => t -> [t] -> [t]
removeElementoRepetido x (y:ys)
    -- Se for o último elemento sendo analisado e ele não é repetido, retorna esse elemento
    | ys == [] && x /= y = [y]
    -- Se for o último elemento sendo analisado e ele é repetido, retorna a lista vazia
    | ys == [] && x == y = []
     -- Se o elemento for diferente do elemento atual sendo iterado, inclui o elemento atual na lista e continua analisando se o elemento se repete
    | x /= y = y : (removeElementoRepetido x ys)
    | otherwise = removeElementoRepetido x ys

uniaoComRepeticao :: [t] -> [t] -> [t]
uniaoComRepeticao a b = a ++ b

uniaoSemRepeticao :: (Eq t) => [t] -> [t] -> [t]
uniaoSemRepeticao [] x = x
uniaoSemRepeticao x [] = x
uniaoSemRepeticao (a:as) (b:bs)
    -- Se o elemento faz parte da segunda lista, inclui ele na lista final, remove esse elemento de ambas listas e continua iterando
    | elem a (b:bs) = a : uniaoSemRepeticao (removeElementoRepetido a as) (removeElementoRepetido a (b:bs))
    | otherwise = a : uniaoSemRepeticao as (b:bs)

main = do
    putStrLn $ show(uniaoComRepeticao [1,2,3,6] [2,4,5,1,2,7])
    putStrLn $ show(uniaoComRepeticao [1,1,1] [1,1,1,1])
    putStrLn $ show(uniaoComRepeticao [1,1,1,1] [1])
    putStrLn $ show(uniaoComRepeticao [1,1,1,1,3] [1,2])
    putStrLn $ show(uniaoSemRepeticao [1,2,3,6] [2,4,5,1,2,7])
    putStrLn $ show(uniaoSemRepeticao [1,1,1] [1,1,1,1])
    putStrLn $ show(uniaoSemRepeticao [1,1,1,1] [1])
    putStrLn $ show(uniaoSemRepeticao [1,1,1,1,3] [1,2])
