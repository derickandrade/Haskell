-- Fornece uma lista e um índice. Retorna o elemento no índice fornecido. Caso o índice esteja fora da lista, retorna o último elemento.
retornaElementoN :: (Eq t) => [t] -> Int -> t
retornaElementoN (a:as) n
    -- Se o contador chegou a 0 ou é o último elemento da lista, retorna o elemento atual
    | as == [] = a
    | n == 0 = a
    -- Senão, decrementa o contador e chama a função de novo para o resto da lista
    | otherwise = retornaElementoN as (n-1)
    

main = do
    putStrLn $ show(retornaElementoN [0,1,2,3] 0)
    putStrLn $ show(retornaElementoN [1,2,3] 1)
    putStrLn $ show(retornaElementoN ["A","B","C","D"] 2)
    putStrLn $ show(retornaElementoN [1] 2)
