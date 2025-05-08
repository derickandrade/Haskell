histograma :: [String] -> [(String,Int)]
histograma [] = []
histograma as = logicaHistograma as [] -- Forncece a lista de strings e uma lista vazia que vai guardar os pares
    
logicaHistograma :: [String] -> [(String,Int)] -> [(String,Int)] -- Recebe a lista de strings e a lista de pares
logicaHistograma [] listaPares = listaPares -- Se as strings acabaram, retorna a lista final
logicaHistograma (a:as) listaPares = logicaHistograma as (iteraListaPares a listaPares) -- Senão, chama a função de novo passando o resto da lista de strings e, como lista de pares, o resultado da iteração da lista de pares com o elemento a
    
iteraListaPares :: String -> [(String,Int)] -> [(String,Int)]
-- Se for uma lista vazia ou a lista de pares chegou ao fim, retorna uma lista com o par string e o contador em 1
iteraListaPares str [] = [(str,1)]
iteraListaPares str (a:as)
-- Se a string fornecida foi encontrada na lista de pares, retorna uma lista com essa string e seu contador incrementado, concatenada com o resto da lista original
    | str == (fst a) = [(fst a, ((snd a) + 1))] ++ as
-- Senão, retorna o par atual concatenado com a lógica para procurar a string na lista
    | otherwise = [a] ++ iteraListaPares str as

main = do
    putStrLn $ show(histograma ["hello", "world", "hello"])
