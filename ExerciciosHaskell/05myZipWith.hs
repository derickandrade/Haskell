-- Dada uma função que recebe um tipo a e um tipo b e retorna um tipo c; uma lista de elementos a; uma lista de elementos b. Retorna uma lista de elementos C, os quais são formados pela função aplicada nos elementos de a e b de índice correspondente
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- Qualquer que seja a função, se uma das listas fornecidas for vazia, retorna uma lista vazia
myZipWith _ [] _ = []
myZipWith _ _ [] = []
-- Senão, aplica a função recebendo como parâmetros os elementos de índice i de cada uma das listas e concatena o resultado com a função aplicada ao resto das listas
myZipWith f (x:xs) (y:ys) = [f x y] ++ myZipWith f xs ys
