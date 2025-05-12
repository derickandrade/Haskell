data Tree a = Empty | Node a (Tree a) (Tree a)

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree _ acc Empty = acc
foldTree f acc (Node x esq dir) = f x (foldTree f (foldTree f acc dir) esq)

tree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)

-- A árvore precisa ser de inteiros pra conseguir somar
sumTree :: Tree Int -> Int
sumTree Empty = 0 -- O caso base retorna 0
sumTree (Node x esq dir) = x + sumTree esq + sumTree dir -- O caso recursivo retorna a soma do número guardado no nó, mais a soma dos elementos guardados nos nós à esquerda e à direita.

main = do
    putStrLn $ show (foldTree (+) 0 tree)
    putStrLn $ show (sumTree tree)
