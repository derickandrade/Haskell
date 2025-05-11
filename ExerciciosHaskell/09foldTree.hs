data Tree a = Empty | Node a (Tree a) (Tree a)

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree _ acc Empty = acc
foldTree f acc (Node x esq dir) = f x (foldTree f (foldTree f acc dir) esq)

tree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)

main = putStrLn $ show (foldTree (+) 0 tree)
