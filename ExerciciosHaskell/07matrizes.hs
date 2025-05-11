somaMatricial :: [[Int]] -> [[Int]] -> [[Int]]
somaMatricial [] _ = []
somaMatricial (a:as) (b:bs) = somaLinha a b : somaMatricial as bs

somaLinha :: [Int] -> [Int] -> [Int]
somaLinha [] _ = []
somaLinha (x:xs) (y:ys) = [x+y] ++ somaLinha xs ys

matrizTransposta :: [[Int]] -> [[Int]]
matrizTransposta [] = []
matrizTransposta ([]:_) = []
matrizTransposta m = constroiLinha m : matrizTransposta (map tail m)

constroiLinha :: [[Int]] -> [Int]
constroiLinha [] = []
constroiLinha (a:as) = head a : constroiLinha as

multiplicacaoMatricial :: [[Int]] -> [[Int]] -> [[Int]]
multiplicacaoMatricial [] _ = []
multiplicacaoMatricial _ [] = []
multiplicacaoMatricial a b = multiplica a (matrizTransposta b) -- Com a b transposta a multiplicação fica direta

elemento :: [Int] -> [Int] -> Int
elemento [] [] = 0
elemento (a:as) (b:bs) = (a*b) + elemento as bs

logicaLinha :: [Int] -> [[Int]] -> [Int]
logicaLinha _ [] = []
logicaLinha (a:as) ((b:bs):resto) = elemento (a:as) (b:bs) : logicaLinha (a:as) resto

multiplica :: [[Int]] -> [[Int]] -> [[Int]]
multiplica [] _ = []
multiplica (a:as) (b:bs) = logicaLinha a (b:bs) : multiplica as (b:bs)
 
matriz 1 = [[1,2,3],[4,5,6],[7,8,9]]
matriz 2 = [[1,2,3],[1,2,3],[2,2,2]]

main = do
    putStrLn $ show( somaMatricial (matriz 1) (matriz 1) )
    putStrLn $ show( matrizTransposta (matriz 1) )
    putStrLn $ show( multiplicacaoMatricial (matriz 1) (matriz 2))
