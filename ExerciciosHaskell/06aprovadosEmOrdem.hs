aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia [] = []
-- A função funciona como um sort, mas calculando a média. Ordena tanto por nome, quanto por nota.
-- O filter vai incluir na lista, se: dado o par, a média for maior ou igual a 5
aprovadosOrdemDeMedia ((str,n1,n2):as) = filter (\(_,x) -> x >= 5 ) (aprovadosOrdemDeMedia(menores (str,n1,n2) as) ++ [(str,((n1+n2)/2))] ++ aprovadosOrdemDeMedia(maiores (str,n1,n2) as))

menores :: (String,Float,Float) -> [(String,Float,Float)] -> [(String,Float,Float)]
menores _ [] = []
menores (str,n1,n2) ((strX,n1x,n2x):as)
    | (((n1x+n2x)/2) < ((n1+n2)/2)) || ( (((n1x+n2x)/2) == ((n1+n2)/2)) && (strX < str)) = [(strX,n1x,n2x)] ++ menores (str,n1,n2) as
    | otherwise = menores (str,n1,n2) as
    
maiores :: (String,Float,Float) -> [(String,Float,Float)] -> [(String,Float,Float)]
maiores _ [] = []
maiores (str,n1,n2) ((strX,n1x,n2x):as)
    | ((n1x+n2x)/2) > ((n1+n2)/2) || ((((n1+n2)/2) == ((n1x+n2x)/2)) && (str < strX)) = [(strX,n1x,n2x)] ++ maiores (str,n1,n2) as
    | otherwise = maiores (str,n1,n2) as
    
main = putStrLn $ show(aprovadosOrdemDeMedia[("a",4.5,5),("b",5,5),("c",4.5,5.5),("d",7,6)])
