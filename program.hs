import Data.Char (toLower)

simWord :: [String] -> [String] -> [String]
simWord _ [] = []
simWord [] _ = []
simWord dictionary (x:xs) = odstranDup $ vetaFun (words x) dictionary  ++ simWord dictionary xs
  where
    odstranDup [] = []
    odstranDup (x:xs) = x : odstranDup (filter (/= x) xs)

    vetaFun [] _  = []
    vetaFun (z:zs) dict 
      | length z >= 3 && matchSlov z dict /= ("", 0)  = fst (matchSlov (map toLower z) dict) : vetaFun zs dict 
      | otherwise = vetaFun zs dict
        where
            matchSlov _ [] = ("", 0)
            matchSlov slovo (y:ys)
                | length slovo /= length y = matchSlov slovo ys
                | pocetRozdiel (map toLower slovo) (map toLower y) <= 2 = (y, length y - pocetRozdiel (map toLower slovo) (map toLower y))
                | otherwise = matchSlov slovo ys
                where
                    pocetRozdiel [] _ = 0
                    pocetRozdiel _ [] = 0
                    pocetRozdiel (b:bs) (c:cs)
                        | toLower b == toLower c = pocetRozdiel bs cs
                        | otherwise = 1 + pocetRozdiel bs cs


main :: IO ()
main = do
  let dictionary = ["bitcoin", "nigeria", "money", "ethereum", "invoice", "transfer","bas"]
  let veta = ["trqnsfer","nigeria","your","m*ney", "Bitco.n" ,"and","b.tco.n" ,"to","n.ger.a", "bitcoinBa","ethereum", "b.tco.n"] --let veta = ["trqnsfer nigeria your m*ney Bitco.n and b.tco.n to n.ger.a bitcoinBa ethereum b.tco.n"]
  putStrLn $ "Vystup: " ++ show (simWord dictionary veta)