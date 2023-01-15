checkUpper :: Char -> Bool
checkUpper x = x `elem` ['A'..'Z']


votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]


count :: String -> Int
count a = length (filter (==a) votes)


cars :: [(String,Int)]
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

checkCars :: String -> Bool
checkCars brand = (\x -> snd . fst $ x) . filter (\x -> fst x == brand) $ cars