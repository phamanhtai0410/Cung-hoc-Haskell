question2 :: [Int] -> [Int]
question2 [] = []
question2 [x] = [x]
question2 [x, y] = [x, y]
question2 (x:y:z:[]) = [x, y, z]
question2 (x:rest) = []