combineString :: String -> String -> String
combineString x y
    | length x > length y       = y ++ x ++ y
    | length x == length y      = ""
    | otherwise                 = x ++ y ++ x