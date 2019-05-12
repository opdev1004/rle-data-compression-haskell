-- encode takes String as an input and return list of (Integer, Char)
-- encode [] = [] Pattern's for empty list.
-- Call convert function first on input and then call compact function.
-- So for example, encode "aabbaaccbb" -> convert "aabbaaccbb" -> compact [(1,'a'),(1,'a'),(1,'b'),(1,'b'),(1,'a'),(1,'a'),(1,'c'),(1,'c'),(1,'b'),(1,'b')] -> [(2,'a'),(2,'b'),(2,'a'),(2,'c'),(2,'b')]
encode :: String -> [(Integer, Char)]
encode [] = []
encode x = compact(convert(x))

-- convert takes String as an input and return list of (Integer, Char)
-- convert [] = [] Pattern's for empty list.
-- It takes input as a x and xs and recursively convert String to data.
convert :: String -> [(Integer, Char)]
convert [] = []
convert (x:xs) = (1, x) : convert(xs)

-- compact takes list of (Integer, Char) as an input and return list of compacted (Integer, Char)
-- compact [] = [] Pattern's for empty list.
-- compact compact (x:xs) = x:xs Pattern's for avoiding Non-exhaustive patterns error.
-- Check vales in tuples and compact list.
compact :: [(Integer, Char)] -> [(Integer, Char)]
compact [] = []
compact ((a, b):(c, d):ys)
                          | b == d    = compact((a + 1, b):ys)
                          | otherwise = (a, b):compact((c, d):ys)
compact(x:[]) = [x]


-- decode takes list of (Integer, Char) as an input and return String
-- decode [] = [] Pattern's for empty list.
-- It calls times function and adds the rest of items in list to decode encoded string.
decode :: [(Integer, Char)] -> String
decode [] = []
decode ((x, y):xs) = times x y ++ decode(xs)

-- times takes Integer and Char as an input and return String
-- It is using enumeration to create string with number of integer.
times :: Integer -> Char -> String
times x y = [1..x] >> [y]
