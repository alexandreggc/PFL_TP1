-- PFL-2022/23 - TP1 tests
-- By: Gonçalo Leão

module PFL2022TP1Tests where

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [y] = [y]
intersperse' x (y1:y2:ys) = y1:x:(intersperse' x (y2:ys))

str1 :: String
str1 = "-4"

str2 :: String
str2 = "2xyzw"

str3 :: String
str3 = "30a^10b^1c^3"

str4 :: String
str4 = "30a^10bc^3 -30z +    y        -yz"

str5 :: String
str5 = largePolyString 'x' "+" 5000

largePolyString :: (Integral a, Show a) => Char -> String -> a -> String
largePolyString varChar opStr n =  concat $ intersperse' opStr [(show i) ++ (varChar:"^") ++ (show i) | i <- [1 .. n]]

str6 :: String
str6 = "3x + 4x - 2y + y + 2x^2 - x^2"

str7 :: String
str7 = "-2y + 3x + y + 2x^2 + 4x - x^2"

str8 :: String
str8 = "-2xyz + 3xy + yzx - 5yx + 3zyx"

str9 :: String
str9 = "-2xyz + 0xy + 1yzx - 5yx + 3zyx"

str10 :: String
str10 = (largePolyString 'x' "+" 2000) ++ '-':(largePolyString 'x' "-" 2000)

str11 :: String
str11 = "x^3z + y"

str12 :: String
str12 = "y^2 + x^3z"

str13 :: String
str13 = largePolyString 'x' "+" 2000

str14 :: String
str14 = largePolyString 'x' "-" 2000

str15 :: String
str15 = largePolyString 'x' "+" 4

str16 :: String
str16 = largePolyString 'x' "-" 4

str17 :: String
str17 = "-2xyz + 3xy + 7y^2z - 5zx + 3y - 2"
