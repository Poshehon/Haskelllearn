-- №1 Выяснить, четна ли сумма числе в списке
oddOrEven :: Integral a => [a] -> String
oddOrEven [] = "even"
oddOrEven x = if ((sum x) `mod` 2 == 0) then "even" else "odd"

-- №2 Тест простоты числа
limit :: Integer -> Integer
limit n
    | n <= 0 = 0
    | otherwise = ceiling (sqrt (fromInteger n))

num :: Integer -> [Integer]
num n
    | n <= 1 = []
    | otherwise = [2..(limit n)]
 
isPrime :: Integer -> Bool
isPrime n
    | n <= 1 = False
    | n == 2 = True
    | otherwise = if product (map (mod n) (num n)) == 0 then False else True

-- №3 Превратить строку в последовательность их номеров по алфавиту
alphabet :: Char -> String
alphabet 'a' = " 1"
alphabet 'b' = " 2"
alphabet 'c' = " 3"
alphabet 'd' = " 4"
alphabet 'e' = " 5"
alphabet 'f' = " 6"
alphabet 'g' = " 7"
alphabet 'h' = " 8"
alphabet 'i' = " 9" 
alphabet 'j' = " 10"
alphabet 'k' = " 11"
alphabet 'l' = " 12"
alphabet 'm' = " 13"
alphabet 'n' = " 14"
alphabet 'o' = " 15"
alphabet 'p' = " 16"
alphabet 'q' = " 17"
alphabet 'r' = " 18"
alphabet 's' = " 19"
alphabet 't' = " 20"
alphabet 'u' = " 21"
alphabet 'v' = " 22"
alphabet 'w' = " 23"
alphabet 'x' = " 24"
alphabet 'y' = " 25"
alphabet 'z' = " 26"
alphabet 'A' = " 1"
alphabet 'B' = " 2"
alphabet 'C' = " 3"
alphabet 'D' = " 4"
alphabet 'E' = " 5"
alphabet 'F' = " 6"
alphabet 'G' = " 7"
alphabet 'H' = " 8"
alphabet 'I' = " 9" 
alphabet 'J' = " 10"
alphabet 'K' = " 11"
alphabet 'L' = " 12"
alphabet 'M' = " 13"
alphabet 'N' = " 14"
alphabet 'O' = " 15"
alphabet 'P' = " 16"
alphabet 'Q' = " 17"
alphabet 'R' = " 18"
alphabet 'S' = " 19"
alphabet 'T' = " 20"
alphabet 'U' = " 21"
alphabet 'V' = " 22"
alphabet 'W' = " 23"
alphabet 'X' = " 24"
alphabet 'Y' = " 25"
alphabet 'Z' = " 26"
alphabet x = ""

replacer :: String -> String
replacer list = foldl (\acc x -> acc ++ (alphabet x)) [] list

alphabetPosition :: String -> String
alphabetPosition list = if replacer list == [] then [] else tail (replacer list) 

-- №4 Найти сумму всех чисел меньше заданного, которые делятся на 5 и на 3
total :: Integer -> Integer
total n = sum [x | x <- [1..(n-1)], (mod x 3 == 0) || (mod x 5 == 0)]