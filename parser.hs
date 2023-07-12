import Data.Char

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

parse1 :: String -> String
parse1 [] = []
parse1 (x:xs) = if x == '\n' then [] else x: (parse1 xs)

parser :: String -> [String]
parser xs
    | xs == [] = []
    | otherwise = (parse1 xs): (parser rem) where
        n = length (parse1 xs)
        rem = drop (n + 1) xs

isok :: String -> Bool
isok [] = False
isok (x:xs) = if x == ' ' && take 2 xs == "= " then True else isok xs

transform :: String -> Maybe (String, String)
transform xs
    | isok xs == False = Nothing
    | otherwise = Just (st xs, fn xs) where
        st (x:xs) = if x == ' ' && take 2 xs == "= " then [] else x:(st xs)
        fn xs = drop (length (st xs) + 3) xs

total :: String -> [Maybe (String, String)]
total xs = map transform (parser xs)

parsingError :: String -> Bool
parsingError xs = if filter (== Nothing) (total xs) == [] then False else True

firstFilter :: Maybe (String, String) -> Bool
firstFilter (Just (x, y)) = x == "firstName"

lastFilter :: Maybe (String, String) -> Bool
lastFilter (Just (x, y)) = x == "lastName"

ageFilter :: Maybe (String, String) -> Bool
ageFilter (Just (x, y)) = x == "age"

incompleteError :: String -> Bool
incompleteError xs = let 
    names = filter firstFilter (total xs)
    surnames = filter lastFilter (total xs)
    ages = filter ageFilter (total xs)
        in if (names == [] || surnames == [] || ages == []) then True else False

incorrectError :: String -> Bool
incorrectError xs = let
    age = (\(Just (x,y)) -> y) (head (filter ageFilter (total xs)))
    in not (all isDigit age)

parsePerson :: String -> Either Error Person
parsePerson xs
    | parsingError xs = Left ParsingError
    | incompleteError xs = Left IncompleteDataError
    | incorrectError xs = let 
        age = (\(Just (x,y)) -> y) (head (filter ageFilter (total xs)))
        in Left (IncorrectDataError age)
    | otherwise = let
        name = (\(Just (x,y)) -> y) (head (filter firstFilter (total xs)))
        surname = (\(Just (x,y)) -> y) (head (filter lastFilter (total xs)))
        age = (\(Just (x,y)) -> y) (head (filter ageFilter (total xs)))
        in Right (Person name surname ((read age) :: Int ))