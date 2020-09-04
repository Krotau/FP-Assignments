module Database
where

type Person  =  (Name, Age, FavouriteCourse)

type Name             =  String
type Age              =  Integer
type FavouriteCourse  =  String

frits, peter, ralf :: Person
frits  =  ("Frits",  33,  "Algorithms and Data Structures")
peter  =  ("Peter",  57,  "Imperative Programming")
ralf   =  ("Ralf",   33,  "Functional Programming")
tim    =  ("Tim", 22, "Function Programming")

students   ::  [Person]
students   =  [frits, peter, ralf, tim]

-- age              :: Person -> Age

age :: Person -> Age
age (_n, a, _c)  =  a

-- name             :: Person -> Name

name :: Person -> Name
name (n, _a, _c) = n

-- favouriteCourse  :: Person -> FavouriteCourse

course :: Person -> FavouriteCourse
course (_n, _a, c) = c

-- showPerson       :: Person -> String

showPerson :: Person -> String
showPerson (n, a, c) = n ++ ", " ++ show a ++ ", " ++ c

-- twins            :: Person -> Person -> Bool

twins :: (Person,Person) -> Bool
twins (x, y) = age(x) == age(y)

-- increaseAge :: Person -> Person
increaseAge :: Person -> Person
increaseAge (n, a, c) = (n, a+1, c)