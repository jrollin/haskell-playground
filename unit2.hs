-- ## lesson 2

-- Write a function that takes a value n. If n is even, the function returns n - 2, and if the number is odd, the function returns 3 × n + 1. 

magic n =   if even n
            then n - 2
            else 3 * n + 1


-- ## lesson 3 : lambdas

-- rewrite with lambdas:

-- doubleDouble x = dubs*2
-- where dubs = x*2

doubleDouble x = (\x -> x^2) x*2


-- rewrite with lambdas

-- counter x = let x = x + 1
--             in
--              let x = x + 1
--              in
--               x

counter x = (\x -> x + 1)
            ((\x -> x + 1)
             ((\x -> x) x))


-- ## Lesson 4. First-class functions 

sfOffice name = if lastName < "L"
                then nameText
                     ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                     ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where lastName = snd name
        nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

dcOffice name = nameText ++ " PO Box 1337 - Washington DC, 20001"
  where nameText = (fst name) ++ " " ++ (snd name) ++ ", Esq."

getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "dc" -> dcOffice
  _ -> (\name -> (fst name) ++ " " ++ (snd name))


-- usage:  getLocationFunction "sf" ("julien","rollin")
