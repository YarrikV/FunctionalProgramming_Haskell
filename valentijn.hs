-- Voorgedefinieerde tabellen
maandgetal = [0,3,3,6,1,4,6,2,5,0,3,5]
jaargetal  = [0,1,2,3,5,6,0,1,3,4,5,6,1,2,3,4,6,0,1,2,4,5,6,0,2,3,4,5]
eeuwgetal  = [ (15,0),(19,0),(23,0),(16,6),(20,6),(24,6),(17,4),(21,4),
               (25,4),(18,2),(22,2),(26,2)]
weekdagen  = ["zondag","maandag","dinsdag","woensdag","donderdag","vrijdag","zaterdag"]

-- Geef het maandgetal  terug uit de tabel
zoekMaandgetal :: Int -> Int
zoekMaandgetal m = last (take m maandgetal)


-- Geef het jaargetal terug uit de tabel
zoekJaargetal :: Int -> Int
zoekJaargetal n = last (take ((mod n 28)+1) jaargetal)


-- Geef het eeuwgetal terug uit de tabel
zoekEeuwgetal :: Int -> Int
zoekEeuwgetal n  = snd $ head $ filter ((==n) . fst) eeuwgetal


-- Pas op voor negatieve indexen
zoekWeekdag :: Int -> [String] -> String
zoekWeekdag x list  = last $ take x list


-- Geef terug of een jaar een schrikkeljaar is of niet
schrikkeljaar :: Int -> Bool
schrikkeljaar j | mod j 100 == 0    = mod j 400 == 0
                | otherwise         = mod j 4 == 0

-- Bereken de weekdag
weekdag :: Int -> Int -> Int -> Int -> Int
weekdag dag maand eeuw jaar  = zoekWeekdag (mod (dag + (zoekMaandgetal maand) + (zoekJaargetal jaar) + (zoekEeuwgetal eeuw) + (if schrikkeljaar jaar then (-1) else 0)) 7) weekdagen


-- Gegeven de eeuw en het jaar geef de
-- weekdag waarop valentijn valt dat jaar.
valentijn :: Int -> Int -> String
valentijn eeuw jaar = zoekWeekdag (mod (14 + 3 + (zoekJaargetal jaar) + (zoekEeuwgetal eeuw) + (if schrikkeljaar jaar then (-1) else 0)) 7) weekdagen