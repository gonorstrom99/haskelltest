main = putStrLn "Hello, World!"
doubleUs x y =doubleMe x + doubleMe y
doubleMe x = x+x
doubleSmallNum x = if x >100 then x else x*2
                    