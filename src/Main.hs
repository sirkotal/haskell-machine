import Functions

main :: IO ()
main = do
    let input = "x := 44; if x <= 43 then x := 1; else x := 33; y := x*2;"
    let statements = parse input
    print statements