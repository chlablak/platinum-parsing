open FParsec
open EBNF

[<EntryPoint>]
let main _ = 
    let lines = System.IO.File.ReadAllLines("../../../../grammar.ebnf");
    let text = Array.fold (fun acc x -> acc + x) "" lines 
    printfn "%s" text

    printfn "%A" (run statements text)

    let timer = System.Diagnostics.Stopwatch ()
    timer.Start()
    for i = 0 to 1000000 do
        let tmp = run statements text
        0
    timer.Stop()
    printfn "%A ms" (float(timer.ElapsedMilliseconds) / 1000000.0)

    printfn "\npress enter to quit..."
    let _ = System.Console.ReadLine()
    0 
