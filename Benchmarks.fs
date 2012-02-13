module SMT2Parser.Benchmarks

#if INTERACTIVE
#r "FParsec"
#r "FParsecCS"

#load "Ast.fs"
#load "Parser.fs"
#endif

open System
open System.IO
open SMT2Parser.Parser

let rec allFiles dir =
    seq { yield! Directory.GetFiles dir
          yield! Seq.collect allFiles (Directory.GetDirectories dir) }

let allSMT2Files dir =
    dir |> allFiles |> Seq.filter (fun f -> f.EndsWith(".smt2") || f.EndsWith(".smt"))

let smt2Files = allSMT2Files (Path.Combine(__SOURCE_DIRECTORY__, "benchmarks"))

let sw = new System.Diagnostics.Stopwatch()

let time fn x =
    sw.Restart()
    fn x |> ignore
    sw.Stop()
    (float sw.ElapsedMilliseconds)/1000.0

let benchmarks() =
    let logFile = Path.Combine(__SOURCE_DIRECTORY__,  "results.txt")
    printfn "Starting..."
    File.WriteAllText(logFile, "Starting...\n") // rewrite the file
    smt2Files |> Seq.toArray 
              |> Array.iter (fun f ->
                                try
                                    printfn "Parsing %s" f
                                    File.AppendAllText(logFile, sprintf "Parsing %s\n" f)
                                    let duration = time parseFile f
                                    printfn "Parsing succeeded in %.2f s" duration
                                    File.AppendAllText(logFile, sprintf "Parsing succeeded in %.2f s\n" duration)
                                with
                                    ex -> printfn "Parsing failed with the error: %s" ex.Message
                                          File.AppendAllText(logFile, sprintf "Parsing failed with the error: %s\n" ex.Message)
                                          )
    printfn "Done."
    File.AppendAllText(logFile, "Done.\n")


#if INTERACTIVE
#time "on";;
let _ = benchmarks();;
#else
open System.Threading

let main() =
    let thread = new Thread(benchmarks, 50000000) // Stack size in byte
    thread.Start()
    thread.Join() // thread finishes    
    Console.ReadKey() |> ignore

do main()
#endif 