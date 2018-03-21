// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Expecto
open Expecto.Logging

[<EntryPoint>]
let main argv =
    runTestsInAssembly { defaultConfig with verbosity = LogLevel.Debug } argv