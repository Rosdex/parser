namespace Rosdex.Parser.Csv.Tests

open Rosdex.Parser.Collections
open Rosdex.Parser.Utils
open Rosdex.Parser.Csv

open Utils

open Expecto
open FsCheck
open FsUnit.Expecto

module Tests =
    module Arb =
        type Generators =
            static member fieldCaseArb =
                [   Arb.generate |> Gen.map CsvCell.Float
                    Arb.generate |> Gen.map CsvCell.Int
                    Arb.generate |> Gen.map (fun (NonEmptyString p) -> CsvCell.String p)]
                |> Gen.oneof
                |> Arb.fromGen

        let config =
            FsCheckConfig.defaultConfig
            |> FsCheckConfig.add<Generators>

    [<Tests>]
    let tests =
        testList "CsvWriter" []