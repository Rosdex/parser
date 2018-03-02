#load """Utils.Repl.fsx"""

open Rosdex.Parser.Csv
open Rosdex.Parser.Yandex.Yml
open Rosdex.Parser.Utils
open Rosdex.Parser.Collections
open Utils.Repl.IO

open System.Xml.Linq
open Microsoft.FSharpLu
open Utils

let pipe (source : string) (target : string) =
    let document = XDocument.Load source
    let offers =
        document
        |> YmlParser.tryExtractOffers
        |> List.map Result.ok
    let categories =
        document
        |> YmlParser.extractCategories
        |> Category.catalog
    match Catalog.validate categories with
    | Catalog.Validation.HasCycle ->
        failwith "Has cycle!"
    | _ ->
        let writer = CsvWriters.ExtendedOffers.ofCatalog categories
        offers
        |> Seq.choose (CsvWriter.tryStringifyValue writer)
        |> Seq.append
            [writer.Fields |> List.map (fun p -> p.Name) |> String.concat ", "]
        |> File.writeLines target

module Path =
    open System.IO

    let convertToCsv path =
        path
        |> Path.GetFileName
        |> Path.setExtension "csv"
        |> Path.combine
            (path
            |> Path.GetDirectoryName
            |> Path.GetDirectoryName
            |> Path.combine <| "CSV-Extended")

let pipes =
    Repl.gitignored.YmlSourceFilesPath.Value
    |> System.IO.Directory.EnumerateFiles
    |> Seq.map (fun p ->
        printfn "%s" p
        try
            p
            |> Path.convertToCsv
            |> pipe p
            None
        with
        | exn -> Some (p, exn))
    |> List.ofSeq

pipes
    |> List.choose id
    |> List.length