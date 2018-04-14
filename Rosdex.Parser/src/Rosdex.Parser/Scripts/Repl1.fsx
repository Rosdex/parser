#load "Utils.Repl.fsx"

open Rosdex.Parser.Dump
open Rosdex.Parser.Collections

let realDbPath =
    Utils.Repl.gitignored.DumpDbFilePath.Value

let realConnectionString =
    DB.Literals.connectionString realDbPath

let context = DB.DumpProvider.GetDataContext(realConnectionString)

context.Main.Category
    |> Seq.map (fun p -> p.IdCategory)
    |> Seq.length

let categories =
    context.Main.Category
    |> Catalog.create
        (fun p -> p.IdCategory)
        (fun p -> p.IdParentCategory)

categories
    |> Catalog.validate

context.Main.Category
    |> Seq.forall (fun p -> p.IdCategory |> categories.Storage.ContainsKey)

categories.Storage
    |> Map.toSeq
    |> Seq.map
        (snd >> fun p -> p.CategoryDataString)

///

let categories' =
    context.Main.Category
    |> Seq.map Category.convert
    |> Catalog.create (fun p -> p.Id) (fun p -> p.ParentId)

let products' =
    context.Main.Product
    |> Seq.map Product.convert
    |> List.ofSeq

let takeParentsOfEndLeafs catalog =
    catalog
    |> Catalog.toSeq
    |> Seq.filter (
        let map = Catalog.valuesByParentId catalog
        catalog.GetId
        >> Some
        >> map.TryFind
        >> Option.isNone)
    |> Seq.choose catalog.GetParentId
    |> Seq.distinct
    |> Seq.map (Catalog.find catalog)
    |> List.ofSeq

let currentCategories' =
    takeParentsOfEndLeafs categories'

categories' |> Catalog.validate

let regroupProduct needRegroup products =
    products
    |> List.map (fun p ->
        p.CategoryId
        |> Catalog.path categories'
        |> List.find needRegroup
        |> fun current -> { p with CategoryId = current.Id })

let regrouppedProducts =
    products'
    |> regroupProduct (fun p -> List.contains p currentCategories')

regrouppedProducts
    |> List.map (fun p -> p.CategoryId)
    |> List.distinct

products'
    |> List.choose (fun p ->
        let category = Catalog.find categories' p.CategoryId
        if p.Data.Vendor = category.Name then
            None
        else
            Some (p, category))

open Rosdex.Parser.Csv
open Microsoft.FSharpLu
open Utils

[
    yield
        Writers.dumpWriter categories'
        |> CsvWriter.stringifyHeaders
    yield!
        regrouppedProducts
        |> Seq.map (Writers.dumpWriter categories' |> CsvWriter.stringifyItem)
]
|> File.writeLines
    (Repl.gitignored.OutputDirectoryPath
        |> Option.get
        |> Repl.IO.Path.combine <| "dump.csv")

///

let mains =
    query {
        for category in context.Main.Category do
        where (category.IdParentCategory = Some "root")
        select category
    }
    |> List.ofSeq

query {
    for category in context.Main.Category do
    where (category.IdParentCategory = Some "root")
    count
}

context.Main.Category |> Seq.map (fun p -> p.Name, p.IdCategory) |> List.ofSeq