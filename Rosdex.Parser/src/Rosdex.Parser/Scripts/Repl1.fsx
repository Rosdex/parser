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

regrouppedProducts
    |> List.collect (fun p -> p.Data.Params)
    |> List.map (fun p -> p.Name)
    |> List.distinct
    |> List.length

open Rosdex.Parser.Csv
open Utils
open Microsoft.FSharpLu

type Counts = {
    Value : string
    Count : int
}

let values =
    regrouppedProducts
    |> List.collect (fun p -> p.Data.Params |> List.map (fun q -> q, p.CategoryId))
    |> List.countBy id
    |> List.map (fun ((param, catId), count) ->
        catId, (param, count))
    |> List.groupBy fst
    |> List.map (fun (cId, p) ->
        cId |> Catalog.find categories' |> fun p -> p.Name
        , p
            |> List.map (fun (_, (param, count)) -> param.Name, { Value = param.Value; Count = count})
            |> List.groupBy fst
            |> List.map (fun (name, p) -> name, List.map snd p)
            |> Map.ofList
        )
    |> Map.ofList

values
    |> Map.toList
    |> List.map (fun (catId, byNames) -> catId, byNames |> Map.toList)
    |> List.collect (
        let tab = sprintf "    %s"
        let itemTab = sprintf "  - %s"
        fun (name, names) ->
            [
                yield name
                yield!
                    names
                    |> List.collect (fun (name, counts) ->
                        [
                            yield itemTab name
                            yield!
                                counts
                                |> List.collect(fun p ->
                                    [   p.Value.Replace("\n", "") |> itemTab
                                        sprintf "%-3i шт" p.Count |> tab
                                    ])
                                |> List.map tab
                        ])
            ])
    |> String.concat "\n"
    |> fun p ->
        System.IO.File.WriteAllText(
            (Repl.gitignored.OutputDirectoryPath
                |> Option.get
                |> Repl.IO.Path.combine <| "values.tabyaml.txt")
            , p)

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