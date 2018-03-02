namespace Rosdex.Parser.Csv

[<RequireQualifiedAccess>]
type CsvCell =
    | String of string
    | Int of int
    | Float of float

type 'a CsvField = {
    Name : string
    Mapping : 'a -> CsvCell option
}

type CsvField =
    static member private createOpt toFieldType mapping name =
        {   Name = name
            Mapping = mapping >> Option.map toFieldType }
    static member private createTrue toFieldType mapping name =
        {   Name = name
            Mapping = mapping >> toFieldType >> Some }

    static member create mapping =
        CsvField.createOpt CsvCell.String mapping
    static member create mapping =
        CsvField.createOpt CsvCell.Int mapping
    static member create mapping =
        CsvField.createOpt CsvCell.Float mapping

    static member create mapping =
        CsvField.createTrue CsvCell.String mapping
    static member create mapping =
        CsvField.createTrue CsvCell.Int mapping
    static member create mapping =
        CsvField.createTrue CsvCell.Float mapping

module CsvField =
    let map mapping field =
        { field with Mapping = field.Mapping >> Option.map mapping }

    let bind binder field =
        { field with Mapping = field.Mapping >> Option.bind binder }

    let mapWithNone mapping field =
        { field with Mapping = field.Mapping >> mapping }

type CsvConfig = {
    Separator : string
    AllowQuote : string option
    Write : CsvCell -> string
}

module CsvConfig =
    let default' = {
        Separator = ","
        AllowQuote = Some "\""
        Write = function
            | CsvCell.Int p -> string p
            | CsvCell.Float p -> p.ToString(System.Globalization.CultureInfo.InvariantCulture)
            | CsvCell.String p -> p
    }

    let tryEscape config (str : string) =
        // TODO: Экранирование переводов строк.
        match
            str.Contains config.Separator
            || (config.AllowQuote |> Option.exists str.Contains)
            || str.Contains "\n"
            , config.AllowQuote
            with
        | true, Some quote ->
            let doubleQuote = quote + quote
            sprintf "%s%s%s"
                quote
                (String.replace quote doubleQuote str)
                quote
            |> Some
        | true, None -> None
        | false, _ -> Some str

type 'a CsvWriter = {
    Fields : 'a CsvField list
    Config : CsvConfig
}

module CsvWriter =
    let tryStringifyField config field item =
        match item |> field.Mapping with
        | Some p -> config.Write p
        | None -> ""
        |> CsvConfig.tryEscape config

    let tryStringifyValue writer item =
        writer.Fields
        |> List.map (fun field ->
            tryStringifyField writer.Config field item)
        |> List.partition Option.isSome
        |> function
            | p, [] ->
                p
                |> List.map Option.get
                |> String.concat writer.Config.Separator
                |> Some
            | _ -> None

module CsvWriters =
    open Rosdex.Parser.Yandex.Yml
    open Rosdex.Parser.Collections

    module ExtendedOffers =
        let fullName offer =
            [   offer.TypePrefix
                offer.Vendor
                offer.Name
                offer.Model ]
            |> List.choose id
            |> String.concat " "

        let params' offer =
            offer.Params
            |> Seq.map (fun p ->
                match p.Value with
                    | UnitValue (unitName, value) -> sprintf "%s | %s | %f" p.Name unitName value
                    | StringValue value -> sprintf "%s | %s" p.Name value)
            |> String.concat "; "

        let categoryPath catalog offer =
            Catalog.tryPath catalog offer.LocalCategoryId
            |> Catalog.Path.tryPath
            |> Option.defaultValue []
            |> List.map Category.name
            |> String.concat " | "

        let ofCatalog catalog = {
            Config = { CsvConfig.default' with AllowQuote = None }
            Fields =
                [   "LocalId" |> CsvField.create (fun p -> p.LocalId)
                    "FullName" |> CsvField.create fullName
                    "TypePrefix" |> CsvField.create (fun p -> p.TypePrefix)
                    "Vendor" |> CsvField.create (fun p -> p.Vendor)
                    "Name" |> CsvField.create (fun p -> p.Name)
                    "Model" |> CsvField.create (fun p -> p.Model)
                    "Params" |> CsvField.create params'
                    "Description" |> CsvField.create (fun p -> p.Description)
                    "PictureUrl" |> CsvField.create (fun p -> p.PictureUrl)
                    "LocalCategoryId" |> CsvField.create (fun p -> p.LocalCategoryId)
                    "LocalCategoryPath" |> CsvField.create (categoryPath catalog) ]
                |> List.map (
                    CsvField.mapWithNone (
                        let empty = " "
                        function
                        | Some (CsvCell.String p) ->
                            p.Replace(',', '.')
                            |> String.filter (fun p ->
                                p <> '\r' && p <> '\n')
                            |> String.trim
                            |> function
                                | "" -> empty
                                | p -> p
                            |> CsvCell.String
                            |> Some
                        | None -> CsvCell.String empty |> Some
                        | p -> p))
        }