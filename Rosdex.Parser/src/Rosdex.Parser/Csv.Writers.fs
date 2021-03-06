﻿module Rosdex.Parser.Csv.Writers
open Operators

module ExtendedOffers =
    open Rosdex.Parser.Yandex.Yml
    open Rosdex.Parser.Collections

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
            | UnitValue (unitName, value) ->
                sprintf "%s | %s | %f" p.Name unitName value
            | StringValue value ->
                sprintf "%s | %s" p.Name value)
        |> String.concat "; "

    let categoryPath catalog offer =
        Catalog.tryPath catalog offer.LocalCategoryId
        |> Catalog.Path.tryPath
        |> Option.defaultValue []
        |> List.map Category.name
        |> String.concat " | "

    let canonicalize cell =
        let stub = " "
        let replaceComas = String.replace "," "."
        let removeEndLines =
            String.filter (fun p ->
                p <> '\r' && p <> '\n')
        let replaceEmptyString = function
            | "" -> stub
            | p -> p
        match cell with
        | Some (CsvCell.String p) ->
            p
            |> replaceComas
            |> removeEndLines
            |> String.trim
            |> replaceEmptyString
            |> CsvCell.String
            |> Some
        | None -> CsvCell.String stub |> Some
        | p -> p

    let ofCatalog catalog = {
        Config = { CsvConfig.default' with AllowQuote = None }
        Fields =
            [   (fun p -> p.LocalId) => "LocalId"
                fullName => "FullName"
                (fun p -> p.TypePrefix) => "TypePrefix"
                (fun p -> p.Vendor) => "Vendor"
                (fun p -> p.Name) => "Name"
                (fun p -> p.Model) => "Model"
                params' => "Params"
                (fun p -> p.Description) => "Description"
                (fun p -> p.PictureUrl) => "PictureUrl"
                (fun p -> p.LocalCategoryId) => "LocalCategoryId"
                categoryPath catalog => "LocalCategoryPath" ]
            |> List.map (CsvField.mapOption canonicalize)
    }

open Rosdex.Parser.Collections

let private typePrefixedName catalog (item : Rosdex.Parser.Dump.Product)=
    Catalog.find catalog item.CategoryId
    |> fun (category : Rosdex.Parser.Dump.Category) ->
        sprintf "%s %s %s"
            item.Data.Vendor
            item.Data.Name
            category.Name

let dumpWriter catalog : Rosdex.Parser.Dump.Product CsvWriter =
    CsvWriter.ofFields [
        (fun p -> p.Id) => "ProductId"
        (fun p -> p.Data.Vendor + " " + p.Data.Name ) => "ProductName"
        typePrefixedName catalog => "ProductNameWithTypePrefix"
        (fun p -> p.Data.Image) => "ImageLink"
        (fun p -> p.Data.Description) => "Description"
        (fun p -> p.Data.OriginalUrl) => "OriginalUrl"
        (fun p -> p.CategoryId) => "CategoryId"
    ]