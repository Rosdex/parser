namespace Rosdex.Parser.Dump

module String =
    let ofUTF8 bytes =
        bytes
        |> System.Text.Encoding.UTF8.GetString

module DB =
    module Literals =
        [<Literal>]
        let dbPath =
            __SOURCE_DIRECTORY__ + """..\Resources\dump-template.db"""

        [<Literal>]
        let templateConnectionString =
            "Data Source=" + dbPath + ";"
            + "Version=3;"

        let connectionString dataSourcePath =
            "Data Source=" + dataSourcePath + ";"
            + "Version=3;"

        // Следует перекинуть SQLite.Interop.dll
        // для соответсвующей платформы из build в lib
        // в System.Data.SQLite.Core .
        // // В fsi нужен x86.
        [<Literal>]
        let libPath =
            __SOURCE_DIRECTORY__ + """..\..\..\packages\System.Data.SQLite.Core\lib\net20"""

    open FSharp.Data.Sql

    type DumpProvider =
        SqlDataProvider<
            DatabaseVendor = Common.DatabaseProviderTypes.SQLITE
            , ConnectionString = Literals.templateConnectionString
            , ResolutionPath = Literals.libPath
            , IndividualsAmount = 1000
            , UseOptionTypes = true
            , SQLiteLibrary = Common.SQLiteLibrary.SystemDataSQLite
            >

[<AutoOpen>]
module DbExtensions =
    open DB

    type DumpProvider.dataContext.``main.categoryEntity`` with
        /// Декодированный blob.
        member this.CategoryDataString =
            this.Categorydata |> String.ofUTF8

        member this.CategoryDataJson =
            this.CategoryDataString
            |> FSharp.Data.JsonValue.Parse

    type DumpProvider.dataContext.``main.productEntity`` with
        /// Декодированный blob.
        member this.ProductDataString =
            this.Productdata |> String.ofUTF8

        member this.ProductDataJson =
            this.ProductDataString
            |> FSharp.Data.JsonValue.Parse

open FSharp.Data.JsonExtensions
open FSharp.Data

type CategoryId =
    string

    // TODO: DU если структура в полном дампе окажется такой же.
    //| Root
    //| Leaf of int

type Category = {
    Id : CategoryId
    ParentId : CategoryId option
    Name : string
    Level : int
}

module Category =
    module DataJson =
        let level (json : JsonValue) =
            json?Attributes?SYS_CATEGORY_LEVEL.AsString() |> int

    let convert (entity : DB.DumpProvider.dataContext.``main.categoryEntity``) = {
        Id = entity.IdCategory
        ParentId = entity.IdParentCategory
        Name = entity.Name
        Level = entity.CategoryDataJson |> DataJson.level
    }

type Param = {
    Name : string
    Value : string
}

type ProductData = {
    Name : string
    Description : string
    OriginalUrl : string
    Vendor : string
    Params : Param list
    Image : string option
}

type Product = {
    Id : int
    CategoryId : CategoryId
    Data : ProductData
}

module Product =
    module ProductDataJson =
        module Json =
            let tryProperty property (json : JsonValue) =
                json.TryGetProperty property

            let property property (json : JsonValue) =
                json.GetProperty property

        type Ext = JsonExtensions

        let private trimmedStringProperty property json =
            json
            |> Json.property property
            |> Ext.AsString
            |> String.trim

        /// Дабы не применять геттеры к чему попало.
        type [<Struct>] Attributes = Attributes of JsonValue

        module Attributes =
            let get json =
                json
                |> Json.property "Attributes"
                |> Attributes

            let value (Attributes json) = json

        let tryParam index (Attributes attributes) =
            index
            |> sprintf "SYS_COMMAND_CREATE_COLUMN%i"
            |> attributes.TryGetProperty
            |> Option.map (fun p ->
                p.AsString()
                    .Split([|"[--->]"|], System.StringSplitOptions.RemoveEmptyEntries)
                |> function
                    | [|title; property|] ->
                        attributes
                        |> trimmedStringProperty property
                        |> fun p ->
                            {
                                Name = title
                                Value = p
                            }
                    | _ ->
                        p.AsString()
                        |> failwithf "Wrong format: %s")

        let params' attributes =
            1
            |> List.unfold (fun index ->
                attributes
                |> tryParam index
                |> Option.map (fun p -> p, index + 1))

        let tryImageUrl =
            Attributes.value
            >> Json.tryProperty "SYS_PRODUCT_IMAGE_FULL"
            >> Option.map (Ext.AsString >> String.trim)

        let name
            , originalUrl
            , description
            , vendor =
            let strFromAttrs name =
                Attributes.value
                >> trimmedStringProperty name
            strFromAttrs "SYS_PRODUCT_NAME"
            , strFromAttrs "SYS_PRODUCT_ORIG_URL"
            , strFromAttrs "SYS_PRODUCT_DESC"
            , strFromAttrs "SYS_PRODUCT_MANUFACTURER"

        let convert json =
            json
            |> Attributes.get
            |> fun p ->
                {
                    ProductData.Name = name p
                    Description = description p
                    OriginalUrl = originalUrl p
                    Vendor = vendor p
                    Params = params' p
                    Image = tryImageUrl p
                }

    let convert (product : DB.DumpProvider.dataContext.``main.productEntity``) =
        {
            Product.Id = product.IdProduct |> int
            CategoryId = product.IdCategory
            Data =
                product.ProductDataJson
                |> ProductDataJson.convert
        }