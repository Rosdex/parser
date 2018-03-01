namespace Rosdex.Parser.Yandex.Yml

module X =
    exception HasNoElement of name : string
    exception HasNoAttribute of name : string

    open System.Xml.Linq

    let name = XName.Get

    let tryElement key (xe : XContainer) =
        xe.Element (name key) |> Option.ofObj

    let element key xe =
        match tryElement key xe with
        | Some p -> p
        | None -> key |> HasNoElement |> raise

    let allElements (xe : XContainer) = xe.Elements()

    let elements key (xe : XContainer) =
        key |> name |> xe.Elements

    let value (xe : XElement) = xe.Value

    let tryAttribute key (xe : XElement) =
        key |> name |> xe.Attribute |> Option.ofObj

    let tryAttributeValue key (xe : XElement) =
        key
        |> name
        |> xe.Attribute
        |> Option.ofObj
        |> Option.map (fun p -> p.Value)

    let attribute key (xe : XElement) =
        tryAttribute key xe
        |> Option.defaultWith (fun () -> HasNoAttribute key |> raise)

    let attributeValue key (xe : XElement) =
        attribute key xe
        |> fun p -> p.Value

    let attributes (xe : XElement) = xe.Attributes()

type Category = {
    Id : int
    Name : string
    ParentId : int option
}

module Category =
    let name { Name = name } = name
    let id { Category.Id = id } = id
    let parentId { Category.ParentId = parentId } = parentId

    let ofXml x = {
        // Warning!
        Id = x |> X.attributeValue "id" |> int
        Name = x |> X.value
        // Warning!
        ParentId = x |> X.tryAttributeValue "parentId" |> Option.map int
    }

    open Rosdex.Parser.Collections

    let catalog categories =
        Catalog.create id parentId categories

type ParamValue =
    | UnitValue of unitName : string * value : float
    | StringValue of string

type Param = {
    Name : string
    Value : ParamValue
}

type Offer = {
    LocalId : int
    Name : string option
    TypePrefix : string option
    Model : string option
    Vendor : string option
    VendorCode : string option
    LocalCategoryId : int
    GlobalCategoryId : int option
    PictureUrl : string option
    Description : string option
    Params : Param list
}

module Param =
    let ofXml x =
        {
            // Warning!
            Name = x |> X.attributeValue "name"
            Value =
                let value = x |> X.value
                x
                |> X.tryAttributeValue "unit"
                // Warning!
                |> Option.map (fun p -> UnitValue(p, float value))
                |> Option.defaultValue (StringValue value)
        }

module Offer =
    let fromXml x =
        let tryValue key =
            x |> X.tryElement key |> Option.map X.value
        {
            // Warning!
            LocalId = x |> X.attributeValue "id" |> int
            Name = tryValue "name"
            TypePrefix = tryValue "typePrefix"
            Model = tryValue "model"
            Vendor =  tryValue "vendor"
            VendorCode = tryValue "vendorCode"
            LocalCategoryId =
                x
            // Warning!
                |> X.element "categoryId"
                |> X.value
            // Warning!
                |> int
            GlobalCategoryId = None
            PictureUrl = tryValue "picture"
            Description = tryValue "description"
            Params =
                x
                |> X.elements "param"
            // Warning!
                |> Seq.map Param.ofXml
                |> List.ofSeq
        }

module YmlParser =
    let extractCategories xdoc =
        xdoc
        // Warning!
        |> X.element "yml_catalog"
        |> X.element "shop"
        |> X.element "categories"
        |> X.allElements
        // Warning!
        |> Seq.map Category.ofXml
        |> List.ofSeq

    // TODO: По канону.
    let tryExtractOffers xdoc =
        xdoc
        // Warning!
        |> X.element "yml_catalog"
        |> X.element "shop"
        |> X.element "offers"
        |> X.allElements
        |> Seq.map (fun p ->
            try p |> Offer.fromXml |> Ok
            with
            | X.HasNoElement name
            | X.HasNoAttribute name -> (p.Value, name) |> Error
            | ex -> (p.Value, ex.Message) |> Error)
        |> List.ofSeq