namespace Rosdex.Parser.Yandex.Yml.Tests

open Rosdex.Parser.Collections
open Rosdex.Parser.Utils
open Rosdex.Parser.Yandex.Yml

open Utils

open Expecto
open FsCheck
open FsUnit.Expecto

module Tests =
    open System.Xml.Linq

    type 'a XmlCase = {
        Xml : XElement
        Subject : 'a
    }

    // TODO: Получить контроль над выбором между attribute или element.
    // | TODO: Добавить прямо в оригинальные DTO.
    module JsonXml =
        open Microsoft.FSharpLu.Json
        open Newtonsoft.Json

        type CamelCaseSettings =
            static member settings =
                let s =
                    JsonSerializerSettings(
                        NullValueHandling = NullValueHandling.Ignore,
                        MissingMemberHandling = MissingMemberHandling.Error,
                        ContractResolver = new Serialization.CamelCasePropertyNamesContractResolver())
                s.Converters.Add(CompactUnionJsonConverter())
                s
            static member formatting = Formatting.None

        type CamelSerializer = With<CamelCaseSettings>

        let buildXml rootName subject =
            CamelSerializer.serialize subject
            |> fun p -> JsonConvert.DeserializeXNode(p, rootName).Root

        let case rootName subject ={
            Xml = buildXml rootName subject
            Subject = subject
        }

    module Arb =
        type Generators =
            static member validCategoryCaseArb =
                gen {
                    let! (PositiveInt id) = Arb.generate
                    let! (NonEmptyString name) = Arb.generate
                    let! parentId =
                        Arb.generate
                        |> Gen.map (fun (PositiveInt p) -> p)
                        |> Gen.optionOf
                    let subject =
                        {
                            Category.Id = id
                            Name = name
                            ParentId = parentId
                        }
                    return {
                        Xml =
                            XElement(
                                "category",
                                [
                                    yield upcast XAttribute("id", string id)
                                    yield!
                                        parentId
                                        |> Option.map (fun p ->
                                            XAttribute("parentId", string p) |> box)
                                        |> Option.toList
                                    yield upcast name])
                        Subject =
                            subject
                    }
                }
                |> Arb.fromGen

        let config =
            FsCheckConfig.add<Generators> FsCheckConfig.defaultConfig

    [<Tests>]
    let categoryTests =
        testList "Yml/Category" [
            testPropertyWithConfig Arb.config "ofXml" <| fun case ->
                case.Xml
                |> Category.ofXml
                |> shouldEqual case.Subject ""
        ]