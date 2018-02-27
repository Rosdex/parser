namespace Rosdex.Parser.Collections.Tests

open Rosdex.Parser.Collections
open Rosdex.Parser.Utils

open Utils

open Expecto
open FsCheck
open FsUnit.Expecto

module CatalogTests =
    let rand = System.Random()

    type ValueStub =
        {
            Id : int
            ParentId : int option
        }

        static member id value = value.Id
        static member parnetId value = value.ParentId

        static member create id parentId = { Id = id; ParentId = parentId }
        static member catalog = {
            Storage = Map.empty
            GetId = ValueStub.id
            GetParentId = ValueStub.parnetId
        }

    module Arb =
        type DivisorCatalogCase = {
            Basis : int
            Count : int
            Target : int
        }

        let divisorCatalogCaseArb =
            gen {
                let! basis = Gen.choose (2, 8)
                let! count = Gen.choose (1, pown basis 4)
                let! target = Gen.choose (1, count)
                return {
                    Basis = basis
                    Count = count
                    Target = target
                }
            }
            |> Arb.fromGen

        let config =
            FsCheckConfig.defaultConfig
            |> FsCheckConfig.addDeclaring<DivisorCatalogCase>

    module DivisorCatalog =
        let ofNumbers basis numbers =
            { ValueStub.catalog with
                Storage =
                    if numbers |> List.exists (fun p -> p <= 0) then
                        failwith "Numbers must be positive!"
                    numbers
                    |> List.map (fun p ->
                        let id = p
                        let parentId = if p < basis then None else p / basis |> Some
                        id, ValueStub.create id parentId)
                    |> Map.ofSeq
                }

        let ofCount basis count=
            [1..count] |> ofNumbers basis

        let children basis totalCount root =
            [root]
            |> List.unreduce (
                List.collect (fun p -> List.init basis (fun i -> p * basis + i))
                >> List.filter ((>=) totalCount)
                >> function
                    | [] -> None
                    | p -> Some p)
            |> List.concat

        open Arb

        let ofCase case = ofCount case.Basis case.Count
        let childrenInCase config =
            children config.Basis config.Count config.Target

    open Arb

    [<Tests>]
    let tests =
        testList "Catalog" [
            testList "tryPath" [
                let catalogWithRemovedNumber case =
                    DivisorCatalog.ofCase case
                    |> Catalog.removeSingle case.Target
                let tryPathes case =
                    List.map (
                        let catalog = catalogWithRemovedNumber case
                        fun id -> id, Catalog.tryPath catalog id)

                let none case =
                    [case.Target]
                    |> tryPathes case
                let incomplete case =
                    DivisorCatalog.childrenInCase case
                    |> tryPathes case
                let complete case =
                    [1..case.Count]
                    |> List.except (DivisorCatalog.childrenInCase case)
                    |> List.except [case.Target]
                    |> tryPathes case
                let all case =
                    [1..case.Count]
                    |> tryPathes case

                yield testPropertyWithConfig Arb.config "deleted has no path" <| fun case ->
                    none case
                    |> List.exactlyOne
                    |> snd
                    |> shouldEqual (Catalog.Path.None case.Target) ""
                // TODO: F# 4.2
                yield testPropertyWithConfig Arb.config "children have incomplete path" <| fun case ->
                    incomplete case
                    |> shouldAll (function
                        | _, Catalog.Path.Incomplete _ -> true
                        | _ -> false) ""
                yield testPropertyWithConfig Arb.config "other have complete path" <| fun case ->
                    complete case
                    |> shouldAll (function
                        | _, Catalog.Path.Complete _ -> true
                        | _ -> false) ""
                yield testPropertyWithConfig Arb.config "next.ParentId = previous.Id" <| fun case ->
                    [complete; incomplete]
                    |> List.collect (fun f -> f case)
                    |> shouldAll (
                        snd
                        >> Catalog.Path.tryPath
                        >> Option.exists (
                            List.pairwise
                            >> List.forall (fun (previous, next) ->
                                next.ParentId = Some previous.Id))) ""
                yield testPropertyWithConfig Arb.config "notFound.Id = deleted.Id" <| fun case ->
                    [incomplete; none]
                    |> List.collect (fun f -> f case)
                    |> shouldAll (
                        snd
                        >> Catalog.Path.tryNotFoundId
                        >> Option.exists (fun id ->
                            id = case.Target)) ""
                yield testPropertyWithConfig Arb.config "final.Id = id" <| fun case ->
                    all case
                    |> shouldAll (fun (id, path) ->
                        match path with
                        | Catalog.Path.Complete path
                        | Catalog.Path.Incomplete (path, _)
                            -> path |> List.last |> fun p -> p.Id = id
                        | Catalog.Path.None finalId -> finalId = id) ""
                yield testPropertyWithConfig Arb.config "complete.Last.ParentId = None" <| fun case ->
                    complete case
                    |> shouldAll (fun (_, path) ->
                        match path with
                        | Catalog.Path.Complete path -> path.Head.ParentId = None
                        | _ -> false) ""
            ]
            testPropertyWithConfig Arb.config "removeCascade" <| fun case ->
                DivisorCatalog.ofCase case
                |> Catalog.removeCascade case.Target
                |> Catalog.storage
                |> shouldEqual (
                    [1..case.Count]
                    |> List.except (DivisorCatalog.childrenInCase case)
                    |> List.except [case.Target]
                    |> DivisorCatalog.ofNumbers case.Basis
                    |> Catalog.storage) ""
            testList "validate" [
                testPropertyWithConfig Arb.config "valid" <| fun case ->
                    DivisorCatalog.ofCase case
                    |> Catalog.validate
                    |> shouldEqual Catalog.Validation.Valid ""
                testPropertyWithConfig Arb.config "has cycle" <| fun case ->
                    DivisorCatalog.ofCase case
                    |> match DivisorCatalog.childrenInCase case with
                        | [] ->
                            Catalog.add { Id = 0; ParentId = Some case.Target }
                            >> Catalog.add { Id = case.Target; ParentId = Some 0 }
                        | children ->
                            let index = children.Length |> rand.Next
                            Catalog.add {
                                Id = case.Target;
                                ParentId = Some children.[index] }
                    |> Catalog.validate
                    |> shouldEqual Catalog.Validation.HasCycle ""
                    //|> function
                    //    | Catalog.Validation.HasCycle _ -> ()
                    //    | p -> failtestf "Catalog has no cycle: %A" p
                testPropertyWithConfig Arb.config "disconnected" <| fun case ->
                    DivisorCatalog.ofCase case
                    |> Catalog.removeSingle case.Target
                    |> Catalog.add { Id = 0; ParentId = Some case.Target }
                    |> Catalog.validate
                    |> shouldEqual Catalog.Validation.Disconected ""
            ]
        ]