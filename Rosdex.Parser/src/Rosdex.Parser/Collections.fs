namespace Rosdex.Parser.Collections

open Rosdex.Parser.Utils

type ('id, 'value) Catalog when 'id : comparison = {
    Storage : Map<'id, 'value>
    GetId : 'value -> 'id
    GetParentId : 'value -> 'id option
}

module Catalog =
    open System.Collections.Generic

    let storage catalog = catalog.Storage
    let getId catalog = catalog.GetId
    let getParentId catalog = catalog.GetParentId

    let create getId getParentId values = {
        Storage = values |> Seq.map (fun p -> getId p, p) |> Map.ofSeq
        GetId = getId
        GetParentId = getParentId
    }

    let count catalog = catalog.Storage.Count

    let tryFind catalog id =
        catalog.Storage.TryFind id

    let find catalog id =
        tryFind catalog id
        |> Option.defaultWith (fun () ->
            sprintf """Id %A not found""" id
            |> KeyNotFoundException
            |> raise)

    let contains catalog value =
        value
        |> catalog.GetId
        |> tryFind catalog
        |> Option.exists ((=) value)

    let valuesByParentId catalog =
        catalog.Storage
        |> Map.toList
        |> List.map snd
        |> List.groupBy catalog.GetParentId
        |> Map.ofList

    [<RequireQualifiedAccess>]
    type 'value Validation =
        | Valid
        | Disconected
        /// Подмножество Disconnected, но приводит к более серьезным проблемам.
        | HasCycle //of NodeInCycle : 'value

    let validate catalog=
        let rec impl validationMaximum generation passed remains =
            match generation with
            | [] ->
                match Map.isEmpty remains with
                | true -> validationMaximum
                | false ->
                    impl
                        Validation.Disconected
                        [Map.toSeq remains |> Seq.head |> fst]
                        Set.empty
                        remains
            | generation ->
                match generation |> List.exists passed.Contains with
                | true -> Validation.HasCycle
                | false ->
                    let newGeneration =
                        generation
                        |> List.choose remains.TryFind
                        |> List.concat
                        |> List.map Some
                    let newPassed =
                        passed
                        |> List.fold (fun set -> set.Add) <| generation
                    remains
                    |> List.fold (fun p -> p.Remove) <| generation
                    |> impl validationMaximum newGeneration newPassed
        catalog
        |> valuesByParentId
        |> Map.map (fun _ p -> List.map catalog.GetId p)
        |> impl Validation.Valid [None] Set.empty

    [<RequireQualifiedAccess>]
    type ('id, 'value) Path =
        | Complete of Path : 'value list
        | Incomplete of Path : 'value list * NotFoundId : 'id
        | None of NotFoundId : 'id
        // TODO? Cycle

    module Path =
        let tryNotFoundId path =
            match path with
            | Path.None id
            | Path.Incomplete (_, id) -> Some id
            | _ -> None

        let tryPath path =
            match path with
            | Path.Complete tokens
            | Path.Incomplete (tokens, _) -> tokens |> Some
            | _ -> None

    let tryPath catalog id =
        let rec impl current passed =
            let newPassed = current::passed
            match catalog.GetParentId current with
            | None ->
                Path.Complete newPassed
            | Some parentId ->
                tryFind catalog parentId
                |> function
                    | None -> Path.Incomplete (newPassed, parentId)
                    | Some p -> impl p newPassed
        match tryFind catalog id with
        | None -> Path.None id
        | Some p -> impl p []

    let path catalog id =
        match tryPath catalog id with
        | Path.Complete path ->
            path
        | Path.Incomplete (_, notFound)
        | Path.None notFound ->
            sprintf "Id %A not found." notFound
            |> KeyNotFoundException
            |> raise

    let mapStorage mapping catalog =
        { catalog with
            Storage = catalog.Storage |> mapping }

    let add value catalog =
        catalog
        |> mapStorage (Map.add (catalog.GetId value) value)

    let removeSingle id catalog =
        catalog |> mapStorage (Map.remove id)

    let removeCascadeMany ids catalog =
        let valuesByParentId = valuesByParentId catalog
        ids
        |> List.unreduce (
            List.choose (Some >> valuesByParentId.TryFind)
            >> List.concat
            >> List.map catalog.GetId
            >> function
                | [] -> None
                | p -> Some p)
        |> List.concat
        |> List.append <| ids
        |> List.fold (fun state id -> Map.remove id state) catalog.Storage
        |> fun storage -> { catalog with Storage = storage }

    let removeCascade id catalog =
        removeCascadeMany [id] catalog