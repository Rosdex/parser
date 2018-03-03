namespace Rosdex.Parser.Utils

[<AutoOpen>]
module AutoOpened =
    let inline (!>) (x:^a) : ^b =
        ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)

module List =
    let unreduce generator value =
        let rec impl value acc =
            match generator value with
            | None -> value::acc
            | Some p -> value::acc |> impl p
        impl value []
        |> List.rev
        |> List.tail

module Result =
    let isOk = function Ok _ -> true | _ -> false
    let isError = function Error _ -> true | _ -> false

    let tryOk = function Ok p -> Some p | _ -> None
    let tryError = function Error p -> Some p | _ -> None

    let ok = function
        | Ok p -> p
        | p -> p |> sprintf "it is %A" |> invalidOp
    let error = function
        | Error p -> p
        | p -> p |> sprintf "it is %A" |> invalidOp