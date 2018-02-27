namespace Rosdex.Parser.Utils

module List =
    let unreduce generator value =
        let rec impl value acc =
            match generator value with
            | None -> value::acc
            | Some p -> value::acc |> impl p
        impl value []
        |> List.rev
        |> List.tail