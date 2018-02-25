namespace Rosdex.Parser.Tests

open Expecto

module SampleTests =
    open FsCheck

    [<Tests>]
    let adapterTests =
        testList "i check visual studio adapter" [
            testList "simple tests" [
                testCase "valid" <| fun _ ->
                    Expect.equal (2 * 2) 4 ""
                ptestCase "invalid" <| fun _ ->
                    Expect.equal (2 * 2) 5 ""
            ]
        ]

    [<Tests>]
    let referencesTests =
        testList "reference checking" [
            testCase "greeting" <| fun _ ->
                Rosdex.Parser.Say.greeting "klei"
                    |> Expect.equal <| "Hello klei" <| ""
        ]

    [<Tests>]
    let propertySampleTests =
        testList "try remember these terms" [
            testProperty "different paths, same destination" <| fun items last ->
                last::(items |> List.rev) |> List.rev = ([last] |> List.append items)
            testProperty "comutativity - order doesn't matter" <| fun pairs ->
                let a, b = pairs |> List.unzip
                List.map2 (+) a b = List.map2 (+) b a
            testProperty "associativity - any grouping" <| fun trios ->
                let a, b, c = List.unzip3 trios
                List.map2 (*) a b |> List.map2 (*) c = (List.map2 (*) b c |> List.map2 (*) a)
            testProperty "roundtripping - there and back again" <| fun (NonEmptyArray (items : int[])) ->
                items
                |> Microsoft.FSharpLu.Json.Compact.serialize
                |> Microsoft.FSharpLu.Json.Compact.deserialize<int[]>
                |> (=) items
            testProperty "involution - self-inverse" <| fun a ->
                a = -(-a)
            testProperty "idempotence - the more things change, the more they stay the same | once is enough" <| fun a ->
                a |> List.distinct = (a |> List.distinct |> List.distinct)
            testProperty "invariants - some things never change" <| fun (NonNull (word : string))->
                word.Length = word.ToUpper().Length
            testProperty "hard to prove, easy to verify" <| fun (items : int list)->
                items |> List.sortDescending |> List.pairwise |> List.forall (fun (a, b) -> a >= b)
            testProperty "oracle - use trusted implementation that give correct result" <| fun items ->
                let foldBaseFilter predicate items =
                    items
                    |> List.fold (fun state item ->
                        if predicate item then item :: state else state) []
                    |> List.rev
                foldBaseFilter (fun p -> p > 0) items = List.filter (fun p -> p > 0) items
        ]