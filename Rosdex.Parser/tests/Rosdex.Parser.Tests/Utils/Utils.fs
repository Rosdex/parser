module Utils

open Expecto
open FsCheck

module FsCheckConfig =
    let addType config type' =
        { config with
            arbitrary =
                type' :: config.arbitrary }

    let add<'a> config =
        typeof<'a> |> addType config

    let addDeclaring<'a> config =
        typeof<'a>.DeclaringType |> addType config