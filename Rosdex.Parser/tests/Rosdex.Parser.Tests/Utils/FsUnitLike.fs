namespace FsUnit.Expecto

open Expecto

[<AutoOpen>]
module FsUnitLikeSyntax =
    let private inv2 assertion expected message actual =
        assertion actual expected message

    let private inv1 assertion message actual =
        assertion actual message

    let shouldEqual expected = inv2 Expect.equal expected

    let shouldTrue message actual = Expect.isTrue actual message
    let shouldFalse = inv1 Expect.isFalse

    let shouldAll predicate message items =
        Expect.all items predicate message

    let shouldContainsAll expected message items =
        Expect.containsAll items expected message

    let shouldSequenceEqual expected message actual =
        Expect.sequenceEqual actual expected message

    let shouldContainsString substring message subject =
        Expect.stringContains subject substring message

    let shouldHaveLength expected message items =
        Expect.hasCountOf items expected message