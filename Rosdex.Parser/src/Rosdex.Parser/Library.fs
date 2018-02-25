namespace Rosdex.Parser

module Say =
    let greeting name =
        sprintf "Hello %s" name

    let hello =
        greeting >> printfn "%s"