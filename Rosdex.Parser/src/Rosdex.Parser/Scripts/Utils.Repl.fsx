#r "System.Xml.Linq"

#load """../../../.paket/load/net461/main.group.fsx"""
#load """../Utils.fs"""
#load """../Collections.fs"""
#load """../Yandex.Yml.fs"""
#load """../Csv.fs"""

module Clipboard =
    type private Clipboard = System.Windows.Forms.Clipboard

    let setText = Clipboard.SetText

    let getText = Clipboard.GetText

// TODO: Убрать дубли с FsharpLu
module IO =
    open System.IO

    module Path =
        let combine root child = Path.Combine(root, child)

        let extension path =
            Path.GetExtension path

        let setExtension extension path =
            Path.ChangeExtension (path, extension)

        let mapExtension mapping path =
            extension path
            |> mapping
            |> setExtension <| path

    module File =
        let readLines = File.ReadLines

        let create = File.Create

        let copy overwrite source destination =
            File.Copy(source, destination, overwrite)

        let exists = File.Exists

    module Directory =
        let createDirectory = Directory.CreateDirectory >> ignore

    module Stream =
        let readBytes (stream : Stream) =
            let buffer = Array.zeroCreate 1
            seq {
                let mutable count = 1
                while count <> 0 do
                    count <- stream.Read(buffer, 0, buffer.Length)
                    yield buffer.[0]
            }

    module StreamReader =
        let readLineSafe (reader : StreamReader) =
            let buffer = Array.zeroCreate 1
            let s = seq {
                while not reader.EndOfStream do
                    reader.Read(buffer, 0, buffer.Length) |> ignore
                    yield buffer.[0] }
            s
            |> Seq.takeWhile ((<>) '\n')
            |> Array.ofSeq
            |> fun p -> new string(p)

        let readTo (writer : StreamWriter) (reader : StreamReader) =
            let buffer = Array.zeroCreate 512
            while not reader.EndOfStream do
                let count = reader.Read(buffer, 0, buffer.Length)
                writer.Write(buffer, 0, count)

type Gitignored = {
    YmlSourceFilesPath : string option
}

let gitignored =
    System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "/gitignored.json")
    |> Microsoft.FSharpLu.Json.Compact.deserialize<Gitignored>