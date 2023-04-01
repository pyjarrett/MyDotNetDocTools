// Performs basic validation that snippets exist.

open System.IO
open System.Text.RegularExpressions

let dotNetDocsRoot = "D:/dev/open_source/dotnet-api-docs"
let dotNetDocsDirs = [| dotNetDocsRoot + "/xml" |]

let rec traverseFiles (dirs: seq<string>) : seq<string> =
    if Seq.isEmpty dirs then
        Seq.empty
    else
        seq {
            yield! dirs |> Seq.collect Directory.EnumerateFiles
            yield! dirs |> Seq.collect Directory.EnumerateDirectories |> traverseFiles
        }

let pattern =
    ":::code language=\"([^\"]+)\" source=\"~([^\"]+)\" id=\"([^\"]+)\":::"

let regex = Regex(pattern)

let processLine file lineIndex line : string option =
    let catch = regex.Match(line)

    if catch.Success then
        let language = catch.Groups[1].Value
        let source = catch.Groups[2].Value
        let id = catch.Groups[3].Value

        if not <| File.Exists(Path.Join(dotNetDocsRoot, source)) then
            Some($"{file}:{lineIndex + 1} Bad snippet: {language} {source} {id}")
        else
            None
    else
        None

let checkLines (file: string) (lines: string[]) : string[] =
    let processFn = processLine file

    lines
    |> Array.mapi processFn
    |> Array.filter (fun res -> res.IsSome)
    |> Array.map (fun (res: string option) -> res.Value)

type ValidationResult = { errors: List<string>; files: int32 }

let checkFile (file: string) : string[] =
    File.ReadAllLines file |> checkLines file

let result =
    traverseFiles (Array.toSeq dotNetDocsDirs)
    |> Seq.filter (fun (file: string) -> Path.GetExtension file = ".xml")
    |> Seq.fold
        (fun prev file ->
            { errors = List.append prev.errors (checkFile file |> Array.toList)
              files = prev.files + 1 })
        { errors = List.Empty; files = 0 }

if result.errors.IsEmpty then
    printfn "OK! No errors"
else
    eprintfn "Errors found in snippet links"
    let printErr = printfn "%O"
    result.errors |> List.iter printErr
