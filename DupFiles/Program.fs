open System
open System.IO

let enumSafe lister path =
    try
        lister path
    with
        | :? System.UnauthorizedAccessException -> Seq.empty

let enumDirsSafe  = enumSafe Directory.EnumerateDirectories
let enumFilesSafe = enumSafe Directory.EnumerateFiles

let rec enumFilesRec dirFilter fileFilter dir =
    seq {
        yield! 
            enumFilesSafe dir
            |> Seq.filter fileFilter
        yield!
            enumDirsSafe dir
            |> Seq.filter dirFilter
            |> Seq.map (fun sub -> enumFilesRec dirFilter fileFilter sub)
            |> Seq.collect (fun lst -> lst)
    }

let filterExt lst =
    fun path -> List.exists (fun elem -> elem = (Path.GetExtension path)) lst

let filterOutPaths lst =
    fun path -> not (List.exists (fun elem -> elem = path) lst)

let eqFiles f1 f2 =
    let bytes1 = Seq.ofArray (File.ReadAllBytes f1)
    let bytes2 = Seq.ofArray (File.ReadAllBytes f2)
    let res = Seq.compareWith (fun x y -> (int x) - (int y)) bytes1 bytes2
    res = 0

//let eqFiles f1 f2 =
//    let toSeq (fs:FileStream) =
//        seq {   let i = fs.ReadByte()
//                if i <> -1 then yield i }
//    use fs1 = new FileStream(f1, FileMode.Open, FileAccess.Read)
//    use fs2 = new FileStream(f2, FileMode.Open, FileAccess.Read)
//    let res = Seq.compareWith (fun x y -> x - y) (toSeq fs1) (toSeq fs2)
//    res = 0

/// return tuple: (list of files equal to and inlcuding file, the rest)
let rec groupFilesEqualTo file files =
    match files with 
    | [] -> ([file], [])
    | (file2::tail) -> let (eqs, rest) = (groupFilesEqualTo file tail)
                       if eqFiles file file2 then (file2::eqs, rest) else (eqs, file2::rest)

/// split paths into groups of identical files
let groupEqualFiles paths =
    let rec groupEqualFilesRec soFar lst =
        match lst with 
        | (file::tail) ->
            let (eq, rest) = groupFilesEqualTo file tail
            if rest.IsEmpty then eq::soFar
            else groupEqualFilesRec (eq::soFar) rest
        | [] -> soFar
    groupEqualFilesRec [] paths

let rec filterOutSingletons lstOfLst =
    match lstOfLst with
    | (h::t) -> 
        let t1 = filterOutSingletons t
        if (List.length h) > 1 then h::t1 else t1
    | [] -> []

let findDuplicates () =
    enumFilesRec 
        (filterOutPaths ["c:\\Windows"; "c:\\ProgramData"; "c:\\Program Files"])
        (filterExt [".jpg"; ".gif"])
        "c:\\multimedia" 
    |> Seq.groupBy (fun path -> (Path.GetExtension path, (FileInfo path).Length))
    |> Seq.filter (fun (_, s) -> (Seq.length s) > 1) // remove singletons
    |> Seq.map (fun (_, sq) -> [for path in sq -> path]) 
    |> Seq.map groupEqualFiles
    |> Seq.map filterOutSingletons
    |> Seq.collect Seq.ofList
    |> Seq.iter (fun lst -> printfn "%A" lst)

[<EntryPoint>]
let main _ =
    findDuplicates ()
    0