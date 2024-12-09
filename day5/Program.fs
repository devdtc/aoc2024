
// Run with `dotnet run input.txt`.

open System
open System.Text.RegularExpressions

// Type representing the input row.
type Row =
  | Constraint of int * int
  | Report of list<int>
  | Empty

// Given a map of invalid successors, returns whether the report is valid.
let rec isValidReport (invalidSuccessors: Map<int, Set<int>>) (report: list<int>): bool =
  match report with
  | x :: xs -> (
    // Get the set of invalid successors for the current item x.
    let badSet = Map.tryFind x invalidSuccessors |> Option.defaultValue Set.empty
    // This item is valid if no successor exists in badSet and the rest of the report is valid.
    not (List.exists (fun y -> Set.contains y badSet) xs) &&
      isValidReport invalidSuccessors xs)
  | _ -> true

// Given a map of invalid successors and a report, returns a fixed report.
let rec fixReport (invalidSuccessors: Map<int, Set<int>>) (report: list<int>): list<int> =
  match report with
  | x :: xs -> (
    // Get the set of invalid successors for the current item x.
    let badSet = Map.tryFind x invalidSuccessors |> Option.defaultValue Set.empty
    // Partition the rest of the report into good and bad items.
    let (badSuccs, goodSuccs) = List.partition (fun y -> Set.contains y badSet) xs
    // Recursively fix the good and bad items, sticking x in between.
    (fixReport invalidSuccessors badSuccs) @ (x :: (fixReport invalidSuccessors goodSuccs)))
  | xs -> xs


[<EntryPoint>]
let main argv =
  let rows =
    let rx = Regex(@"(\d+)\|(\d+)", RegexOptions.Compiled)

    // Parse the input line by line into a sequence of Rows. Determines the kind of row by applying a regex for a constraint.
    System.IO.File.ReadLines(argv.[0])
    |> Seq.map (fun s ->
      let m = rx.Match(s)
      if m.Success then
        Constraint (int m.Groups[1].Captures[0].Value, int m.Groups[2].Captures[0].Value)
      else if s = "" then
        Empty
      else
        Report (s.Split(',') |> List.ofArray |> List.map int))

  // From the valid (before, after) constraints, build a map of invalid successors by constructing a set of all befores for each after.
  let invalidSuccessors =
    rows
    // Get a sequence of (after, before) pairs from the input, filtering out unrelated rows.
    |> Seq.map (function Constraint (before, after) -> Some (after, before) | _ -> None)
    |> Seq.filter Option.isSome |> Seq.map Option.get
    // Group by after to get all the invalid successors.
    |> Seq.groupBy fst
    // Construct a map{value, set{invalid_successors}}.
    |> Seq.map (fun (x, invalidSuccs) -> (x, Seq.map snd invalidSuccs |> Set.ofSeq))
    |> Map.ofSeq

  // Partition the reports into valid and invalid ones.
  let (validReports, invalidReports) =
    rows
    // Unwrap the reports into plain lists of ints and filter unrelated rows.
    |> Seq.map (function Report xs -> Some xs | _ -> None) |> Seq.filter Option.isSome |> Seq.map Option.get
    // Partition them by isValidReport.
    |> List.ofSeq |> List.partition (isValidReport invalidSuccessors)

  // To get the middle element, we drop half the report then take the next element. Then we sum them all
  let sumMiddles = List.map (fun xs -> List.skip ((List.length xs) / 2) xs |> List.head) >> List.sum

  let part1 = validReports |> sumMiddles
  printfn "part1: %d\n" part1

  let part2 =
    invalidReports
    |> List.map (fixReport invalidSuccessors)
    |> sumMiddles
  printfn "part2: %d\n" part2

  0  // exit success
