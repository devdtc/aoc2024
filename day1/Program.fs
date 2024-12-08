
// Run with `dotnet run input.txt`.

open System

[<EntryPoint>]
let main argv =
  // Parse the input into left and right lists.
  let (left, right) =
    // Read the input into a list of lists of int.
    let inputArr =
       System.IO.File.ReadLines(argv.[0])
       |> Seq.map (fun s ->
         s.Split(" ", StringSplitOptions.RemoveEmptyEntries)
         |> Seq.ofArray
         |> Seq.map int)
     // Then filter by the first and last elements of each nested list (i.e. column).
     in (inputArr |> Seq.map Seq.head, inputArr |> Seq.map Seq.last)


  // Compute part1 result by sorting lists, pairing (zipping) them in order and then computing the result.
  let part1 =
    Seq.zip (Seq.sort left) (Seq.sort right)
    |> Seq.map (fun (l, r) -> Math.Abs(l - r))
    |> Seq.sum
  printfn "part1: %d\n" part1

  // Build a map of key to number of times it occurs in the second list.
  let cntMap =
    right
    |> Seq.groupBy id  // Group by the identity function such that a list containing three 5s would result in a pair (5, {5, 5, 5})
    |> Seq.map (fun (k, vals) -> (k, Seq.length vals))  // Replace the list of occurences with a count
    |> Map.ofSeq  // Convert to a map

  // Compute part2 result by looking up the occurrence count in cntMap.
  let part2 =
    left
    |> Seq.map (fun k -> k * (Option.defaultValue 0 (Map.tryFind k cntMap)))
    |> Seq.sum
  printfn "part2: %d\n" part2


  0  // exit success
