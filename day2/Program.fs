
// Run with `dotnet run input.txt`.

open System

// Define a type that's either Inc, Dec or Same
type Dir = Inc | Dec | Same

let isSafe =
  // Converts two consecutive numbers into a Dir.
  let getDir x y =
    if y > x then Inc else
    if x > y then Dec else
    Same

  // Do two Dirs match. Same matches with any direction.
  let dirsMatch d1 d2 = d1 = Same || d2 = Same || d1 = d2

  let rec go (prevDir : Dir) (xs : list<int>) : bool =
    match xs with
    // For a list containing at least two numbers, check if they're safe and repeat for the rest of the list.
    | x :: y :: rest -> (
      let diff = Math.Abs(x - y)
      let dir = getDir x y

      diff >= 1 && diff <= 3 &&
      dirsMatch prevDir dir &&
      go dir (y :: rest))
    // Otherwise, there are 1 or fewer numbers, which is trivially safe.
    | _ -> true

  // A lambda that calls the main body (go).
  fun xs -> go Same xs


[<EntryPoint>]
let main argv =
  // Read the input into a list of reports (list of int).
  let input =
    System.IO.File.ReadLines(argv.[0])
    |> List.ofSeq
    |> List.map (fun s ->
      s.Split(" ", StringSplitOptions.RemoveEmptyEntries)
      |> List.ofArray
      |> List.map int)

  // Apply the isSafe function to each report, filter it to just the ones that were true, then count.
  let part1 = List.map isSafe input |> List.filter id |> List.length
  printfn "part1: %d\n" part1

  // For each report generate every variation of the report with 1 item removed, check if any are safe, then count.
  let part2 =
    input
    |> List.map
      (fun xs ->
        List.map
          (fun i -> List.removeAt i xs)
          [0 .. ((List.length xs) - 1)]
        |> List.map isSafe
        |> List.exists id)
    |> List.filter id |> List.length
  printfn "part2: %d\n" part2


  0  // exit success
