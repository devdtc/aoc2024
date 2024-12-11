
// Run with `dotnet run input.txt`.

open System

// Return whether there's a valid sequence of operations over parts that equal result.
let isValidEq (ops: list<int64 -> int64 -> int64>) (result: int64, parts: list<int64>): bool =
  let rec go cur = function
    | x :: xs -> List.exists (fun op -> go (op cur x) xs) ops
    | [] -> cur = result
  go (List.head parts) (List.tail parts)

let concatOp (a: int64) (b: int64): int64 = int64 ((string a) + (string b))


[<EntryPoint>]
let main argv =
  // Read the input into a list of results and equations (list of values).
  let input =
    System.IO.File.ReadLines(argv.[0])
    |> Seq.map (fun s ->
      let split1 = s.Split(':', StringSplitOptions.RemoveEmptyEntries)
      let split2 = split1[1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
      (int64 split1[0], Seq.map int64 split2 |> List.ofSeq))

  let ops = [(+); (*)]
  let part1 = input |> Seq.filter (isValidEq ops) |> Seq.map fst |> Seq.sum
  printfn "part1: %d\n" part1

  let part2 = input |> Seq.filter (isValidEq (concatOp :: ops)) |> Seq.map fst |> Seq.sum
  printfn "part2: %d\n" part2

  0  // exit success
