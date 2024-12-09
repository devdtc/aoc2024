
// Run with `dotnet run input.txt`.

open System
open System.Text.RegularExpressions

type Op =
  | Mul of int * int
  | Do
  | Dont


[<EntryPoint>]
let main argv =
  let input = System.IO.File.ReadAllText(argv.[0])

  // Construct a sequential list of operations from input.
  let ast =
    let rx = Regex(@"mul\((\d+),(\d+)\)|do\(\)|don't\(\)", RegexOptions.Compiled)

    rx.Matches(input)
    |> Seq.map (fun m ->
        match m.Value with
        | "do()" -> Do
        | "don't()" -> Dont
        | _ -> Mul (int m.Groups[1].Captures[0].Value, int m.Groups[2].Captures[0].Value))

  // Map every operation to a numeric value. We use the `function` syntax which combines a lambda
  // with a match statement, so instead of `fun x -> match x with ...` we write `function ...`.
  let part1 =
    ast
    |> Seq.map (function Mul (x, y) -> x * y | _ -> 0)
    |> Seq.sum
  printfn "part1: %d\n" part1

  // Use a fold operation to accumulate every element. The accumulator is the pair (on, current_value) where
  // on is a bool denoting whether to sum upcoming Mul ops, and current_value is the current intermediate result
  // from previous ops.
  let part2 =
    ast
    |> Seq.fold
      (fun (on, cur) next ->
        match (on, next) with
        | (_, Do) -> (true, cur)
        | (_, Dont) -> (false, cur)
        | (true, Mul (x, y)) -> (true, cur + (x * y))
        | _ -> (on, cur))
      (true, 0)  // Initial accumulator value (on=true, cur=0)
    |> snd  // Return the second value of the (on, cur) pair to get cur.
  printfn "part2: %d\n" part2

  0  // exit success
