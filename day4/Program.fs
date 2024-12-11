
// Run with `dotnet run input.txt`.

open System

// Given a matrix, is the char at (i, j) equal to ch.
let isCharAt (mat: array<array<char>>) (i: int) (j: int) (ch: char): bool =
  if i < 0 || j < 0 || i >= mat.Length then false
  else
    let row = mat[i]
    if j >= row.Length then false
    else row[j] = ch

// Given a matrix and point, count the number of occurrences of str in all directions.
let countStrAtPoint =
  // Recursively walk in direction (di, dj) to see if it spells out chars.
  let rec matchesCharSeq
      (mat: array<array<char>>)
      (i: int) (j: int)
      (di: int) (dj: int)
      (chars: seq<char>): bool =
    if Seq.isEmpty chars then true
    else
      isCharAt mat i j (Seq.head chars) &&
        (matchesCharSeq mat (i + di) (j + dj) di dj (Seq.tail chars))

  // All possible directions
  let dirs = [(1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1)]

  fun mat i j str ->
    // Count the number of directions that spell out str.
    dirs
    |> List.filter (fun (di, dj) -> matchesCharSeq mat i j di dj str)
    |> List.length


[<EntryPoint>]
let main argv =
  // Read the input into a matrix of characters.
  let input =
    System.IO.File.ReadLines(argv.[0])
    |> Array.ofSeq
    |> Array.map (fun s -> s |> Array.ofSeq)

  // For each point, count the number of directions that spell out XMAS, then sum the resulting matrix of counts.
  let part1 =
    input
    |> Array.mapi (fun i row ->
      row |> Array.mapi (fun j _ ->
        countStrAtPoint input i j "XMAS"))
    |> Array.map Array.sum
    |> Array.sum
  printfn "part1: %d\n" part1

  // For each point containing the letter 'A', check whether it forms an X of "MAS" about that point, then sum.
  let part2 =
    input
    |> Array.mapi (fun i row ->
      row |> Array.mapi (fun j ch ->
        if ch = 'A' then
          let isTl = isCharAt input (i - 1) (j - 1)
          let isBr = isCharAt input (i + 1) (j + 1)
          let isBl = isCharAt input (i + 1) (j - 1)
          let isTr = isCharAt input (i - 1) (j + 1)
          let res1 = (isTl 'M' && isBr 'S') || (isTl 'S' && isBr 'M')
          let res2 = (isBl 'M' && isTr 'S') || (isBl 'S' && isTr 'M')
          res1 && res2
        else false))
    |> Array.map (Array.filter id >> Array.length)  // The `>>` operator lets us compose operations, first filtering then taking the length.
    |> Array.sum
  printfn "part2: %d\n" part2

  0  // exit success
