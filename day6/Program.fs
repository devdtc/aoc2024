
// Run with `dotnet run input.txt`.

open System

type CellType = Start | Block
type Dir = Up | Down | Left | Right
type Pos = int * int
type PathType = Finite | Infinite

let rotate = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

// Get the next position in direction dir.
let advance (x, y) dir =
  match dir with
  | Up -> (x, y-1)
  | Down -> (x, y+1)
  | Left -> (x-1, y)
  | Right -> (x+1, y)

// Given bounds, a set of blocks/obstacles, position, direction and path up to this point, return the remaining path and whether it's infinite or not.
let rec walk
    (bounds: Pos) (blocks: Set<Pos>)
    (pos: Pos) (dir: Dir)
    (path: Set<Pos * Dir>)
    : PathType * Set<Pos * Dir> =
  if Set.contains (pos, dir) path then
    // If we've already seen this position and direction, then the path is infinite.
    (Infinite, path)
  else
    let nextPos = advance pos dir
    let (nx, ny) = nextPos
    let (bx, by) = bounds
    if nx < 0 || nx >= bx || ny < 0 || ny >= by then
      // If the next move is out of bounds then return our finite path.
      (Finite, Set.add (pos, dir) path)
    else if Set.contains nextPos blocks then
      // Otherwise if the next move is blocked, rotate and try again.
      walk bounds blocks pos (rotate dir) path
    else
      // Finally, if we can move, add position/direction to path and continue.
      walk bounds blocks nextPos dir (Set.add (pos, dir) path)


[<EntryPoint>]
let main argv =
  // Build an input matrix from input.
  let inputMat =
    System.IO.File.ReadLines(argv.[0])
    |> Array.ofSeq
    |> Array.map (fun s -> s |> Array.ofSeq)
  // Convert the matrix into a list of points and their CellType, filtering anything not a start or block.
  let input =
    inputMat
    |> Array.mapi (fun y -> Array.mapi (fun x ch ->
      if ch = '^' then Some ((x, y), Start) else
      if ch = '#' then Some ((x, y), Block) else
                       None))
    |> Seq.collect id |> Seq.filter Option.isSome |> Seq.map Option.get
  let bounds = (inputMat[0].Length, inputMat.Length)

  let startDir = Up
  // Find the first (and only) starting location.
  let startPos =
    input
    |> Seq.map (function (pos, Start) -> Some pos | _ -> None)
    |> Seq.filter Option.isSome |> Seq.head |> Option.get

  // Build a set of positions where blocks are located.
  let blocks =
    input
    |> Seq.map (function (pos, Block) -> Some pos | _ -> None)
    |> Seq.filter Option.isSome |> Seq.map Option.get |> Set.ofSeq

  // Compute the initial path for part1 without adding any blocks.
  let initPath = snd (walk bounds blocks startPos startDir Set.empty)
  let part1 = initPath |> Set.map fst |> Set.count
  printfn "part1: %d\n" part1

  // For every point in the initial path, try adding a block there and check whether that makes the path infinite.
  let part2 =
    initPath
    |> Set.map (function (pos, _) -> pos)  // Map to pos within the Set type to get all unique positions since blocks don't have a direction
    |> Seq.map (fun pos ->  // Now count the number of blocks along that path that are infinite. We convert to Seq because we don't want to deduplicate the result (a bool).
      if pos = startPos then
        false
      else
        Infinite = fst (walk bounds (Set.add pos blocks) startPos startDir Set.empty))
    |> Seq.filter id |> Seq.length  // Count the infinite paths.
  printfn "part2: %d\n" part2

  0  // exit success
