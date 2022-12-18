import Aoc2022.Lib

def part1 (lines : Array String) := 
  let x := lines.map λ line =>
    let t := line.splitOn ","
    let l := t[0]!.splitOn "-"
    let r := t[1]!.splitOn "-"
    let lmin := l[0]!.toNat!
    let lmax := l[1]!.toNat!
    let rmin := r[0]!.toNat!
    let rmax := r[1]!.toNat!
    if (lmin ≤ rmin && rmax ≤ lmax) || (rmin ≤ lmin && lmax ≤ rmax) then 1
    else 0
  x.sum

def part2 (lines : Array String) := 
  let x := lines.map λ line =>
    let t := line.splitOn ","
    let l := t[0]!.splitOn "-"
    let r := t[1]!.splitOn "-"
    let lmin := l[0]!.toNat!
    let lmax := l[1]!.toNat!
    let rmin := r[0]!.toNat!
    let rmax := r[1]!.toNat!
    if lmax < rmin || rmax < lmin then 0
    else 1
  x.sum

def main : IO Unit := do
  let lines ← IO.FS.lines "in04.txt"
  IO.println s!"part1: {part1 lines}"
  IO.println s!"part2: {part2 lines}"

-- #eval main