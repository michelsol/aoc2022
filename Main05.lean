import Aoc2022.Lib

def part1 (lines : Array String) := 
  let x := lines.map λ line =>
    0
  x.sum

def main : IO Unit := do
  let lines ← IO.FS.lines "ex05.txt"
  IO.println s!"part1: {part1 lines}"

-- #eval main