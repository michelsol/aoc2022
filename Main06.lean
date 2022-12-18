import Aoc2022.Lib

def part1 (lines : Array String) := 
  0

def main : IO Unit := do
  let lines ← IO.FS.lines "in06.txt"
  IO.println s!"part1: {part1 lines}"

-- #eval main