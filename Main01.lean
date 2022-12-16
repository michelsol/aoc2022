import Std.Data.List.Basic
import Std.Data.Nat.Basic

def part1and2 (lines : Array String) := 
  let elves := (lines.toList.splitOn "").toArray
  let sums := elves.map λ elf => Nat.sum <| elf.map String.toNat!
  let sorted := (sums.qsort Nat.blt).reverse
  let answer1 := sorted.toList.get! 0
  let answer2 := Nat.sum ((Array.range 3).map sorted.toList.get!).toList
  (answer1, answer2)

def main : IO Unit := do
  let lines ← IO.FS.lines "in01.txt"
  IO.println s!"part1: {part1and2 lines}"

-- #eval main