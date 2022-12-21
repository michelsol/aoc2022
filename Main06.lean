import Aoc2022.Lib

def part (lines : Array String) (n : Nat) : Nat := 
  let l := lines[0]!.toArray
  Id.run <| do
    let mut i := 0
    repeat
      let a := (l.extract i (i + n)).sort
      let mut hasDuplicates := false
      for j in Array.range (n - 1) do
        if a[j]! = a[j + 1]! then
          hasDuplicates := true
      if !hasDuplicates then return i + n 
      i := i + 1
    unreachable!

def main : IO Unit := do
  let lines ← IO.FS.lines "data/in06.txt"
  IO.println s!"part1: {part lines 4}"
  IO.println s!"part1: {part lines 14}"

-- #eval main
