import Std.Data.List.Basic
import Std.Data.Nat.Basic

def priority (a : Char) :=
  if a.isLower then a.toNat + 1 - 'a'.toNat
  else a.toNat + 1 + 26 - 'A'.toNat

def part1 (lines : Array String) := 
  let commonItems := lines.map λ line =>
    let size := line.length / 2
    let left := (line.take size).toList.toArray.qsort (.<.)
    let right := (line.takeRight size).toList.toArray.qsort (.<.)
    show Char from Id.run <| do
      let mut i := 0
      let mut j := 0
      while i < size && j < size do
        let l := left.get! i
        let r := right.get! j
        if l = r then return l
        else if l < r then i := i + 1
        else j := j + 1
      panic "no common item found"
  (commonItems.map priority).foldr (.+.) 0

def main : IO Unit := do
  let lines ← IO.FS.lines "in03.txt"
  IO.println s!"part1: {part1 lines}"

-- #eval main