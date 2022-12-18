import Std.Data.List.Basic
import Std.Data.Nat.Basic
import Aoc2022.Lib

def priority (a : Char) :=
  if a.isLower then a.toNat + 1 - 'a'.toNat
  else a.toNat + 1 + 26 - 'A'.toNat

def part1 (lines : Array String) := 
  let commonItems := lines.map λ line =>
    let size := line.length / 2
    let left := (line.take size).toArray.sort
    let right := (line.takeRight size).toArray.sort
    Id.run <| do
      let mut i := 0
      let mut j := 0
      repeat
        let l := left.get! i
        let r := right.get! j
        if l = r then return l
        else if l < r then i := i + 1
        else j := j + 1
      unreachable!
  (commonItems.map priority).sum

def part2 (lines : Array String) : Nat := 
  let groups := lines.foldl
    (λ a s => match a.back? with
      | none => #[#[s]]
      | some last => if last.size < 3
        then a.set! (a.size - 1) (last.push s)
        else a.push #[s]
      ) #[]
  let commonItems := groups.map λ group => 
    let group := group.map λ sack => sack.toArray.sort
    let s1 := group.get! 0
    let s2 := group.get! 1
    let s3 := group.get! 2
    Id.run <| do
      let mut i1 := 0
      let mut i2 := 0
      let mut i3 := 0
      repeat
        let c1 := s1.get! i1
        let c2 := s2.get! i2
        let c3 := s3.get! i3
        if c1 = c2 && c2 = c3 then return c1
        else if c1 <= c2 && c1 <= c3 then i1 := i1 + 1
        else if c2 <= c1 && c2 <= c3 then i2 := i2 + 1
        else if c3 <= c1 && c3 <= c1 then i3 := i3 + 1
        else unreachable!
      unreachable!
  (commonItems.map priority).sum

def main : IO Unit := do
  let lines ← IO.FS.lines "in03.txt"
  IO.println s!"part1: {part1 lines}"
  IO.println s!"part2: {part2 lines}"

-- #eval main