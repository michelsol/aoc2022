import Std.Data.List.Basic
import Std.Data.Int.Basic

def winsDrawOrLosesScore (a b : Int) : Int :=
  ((a - b + 1).emod 3) * 3

def part1 (lines : Array String) : Int := 
  let scores := lines.map λ line =>
    let otherShape := match line.get! ⟨0⟩ with
      | 'A' => 1 | 'B' => 2 | 'C' => 3
      | _ => panic "unknown char"
    let myShape := match line.get! ⟨2⟩ with
      | 'X' => 1 | 'Y' => 2 | 'Z' => 3 
      | _ => panic "unknown char"
    myShape + winsDrawOrLosesScore myShape otherShape
  -- dbg_trace s!"scores: {scores}"
  scores.foldr Add.add 0

def part2 (lines : Array String) : Int := 
  let scores := lines.map λ line =>
    let otherShape : Int := match line.get! ⟨0⟩ with
      | 'A' => 1 | 'B' => 2 | 'C' => 3
      | _ => panic "unknown char"
    let outcome : Int := match line.get! ⟨2⟩ with
      | 'X' => 1 | 'Y' => 2 | 'Z' => 3 
      | _ => panic "unknown char"
    let myShape := (otherShape + outcome).emod 3 + 1
    myShape + winsDrawOrLosesScore myShape otherShape
  scores.foldr Add.add 0

def main : IO Unit := do
  let lines ← IO.FS.lines "in02.txt"
  IO.println s!"part1: {part1 lines}"
  IO.println s!"part2: {part2 lines}"

-- #eval main
