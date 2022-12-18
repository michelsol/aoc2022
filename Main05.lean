import Aoc2022.Lib

def part1 (lines : Array String) : String := 
  let stackCount := (lines[0]!.length + 1)/4
  let stackLineCount := (lines.findIdx? (. == "")).get! - 1
  let firstOrderLineIndex := stackLineCount + 2
  let orderCount := lines.size - firstOrderLineIndex
  let stacks := Id.run <| do
    let mut stacks := mkArray stackCount #[]
    for i in (Array.range stackLineCount).reverse do
      for j in Array.range stackCount do
        let c := lines[i]!.get! ⟨4 * j + 1⟩
        if !c.isWhitespace then
          stacks := stacks.set! j (stacks[j]!.push c)
    for i in Array.range orderCount do
      let line := lines[firstOrderLineIndex + i]!.splitOn " "
      let moves := line[1]!.toNat!
      let src := line[3]!.toNat! - 1
      let dst := line[5]!.toNat! - 1
      for _ in Array.range moves do
        let item := stacks[src]!.back
        stacks := stacks.set! src (stacks[src]!.pop)
        stacks := stacks.set! dst (stacks[dst]!.push item)
    return stacks
  String.mk <| (List.range stackCount).map λ stack => stacks[stack]!.back

def part2 (lines : Array String) : String := 
  let stackCount := (lines[0]!.length + 1)/4
  let stackLineCount := (lines.findIdx? (. == "")).get! - 1
  let firstOrderLineIndex := stackLineCount + 2
  let orderCount := lines.size - firstOrderLineIndex
  let stacks := Id.run <| do
    let mut stacks := mkArray stackCount #[]
    for i in (Array.range stackLineCount).reverse do
      for j in Array.range stackCount do
        let c := lines[i]!.get! ⟨4 * j + 1⟩
        if !c.isWhitespace then
          stacks := stacks.set! j (stacks[j]!.push c)
    for i in Array.range orderCount do
      let line := lines[firstOrderLineIndex + i]!.splitOn " "
      let moves := line[1]!.toNat!
      let src := line[3]!.toNat! - 1
      let dst := line[5]!.toNat! - 1
      let items := stacks[src]!.extract (stacks[src]!.size - moves) stacks[src]!.size
      for _ in Array.range moves do
        stacks := stacks.set! src (stacks[src]!.pop)
      for k in Array.range moves do
        stacks := stacks.set! dst (stacks[dst]!.push items[k]!)
    return stacks
  String.mk <| (List.range stackCount).map λ stack => stacks[stack]!.back

def main : IO Unit := do
  let lines ← IO.FS.lines "in05.txt"
  IO.println s!"part1: {part1 lines}"
  IO.println s!"part2: {part2 lines}"

-- #eval main