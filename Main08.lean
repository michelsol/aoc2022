import Aoc2022.Lib

notation:max "#[:" b "]" => (Array.range b)
notation:max "#[" a ":" b "]" => (Array.map (. + a) (Array.range (b - a)))

def numberOfVisibleTrees (a : Array (Array Nat)) : Nat := runST λ σ => do
  let m := a.size
  let n := a[0]!.size
  let visible : Array (Array (ST.Ref σ Bool)) ← a.mapM <| Array.mapM λ _ => ST.mkRef false
  for i in #[:m] do
    let mut tallestSoFar : Int := -1
    for j in #[:n] do
      if a[i]![j]! > tallestSoFar then
        tallestSoFar := a[i]![j]!
        ((visible.get ⟨i, sorry⟩).get ⟨j, sorry⟩).set true
  for i in #[:m] do
    let mut tallestSoFar : Int := -1
    for j in #[:n].reverse do
      if a[i]![j]! > tallestSoFar then
        tallestSoFar := a[i]![j]!
        ((visible.get ⟨i, sorry⟩).get ⟨j, sorry⟩).set true
  for j in #[:n] do
    let mut tallestSoFar : Int := -1
    for i in #[:m] do
      if a[i]![j]! > tallestSoFar then
        tallestSoFar := a[i]![j]!
        ((visible.get ⟨i, sorry⟩).get ⟨j, sorry⟩).set true
  for j in #[:n] do
    let mut tallestSoFar : Int := -1
    for i in #[:m].reverse do
      if a[i]![j]! > tallestSoFar then
        tallestSoFar := a[i]![j]!
        ((visible.get ⟨i, sorry⟩).get ⟨j, sorry⟩).set true
  let mut c := 0
  for i in #[:m] do
    for j in #[:n] do
      if (← ((visible.get ⟨i, sorry⟩).get ⟨j, sorry⟩).get) = true then
        c := c + 1
  return c

def bestScore (a : Array (Array Nat)) : Nat := runST λ _ => do
  let m := a.size
  let n := a[0]!.size
  let mut c := 0
  for i in #[:m] do
    for j in #[:n] do
        let mut l := j
        for k in #[:j].reverse do
          if a[i]![k]! >= a[i]![j]! then l := j - k; break
        let mut r := n - 1 - j
        for k in #[j+1:n] do
          if a[i]![k]! >= a[i]![j]! then r := k - j; break
        let mut u := i
        for k in #[:i].reverse do
          if a[k]![j]! >= a[i]![j]! then u := i - k; break
        let mut d := m - 1 - i
        for k in #[i+1:m] do
          if a[k]![j]! >= a[i]![j]! then d := k - i; break
        let score := l * r * u * d
        c := max c score
  return c

def part1 (lines : Array String) : Nat := 
  let a := lines.map λ line =>
    line.toArray.map λ 
      | '0' => 0 | '1' => 1 | '2' => 2 | '3' => 3 | '4' => 4
      | '5' => 5 | '6' => 6 | '7' => 7 | '8' => 8 | '9' => 9
      | _ => unreachable!
  -- dbg_trace s!"input: {a}"
  dbg_trace s!"visible trees: {numberOfVisibleTrees a}"
  dbg_trace s!"best score : {bestScore a}"
  0

def main : IO Unit := do
  let lines ← IO.FS.lines "data/ex08.txt"
  IO.println s!"part1: {part1 lines}"
  -- IO.println s!"part2: {part2 lines}"

-- #eval main