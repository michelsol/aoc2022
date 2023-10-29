import Std.Data.Hashmap.Basic

open Std

abbrev Monkey := String

inductive Node where
  | Const (n : Nat)
  | Add (a b : Monkey)
  | Sub (a b : Monkey)
  | Mul (a b : Monkey)
  | Div (a b : Monkey)

instance : Inhabited Node where
  default := .Const 0

instance : ToString Node where
  toString
    | .Const n => toString n
    | .Add a b => toString a ++ " + " ++ toString b
    | .Sub a b => toString a ++ " - " ++ toString b
    | .Mul a b => toString a ++ " * " ++ toString b
    | .Div a b => toString a ++ " / " ++ toString b


def parseMonkey (s : String) : Monkey := s

def parseNode (s : String) : Node :=
  bif s.contains '+' then .Add (s.splitOn " + ")[0]! (s.splitOn " + ")[1]! else
  bif s.contains '-' then .Sub (s.splitOn " - ")[0]! (s.splitOn " - ")[1]! else
  bif s.contains '*' then .Mul (s.splitOn " * ")[0]! (s.splitOn " * ")[1]! else
  bif s.contains '/' then .Div (s.splitOn " / ")[0]! (s.splitOn " / ")[1]! else
  .Const s.toNat!

partial
def eval! (h : @& HashMap Monkey Node) (m : Monkey) : (HashMap Monkey Node) × Nat :=
  let node := h.find! m
  match node with
  | .Const n => (h, n)
  | .Add a b =>
    let (h, na) := eval! h a
    let (h, nb) := eval! h b
    let r := na + nb
    let h := h.insert m (.Const r)
    (h, r)
  | .Sub a b =>
    let (h, na) := eval! h a
    let (h, nb) := eval! h b
    let r := na - nb
    let h := h.insert m (.Const r)
    (h, r)
  | .Mul a b =>
    let (h, na) := eval! h a
    let (h, nb) := eval! h b
    let r := na * nb
    let h := h.insert m (.Const r)
    (h, r)
  | .Div a b =>
    let (h, na) := eval! h a
    let (h, nb) := eval! h b
    let r := na / nb
    let h := h.insert m (.Const r)
    (h, r)

def part1 (lines : Array String) :=
  let hmap := HashMap.ofList
    (lines.map λ line =>
      let splitLine := line.splitOn ":"
      (parseMonkey (splitLine[0]!).trim, parseNode (splitLine[1]!).trim)
    ).toList
  -- hmap.toArray
  (eval! hmap "root").2

def main : IO Unit := do
  -- let lines ← IO.FS.lines "data/test21.txt"
  let lines ← IO.FS.lines "data/in21.txt"
  IO.println s!"part1: {part1 lines}"

-- #eval main
