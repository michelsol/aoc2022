import Aoc2022.Lib
import Std.Data.HashMap.Basic

class FSTree.Class where
  FSTree : Type → Type
  children {σ} : FSTree σ → DynArray σ (FSTree σ)
  dict {σ} : FSTree σ → ST.Ref σ (Std.HashMap String Nat)
  parent {σ} : FSTree σ → Option (FSTree σ)
  totalSize {σ} : FSTree σ → ST.Ref σ Nat
  node {σ} : DynArray σ (FSTree σ) → ST.Ref σ (Std.HashMap String Nat) → Option (FSTree σ) → ST.Ref σ Nat → FSTree σ

unsafe inductive UnsafeFSTree (σ : Type) : Type where
  | node (c : DynArray σ (UnsafeFSTree σ)) (dict : ST.Ref σ (Std.HashMap String Nat))
    (parent : Option (UnsafeFSTree σ)) (totalSize : ST.Ref σ Nat)

namespace UnsafeFSTree
variable {σ} (t : UnsafeFSTree σ)
unsafe def children := match t with | node c _ _ _ => c
unsafe def dict := match t with | node _ d _ _ => d
unsafe def parent := match t with | node _ _ p _ => p
unsafe def totalSize := match t with | node _ _ _ s => s
end UnsafeFSTree

unsafe instance i0 : FSTree.Class where
  FSTree := UnsafeFSTree
  children := UnsafeFSTree.children
  dict := UnsafeFSTree.dict
  totalSize := UnsafeFSTree.totalSize
  parent := UnsafeFSTree.parent
  node := UnsafeFSTree.node

@[implemented_by i0]
instance : FSTree.Class := sorry

section
  def FSTree := FSTree.Class.FSTree (self := inferInstance)
  namespace FSTree
  def children {σ} : FSTree σ → DynArray σ (FSTree σ) := FSTree.Class.children
  def dict {σ} : FSTree σ → ST.Ref σ (Std.HashMap String Nat) := FSTree.Class.dict
  def parent {σ} : FSTree σ → Option (FSTree σ) := FSTree.Class.parent
  def totalSize {σ} : FSTree σ → ST.Ref σ Nat := FSTree.Class.totalSize
  def node {σ} : DynArray σ (FSTree σ) → ST.Ref σ (Std.HashMap String Nat) → Option (FSTree σ) → ST.Ref σ Nat → FSTree σ := FSTree.Class.node
  end FSTree
end

-- above code is a bunch of hacks to setup a mutable tree structure.

def FSTree.mkEmpty {σ} (parent : Option (FSTree σ) := none) (totalSize := 0) : ST σ (FSTree σ) := do
  return FSTree.node (← ST.mkRef #[]) (← ST.mkRef Std.mkHashMap) parent (← ST.mkRef totalSize)

partial def FSTree.getDescription {σ} (p : String := "") (tree : FSTree σ) : ST σ String := do
  let r := ← (← tree.dict.get).toList.mapM
      λ (name, id) => do
        let child ← ((← tree.children.get).get ⟨id, sorry⟩).get
        return s!"{name} ({(← child.totalSize.get)}){
            if (← child.children.get).size = 0 then "" else (← child.getDescription (p ++ "  "))
            }"
  return "\n" ++ p ++ "- " ++ ((r.intersperse <| "\n" ++ p ++ "- ").map String.toList).join.asString

-- #eval runST λ σ => do
--   let t ← FSTree.mkEmpty (σ := σ) none
--   dbg_trace s!"{← t.getDescription}"

partial def FSTree.computeSizes {σ} (tree : FSTree σ) : ST σ Unit := do
  for child in (← tree.children.getArray) do
    child.computeSizes
    tree.totalSize.modify (. + (← child.totalSize.get))

partial def FSTree.sumSizesLe100000 {σ} (tree : FSTree σ) : ST σ Nat := do
  let mut r := 0
  let a := (← tree.children.getArray)
  let isDir := a.size > 0
  let c := (← tree.totalSize.get)
  if isDir ∧ c ≤ 100000 then r := r + c
  for child in a do
    r := r + (← child.sumSizesLe100000)
  return r

partial def FSTree.findSmallestDirWithSizeGe {σ} (tree : FSTree σ) (driveSize : Nat) : ST σ Nat := do
  let minDirSize := driveSize - 40000000
  let mut r := driveSize
  let a := (← tree.children.getArray)
  let isDir := a.size > 0
  let c := (← tree.totalSize.get)
  if isDir ∧ c ≥ minDirSize then r := min r c
  for child in a do
    r := min r (← child.findSmallestDirWithSizeGe driveSize)
  return r

def part1and2 (lines : Array String) : IO Unit := do
  let root ← FSTree.mkEmpty (σ := IO.RealWorld) none
  let mut tree := root
  let mut i := 0 -- current line index
  while i < lines.size do
    let line := lines[i]!; i := i + 1
    let tokens := line.splitOn " "
    if line.get! ⟨0⟩ != '$' then unreachable!
    let command := tokens[1]!.trim.toLower
    if command = "cd" then
      let name := tokens[2]!
      if name = "/" then tree := root
      else if name = ".." then tree := match tree.parent with | .some p => p | .none => tree
      else
        match (← tree.dict.get).find? name with
        | none =>
          let id := (← tree.children.get).size
          let new ← FSTree.mkEmpty (some tree)
          tree.children.push new
          tree.dict.set <| (← tree.dict.get).insert name id
          tree := new
        | some id => tree ← ((← tree.children.get).get ⟨id, sorry⟩).get
    else if command = "ls" then
      while i < lines.size && lines[i]!.get! ⟨0⟩ != '$' do
        let line := lines[i]!; i := i + 1
        let tokens := line.splitOn " "
        let name := tokens[1]!
        let firstToken := tokens[0]!.trim.toLower
        let fileSize :=
          if firstToken = "dir" then 0
          else firstToken.toNat!
        if ((← tree.dict.get).find? name).isNone then
          let id := (← tree.children.get).size
          let new ← FSTree.mkEmpty (some tree) fileSize
          tree.children.push new
          tree.dict.set <| (← tree.dict.get).insert name id
    else unreachable!
  root.computeSizes
  -- IO.println s!"{← root.getDescription}"
  IO.println s!"{← root.sumSizesLe100000}"
  IO.println s!"{← root.findSmallestDirWithSizeGe (← root.totalSize.get)}"

def main : IO Unit := do
  let lines ← IO.FS.lines "data/in07.txt"
  IO.println s!"part1 and 2: {← part1and2 lines}"

-- #eval main
