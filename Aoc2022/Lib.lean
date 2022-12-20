import Mathlib.Init.ZeroOne
import Mathlib.Data.Nat.Basic
import Mathlib.Data.Int.Basic
import Std.Data.Array.Lemmas

class Foldr.{u} (l : Type u → Type u) where
  foldr {α β : Type u} (f : α → β → β) (init : β) : l α → β
instance : Foldr List where foldr := List.foldr
instance : Foldr Array where foldr := Array.foldr

class Summable.{u} (l : Type u) (α : Type u |> outParam) where
  sum : l → α
instance {l} [Foldr l] {α} [Add α] [Zero α] : Summable (l α) α where
  sum := Foldr.foldr (.+.) 0

-- #eval Array.range 10 |> Summable.sum

def RefArray (σ : Type) (α : Type) : Type :=
  Array (ST.Ref σ α)

def DynArray (σ : Type) (α : Type) : Type :=
  ST.Ref σ (RefArray σ α)

def RefArray.getArray {σ} {α} (a : RefArray σ α) : ST σ (Array α) :=
  a.mapM ST.Ref.get

def RefArray.setArray {σ} {α} (a : RefArray σ α) (x : Array α) : ST σ Unit :=
  (a.zip x).forM (λ (s, v) => s.set v)

def DynArray.getArray {σ} {α} (a : DynArray σ α) : ST σ (Array α) :=
  a.get >>= RefArray.getArray

def DynArray.push {σ} {α} (a : DynArray σ α) (v : α) : ST σ Unit := do
  a.set <| (← a.get).push (← ST.mkRef v)

def DynArray.pop {σ} {α} (a : DynArray σ α) : ST σ Unit := do
  a.set <| (← a.get).pop


namespace List
def sum {α} [Add α] [Zero α] (l : List α) : α :=
  l.foldr (.+.) 0

def sort {α} [Inhabited α] [i : LT α] [DecidableRel i.lt] (l : List α) : List α :=
  (l.toArray.qsort (.<.)).toList

def min {α} [Min α] : (l : List α) → l ≠ [] → α
  | a :: l, _ => l.foldr Min.min a 

def max {α} [Max α] : (l : List α) → l ≠ [] → α
  | a :: l, _ => l.foldr Max.max a 

end List

namespace Array
def sum {α} [Add α] [Zero α] (l : Array α) : α :=
  l.foldr (.+.) 0

def sort {α} [Inhabited α] [i : LT α] [DecidableRel i.lt] (l : Array α) : Array α :=
  l.qsort (.<.)

def min {α} [Min α] (l : Array α) (hl : l ≠ #[]) : α :=
  l.foldr Min.min (l.get ⟨0, by sorry⟩)

def max {α} [Max α] (l : Array α) (hl : l ≠ #[]) : α :=
  l.foldr Max.max (l.get ⟨0, by sorry⟩)

end Array

def String.toArray (s : String) : Array Char := s.toList.toArray
