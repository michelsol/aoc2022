import Lake
open Lake DSL

package «aoc2022» {
  -- add package configuration options here
}

lean_lib «Aoc2022» {
  -- add library configuration options here
}

-- @[default_target] specifies what's built when using "lake build"
@[default_target]
lean_exe «aoc01» { root := `Main01 }
@[default_target]
lean_exe «aoc02» { root := `Main02 }
@[default_target]
lean_exe «aoc03» { root := `Main03 }
@[default_target]
lean_exe «aoc04» { root := `Main04 }
@[default_target]
lean_exe «aoc05» { root := `Main05 }
@[default_target]
lean_exe «aoc06» { root := `Main06 }
@[default_target]
lean_exe «aoc07» { root := `Main07 }
@[default_target]
lean_exe «aoc08» { root := `Main08 }
@[default_target]
lean_exe «aoc21» { root := `Main21 }

require std from git "https://github.com/leanprover/std4" @ "bd60d932e2c786c7347e57576598568b2816e316"
require Qq from git "https://github.com/gebner/quote4" @ "a387c0eb611857e2460cf97a8e861c944286e6b2"
require aesop from git "https://github.com/JLimperg/aesop" @ "5e016236e9e699691aaa9872fa380df12cd7f677"
require proofwidgets from git "https://github.com/EdAyers/ProofWidgets4" @ "f1a5c7808b001305ba07d8626f45ee054282f589"
require Cli from git "https://github.com/mhuisi/lean4-cli" @ "39229f3630d734af7d9cfb5937ddc6b41d3aa6aa"
require mathlib from git "https://github.com/leanprover-community/mathlib4" @ "b56efa53d7479fda9740f364170cbaef34699dee"
