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


require std from git "https://github.com/leanprover/std4" @ "main"
require mathlib from git "https://github.com/leanprover-community/mathlib4.git" @ "master"

