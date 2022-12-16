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


require std from git "https://github.com/leanprover/std4" @ "main"
