import Lake
open Lake DSL

package server

require http from ".."/".."

@[defaultTarget]
lean_exe server {
  root := `Main
}