import Lake
open Lake DSL

package http {
  srcDir := "src"
}

require socket from git "https://github.com/xubaiw/Socket.lean" @ "9fc98bc24ccdb4471b2e38e94dccd18f75d2a293"

@[defaultTarget]
lean_lib HTTP