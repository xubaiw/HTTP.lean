import Lake
open Lake DSL

package server {
  dependencies := #[{
    name := `Http
    src := Source.path "../.."
  }]
}
