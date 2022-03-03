import Lake
open Lake DSL

package Http {
  srcDir := "src"
  defaultFacet := PackageFacet.staticLib
  dependencies := #[
    {
      name := `socket
      src := Source.git "https://github.com/xubaiw/Socket.lean" "752eadc681a3a4f967d4263c057fe41b851c9d2d"
    }
  ]
}
