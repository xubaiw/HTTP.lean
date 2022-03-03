import Std
import Http.Parser
import Http.Types
import Socket

open Std Parsec Socket

namespace Http

namespace Uri

private def toString (uri : Uri) : String :=
  s!"{uri.scheme}://"
  ++ if let some user := uri.userInfo then s!"{user}@"
  else ""
  ++ s!"{uri.host}"
  ++ if let some port := uri.port then s!":{port}"
  else ""
  ++ s!"{uri.path}"
  ++ if let some query := uri.query then s!"?{query}"
  else ""
  ++ if let some fragment := uri.fragment then s!"#{fragment}"
  else ""

instance : ToString Uri := ⟨toString⟩

def parse (s : String) : Except String Uri := Parser.uri.parse s

end Uri
end Http
