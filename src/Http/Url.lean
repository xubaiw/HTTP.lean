import Std
import Http.Parser
import Http.Types
import Socket

open Std Parsec Socket

namespace Http

namespace Url

private def toString (url : Url) : String :=
  s!"{url.scheme}://"
  ++ if let some user := url.userInfo then s!"{user}@"
  else ""
  ++ s!"{url.host}"
  ++ if let some port := url.port then s!":{port}"
  else ""
  ++ s!"{url.path}"
  ++ if let some query := url.query then s!"?{query}"
  else ""
  ++ if let some fragment := url.fragment then s!"#{fragment}"
  else ""

instance : ToString Url := ⟨toString⟩

def parse (s : String) : Except String Url := Parser.url.parse s

end Url
end Http
