import Std
import HTTP.Parser
import HTTP.Types
import Socket

open Std Parsec Socket

namespace HTTP

namespace URI

def toString (uri : URI) : String :=
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

instance : ToString URI := ⟨toString⟩

def parse (s : String) : Except String URI := Parser.uri.parse s

end URI
end HTTP
