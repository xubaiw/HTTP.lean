import Http.Types
import Http.Headers
import Http.Parser

namespace Http.Response

def parse (s : String) : Except String Response := Parser.response.parse s

def toResponseString (r : Response) : String :=
  s!"{r.protocol.toString} {r.statusCode} {r.message}" ++
  crlf ++
  r.headers.toRequestFormat ++
  crlf ++
  (if let some body := r.body then body else "" ) ++
  crlf ++ crlf

instance : Coe String Response where
  coe s := {
    statusCode := 200
    message := "OK"
    protocol := Protocol.http "1.1"
    headers := Headers.fromList [("Content-Length", toString s.bsize)]
    body := s
  }

end Http.Response
