import Http.Types
import Http.Url
import Http.Request
import Http.Response
import Http.Headers

namespace Http

namespace Client

def request (method : Method)  (url : Url) (body : Option String) : IO Response := do
  try
    let headers := Headers.fromList [("Host", url.host)]
    let request := Request.init url method headers body
    let text ← String.fromUTF8Unchecked <$> liftM request.send
    match Response.parse text with
    | Except.ok response => return response
    | Except.error e => throw <| IO.Error.userError e
  catch e =>
    throw <| IO.Error.userError s!"Request failed: {e}"

def get (url : Url) : IO Response :=
  request Method.get url none

def post (url : Url) (body : String) : IO Response :=
  request Method.post url none

end Client

end Http
