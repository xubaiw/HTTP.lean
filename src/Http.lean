import Http.Types
import Http.Url
import Http.Request
import Http.Response
import Http.Headers
import Socket

open Socket

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

def serve (port : UInt16) (handler : Request → IO Response) : IO Unit := do
  -- bind
  let localAddr ← SockAddr.mk "localhost" "8080" AddressFamily.inet SockType.stream
  let socket ← Socket.mk AddressFamily.inet SockType.stream
  socket.bind localAddr
  socket.listen 5

  let baseUrl := {
    userInfo := none
    host := "localhost"
    port := port
    scheme := "http"
    path := []
    query := none
    fragment := none
  }
  -- serving
  repeat do
    let (remoteAddr, socket') ← socket.accept
    let recv ← String.fromUTF8Unchecked <$> socket'.recv 5000
    if let Except.ok request := Request.parse baseUrl recv then
      let response ← handler request
      -- TODO: add logging
      discard <| socket'.send response.toResponseString.toUTF8
    -- TODO: handle HTTP request error handling
    else
      IO.println s!"Invalid request: {recv}"
    socket'.close
    
end Http
