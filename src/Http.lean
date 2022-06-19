import HTTP.Types
import HTTP.Uri
import HTTP.Request
import HTTP.Response
import HTTP.Headers
import Socket

open Socket

namespace HTTP

namespace Client

def request (method : Method)  (uri : Uri) (body : Option String) : IO Response := do
  try
    let headers := Headers.fromList [("Host", uri.host)]
    let request := Request.init uri method headers body
    let text ← String.fromUTF8Unchecked <$> liftM request.send
    match Response.parse text with
    | Except.ok response => return response
    | Except.error e => throw <| IO.Error.userError e
  catch e =>
    throw <| IO.Error.userError s!"Request failed: {e}"

def get (uri : Uri) : IO Response :=
  request Method.get uri none

def post (uri : Uri) (body : String) : IO Response :=
  request Method.post uri none

end Client

def serve (port : UInt16) (handler : Request → IO Response) : IO Unit := do
  -- bind
  let localAddr ← SockAddr.mk "localhost" "8080" AddressFamily.inet SockType.stream
  let socket ← Socket.mk AddressFamily.inet SockType.stream
  socket.bind localAddr
  socket.listen 5

  let baseUri := {
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
    
    match Request.parse baseUri recv with
    | Except.ok request =>
      let response ← handler request
      -- TODO: add logging
      discard <| socket'.send response.toResponseString.toUTF8
    | Except.error e =>
      IO.println s!"Error: {e}"
    socket'.close
    
end HTTP
