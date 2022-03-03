import Http.Types
import Http.Headers
import Http.Url
import Socket

namespace Http.Request

open Socket

def init (url : Url) (method : Method) (headers : Headers) (body : Option String) : Request :=
  {
    url,
    method,
    headers,
    body,
    protocol := url.scheme.asProtocol
  }

def toRequestString (r : Request) : String :=
  s!"{r.method} {r.url.path} {r.protocol.toString}" ++ crlf ++
  r.headers.toRequestFormat ++
  crlf ++ crlf ++
  if let some body := r.body then body else ""
  
open Protocol in
def send (request : Request) : IO ByteArray := do
  let defaultPort :=
    match request.protocol with
    | http _ => 80
    | https _ => 443
    | _ => 80
  let host := request.url.host
  let port := request.url.port.getD defaultPort |> ToString.toString

  let remoteAddr ← SockAddr.mk host port AddressFamily.inet SockType.stream
  let socket ← Socket.mk AddressFamily.inet SockType.stream
  socket.connect remoteAddr
  let strSend := request.toRequestString
  let bytesSend ← socket.send strSend.toUTF8
  let bytesRecv ← socket.recv 5000
  return bytesRecv

def parse (baseUrl : Url) (s : String) : Except String Request := (Parser.request baseUrl).parse s

end Http.Request
