import HTTP.Types
import HTTP.Headers
import HTTP.Uri
import Socket

namespace HTTP.Request

open Socket

def init (uri : Uri) (method : Method) (headers : Headers) (body : Option String) : Request :=
  {
    uri,
    method,
    headers,
    body,
    protocol := uri.scheme.asProtocol
  }

def toRequestString (r : Request) : String :=
  s!"{r.method} {r.uri.path} {r.protocol.toString}" ++ crlf ++
  r.headers.toRequestFormat ++
  crlf ++
  (if let some body := r.body then body else "") ++
  crlf ++ crlf
  
open Protocol in
def send (request : Request) : IO ByteArray := do
  let defaultPort :=
    match request.protocol with
    | http _ => 80
    | https _ => 443
    | _ => 80
  let host := request.uri.host
  let port := request.uri.port.getD defaultPort |> ToString.toString

  let remoteAddr ← SockAddr.mk host port AddressFamily.inet SockType.stream
  let socket ← Socket.mk AddressFamily.inet SockType.stream
  socket.connect remoteAddr
  let strSend := request.toRequestString
  let bytesSend ← socket.send strSend.toUTF8
  let bytesRecv ← socket.recv 5000
  return bytesRecv

def parse (baseUri : Uri) (s : String) : Except String Request := (Parser.request baseUri).parse s

end HTTP.Request
