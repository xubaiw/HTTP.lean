import Std
namespace Http

namespace Uri

def Hostname := String

deriving instance ToString for Hostname

def Scheme := String
 deriving BEq

def Scheme.mk (s: String) := s

deriving instance ToString for Scheme

def Path := List String

instance : ToString Path where
  toString p := p.foldl (λ acc s => acc ++ s) "/"

structure UserInfo where
  username : String
  password : Option String

instance : ToString UserInfo where
  toString ui := ""

def Fragment := String
 deriving BEq

deriving instance ToString for Fragment

def Query := String
 deriving BEq

deriving instance ToString for Query

end Uri

open Uri

structure Uri where
  userInfo : Option UserInfo
  host: Hostname
  port: Option UInt16
  scheme: Scheme
  path: Path
  query: Option Query
  fragment: Option Fragment

def crlf : String := "\r\n"

/--
A Case insensitive String with case insensitive BEq and Hashable instances.
-/
def CaseInsString := String
  deriving ToString

instance caseInsensitiveStringBEq : BEq CaseInsString where
  beq s1 s2 := s1.capitalize == s2.capitalize

instance caseInsensitiveStringHashable : Hashable CaseInsString where
  hash s := s.capitalize.hash

inductive Method
  | get
  | head
  | post
  | put
  | delete
  | connect
  | options
  | trace
  | patch

def Method.toString: Method → String
  | get => "GET"
  | head => "HEAD"
  | post => "POST"
  | put  => "PUT"
  | delete => "DELETE"
  | connect => "CONNECT"
  | options => "OPTIONS"
  | trace => "TRACE"
  | patch => "PATCH"

instance : ToString Method where
  toString := Method.toString

inductive Protocol
  | http (version : String)
  | https (version : String)
  | other (name : String) (version : String)

def Protocol.toString : Protocol → String
  | http v => s!"HTTP/{v}"
  | https v => s!"HTTPS/{v}"
  | other name v => s!"{name.capitalize}/{v}"

open Protocol in
def Uri.Scheme.asProtocol (s : Scheme) : Protocol :=
  match ToString.toString s with
  | "http" => http "1.1"
  | "https" => https "1.2"
  | s => other s ""

instance : ToString Protocol where
  toString := Protocol.toString

/-
Meta information for Requests and Responses.
-/
def Headers := Std.HashMap CaseInsString String

structure Request where
  uri : Uri
  protocol : Protocol
  method : Method
  headers : Headers
  body : Option String

/-
A HTTP response from a request.
-/
structure Response where
  message : String
  protocol : Protocol
  statusCode : Nat
  headers : Headers
  body : Option String

end Http
