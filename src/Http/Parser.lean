import Http.Types
import Http.Parsec
import Std.Data.HashMap

open Parsec Std Http.Uri

namespace Http.Parser

def schemeParser : Parsec Scheme :=
  Scheme.mk <$> manyChars (asciiLetter <|> oneOf ['+', '-', '.'])

def hostName : Parsec Hostname := do
  let name := many1Chars (asciiLetter <|> digit <|> pchar '-')
  let start := name ++ pstring "."
  many1Strings start ++ name

def parseDigit! (c : Char) : Nat :=
  match c with
  | '0' => 0
  | '1' => 1
  | '2' => 2
  | '3' => 3
  | '4' => 4
  | '5' => 5
  | '6' => 6
  | '7' => 7
  | '8' => 8
  | '9' => 9
  | _ => panic! "Not a digit"

def parseUInt16 : Parsec UInt16 := do
  let as ← many1 digit
  let mut n := 0
  for (i, c) in as.toList.reverse.enum do
    let d := parseDigit! c
    n := n + d * 10 ^ i
  return n.toUInt16

def maybePort : Parsec (Option UInt16) := do
  option $ parseUInt16

def psegment : Parsec String :=
  many1Chars <| oneOf ['-', '%', '_', '+', '$', '.', ':', '*', '@' ] <|> asciiLetter <|> digit

partial def pathParser : Parsec Path := do
  let pathString ← many1Chars <| oneOf ['-', '%', '_', '+', '$', '.', ':', '*', '@', '/'] <|> asciiLetter <|> digit
  return pathString.splitOn "/" |>.filter (fun s => s.length ≠ 0)

def userInfoParser : Parsec UserInfo := do
  let username ← many1Chars <| asciiLetter <|> digit
  let password ← option do skipChar ':'; many1Chars <| asciiLetter <|> digit
  skipChar '@'
  return { username, password : UserInfo }

partial def queryParser : Parsec Query := do
  skipChar '?'
  manyChars <| satisfy (· ≠ '#')

partial def fragmentParser : Parsec Fragment := do
  skipChar '#'
  rest

def uri : Parsec Uri := do  
  let scheme ← schemeParser
  skipString "://"
  let userInfo ← option userInfoParser
  let host ← hostName
  let optPort ← maybePort
  let path ← pathParser
  let query ← option queryParser
  let fragment ← option fragmentParser
  return { scheme, host, port := optPort, path, query, fragment, userInfo : Uri }

def relativeUri (baseUri : Uri) : Parsec Uri := do  
  let path ← pathParser
  let query ← option queryParser
  let fragment ← option fragmentParser
  return { baseUri with path, query, fragment }

def header : Parsec (CaseInsString × String) := do
  let key ← many1Chars (asciiLetter <|> pchar '-')
  ws
  skipChar ':'
  ws  
  let value ← manyChars <| satisfy (λ c => c != '\n')
  ws
  return (key, value)

def headers : Parsec Headers := do
  let headers : HashMap CaseInsString String
    ← Array.foldl (λ map (k ,v) => map.insert k v) HashMap.empty <$> (many header)
  ws
  return headers

def protocol : Parsec Protocol := do
  let name ← many1Chars asciiLetter
  skipChar '/'
  let version ← many1Chars (digit <|> pchar '.')
  match name with
  | "HTTP" => return Protocol.http version
  | "HTTPS" => return Protocol.https version
  | s => return Protocol.other s version

def method : Parsec Method := attempt do
  let m ← many1Chars asciiLetter
  match m with
  | "GET" => return Method.get
  | "POST" => return Method.post
  | "PUT" => return Method.put
  | "DELETE" => return Method.delete
  | "HEAD" => return Method.head
  | "OPTIONS" => return Method.options
  | "CONNECT" => return Method.connect
  | "TRACE" => return Method.trace
  | "PATCH" => return Method.patch
  | s => fail s!"Fail to parse method {s}"

def digitsToNat (ds : Array Nat) : Nat :=
  ds.toList.enum.foldl (λ acc (d, i) => acc + d * 10 ^ i) 0

def response : Parsec Response := do
  let protocol ← protocol
  ws
  let statusCode ← digitsToNat <$> many1 digitNat
  ws
  let message ← manyChars <| satisfy (λ c => c != '\n')
  ws
  let headers ← headers
  let body ← rest
  return {
    protocol,
    headers,
    message,
    body := some body,
    statusCode,
  }

def request (baseUri : Uri) : Parsec Request := do
  let method ← method
  ws
  let uri ← relativeUri baseUri
  ws
  let protocol ← protocol
  ws
  let headers ← headers
  let body ← rest
  return {
    method,
    uri,
    protocol,
    headers,
    body := some body,
  }

end Http.Parser