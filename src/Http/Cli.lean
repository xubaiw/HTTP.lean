import HTTP
import OpenSSL

open OpenSSL
open HTTP

def main (args : List String) : IO UInt32 := do
  try
    let ctx ← Context.init ()
    let ssl ← ctx.initSSL
    
    match args with
    | [ "--get", suri ] => do
      let uri ← IO.ofExcept <| Uri.parse suri
      let response ← Client.get uri
      println! "headers : {response.headers}"
      println! "body: {response.body}"
    | [ "--post", suri, body ] => do
      let uri ← IO.ofExcept <| Uri.parse suri
      let response ← Client.post uri body
      println! "headers : {response.headers}"
      println! "body: {response.body}"
    | unknown => println! "Unknown arguments {unknown}"
    pure 0
  catch e =>
    IO.eprintln <| "error: " ++ toString e
    pure 1
