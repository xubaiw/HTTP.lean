import HTTP

open HTTP HTTP.URI

#eval Parser.hostName.parse "yatima.io"
#eval Parser.pathParser.parse "/yatima.io/index.html"
#eval URI.parse "http://yatima.io/"

def main (args : List String) : IO UInt32 := do
  try
    let uri ← IO.ofExcept <| URI.parse "http://yatima.io/test?1=1#a"
    println! "{uri}"
    pure 0
  catch e =>
    IO.eprintln s!"error: {e}"
    pure 1

