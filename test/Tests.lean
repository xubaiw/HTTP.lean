import Http

open Http Http.Url

#eval Parser.hostName.parse "yatima.io"
#eval Parser.pathParser.parse "/yatima.io/index.html"
#eval Url.parse "http://yatima.io/"

def main (args : List String) : IO UInt32 := do
  try
    let url ← IO.ofExcept <| Url.parse "http://yatima.io/test?1=1#a"
    println! "{url}"
    pure 0
  catch e =>
    IO.eprintln s!"error: {e}"
    pure 1

