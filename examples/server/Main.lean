import Http

open Http

def handler (req : Request) : IO Response := do
  let body := "Hello, World!"
  let response := {
    message := "OK"
    protocol := Protocol.http "1.1"
    statusCode := 200
    headers := Headers.fromList [("Content-Length", toString body.bsize)]
    body
  }
  return response

def main : IO Unit :=
  Http.serve 8080 handler
