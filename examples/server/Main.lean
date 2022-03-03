import Http

open Http

def handler (req : Request) : IO Response := do
  IO.println "Request received"
  return {
    message := "OK"
    protocol := Protocol.http "0.9"
    statusCode := 200
    headers := Headers.fromList []
    body := "Hello, World!"
  }

def main : IO Unit :=
  Http.serve 8080 handler
