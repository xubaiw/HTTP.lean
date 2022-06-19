import HTTP

open HTTP

def handler (req : Request) : IO Response := do
  IO.println req.headers
  return "Hello, World!"

def main : IO Unit := do
  IO.println "serving at http://localhost:8080"
  HTTP.serve 8080 handler
