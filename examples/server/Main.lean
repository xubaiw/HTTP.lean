import Http

open Http

def handler (req : Request) : IO Response := do
  return "Hello, World!"

def main : IO Unit :=
  Http.serve 8080 handler
