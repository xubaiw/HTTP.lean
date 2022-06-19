# HTTP basics for Lean

Uses [Socket.lean](https://github.com/xubaiw/Socket.lean) to create basic HTTP functionality. 

Build with `nix build .`

## Command line examples

It can be used from the commandline like so

```
nix run . -- --get http://www.example.com
```

## Code examples

A basic snippet to perform a GET request.

```lean
import HTTP

open HTTP

def main : IO Unit :=
  -- Supports (basic) URL parsing
  let uri ← URI.parse "http://www.example.com"
  let response ← Client.get uri
  if let some body := response.body then
    println! body
  else
    println! "no body in response"
```

# Features TODO

- [ ] Parsing and sanitizing special characters in URIs
- [ ] HTTPs and OpenSSL.lean integration
- [ ] Add doc-gen4
