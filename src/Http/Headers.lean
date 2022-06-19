import HTTP.Types
import Std

namespace HTTP.Headers
open Std

protected def toString (h : Headers) : String :=
  h.fold (λ acc a b => acc ++ s!"{a}: {b}, ") ""

instance : ToString Headers where
  toString := Headers.toString
  
def toRequestFormat (h : Headers) : String :=
  h.fold (λ acc a b => acc ++ s!"{a}: {b}" ++ crlf) ""

def set (self : Headers) (name : CaseInsString) (value : String) : Headers :=
  self.insert name value
  
def merge (a b : Headers) : Headers :=
  b.fold (λ a k v => a.set k v) a
  
def fromList (l : List (CaseInsString × String)) : Headers :=
  l.foldl (λ h (n, v) => h.set n v) HashMap.empty

end HTTP.Headers
