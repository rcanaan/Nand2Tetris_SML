fun foo 0 = "toff"
|   foo n = bar(n-1)
and bar 0 = "berut"
|   bar n = foo(n-1);


fun evenlength empty = 0
| evenlength (evenCons (_,ol)) = 1 + oddlength ol
and oddlength (one _) = 1
| oddlength (oddCons (_,el)) = 1 + evenlength el;

structure St = struct
  fun ad(x, y) = ml(x,y)
  fun ml
end