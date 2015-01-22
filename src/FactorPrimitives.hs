module FactorPrimitives where
import FactorDefinitions

--implementing http://docs.factorcode.org/content/vocab-kernel.html
callPrimitive' :: String -> Stack -> Stack
-- Builtin classes
-- Tuple classes
-- Union classes
-- Intersection classes

-- Generic words
-- ?execute
-- boa
-- callPrimitive "call" ((FQuotation x):xs) = (foldl (\stack word -> evalOnStack word stack) xs x)
callPrimitive' "clone" stack = stack
-- equal?
-- execute
-- hashcode*
-- new
-- throw

-- Primitives
callPrimitive' "(clone)" stack = callPrimitive' "clone" stack
callPrimitive' "-rot" (z:y:x:xs) = y : x : z : xs
callPrimitive' "drop2" (x:y:xs) = xs
callPrimitive' "dup2" stack@(x:y:_) = x : y : stack
callPrimitive' "nip2" (x:y:xs) = xs
callPrimitive' "drop3" (x:y:z:xs) = xs
callPrimitive' "dup3" stack@(x:y:z:_) = x : y : z : stack
callPrimitive' "drop4" (x:y:z:w:xs) = xs
callPrimitive' "dup4" stack@(x:y:z:w:_) = x : y : z : w : stack

callPrimitive' "<wrapper>" (x:xs) = undefined
callPrimitive' "callstack" stack = undefined
callPrimitive' "callstack>array" stack = undefined
callPrimitive' "die" stack = error "die called: exiting..."

callPrimitive' "drop" (x:xs) = xs
callPrimitive' "dup" stack@(x:_) = x : stack
callPrimitive' "dupd" (y:x:xs) = y : x : x : xs
callPrimitive' "eq?" (x:y:xs) = (FBoolean (x == y)) : xs
callPrimitive' "nip" (y:x:xs) = y : xs
callPrimitive' "over" (y:x:xs) = x : y : x : xs
callPrimitive' "pick" (z:y:x:xs) = x : z : y : x : xs
callPrimitive' "retainstack" stack = undefined
callPrimitive' "rot" (z:y:x:xs) = x : z : y : xs
callPrimitive' "swap" (x:y:xs) = y : x : xs
callPrimitive' "swapd" (z:y:x:xs) = z : x : y : xs

callPrimitive' "+" ((FNumber x):(FNumber y):xs) = (FNumber(y+x)) : xs
callPrimitive' "-" ((FNumber x):(FNumber y):xs) = (FNumber(y-x)) : xs
callPrimitive' "*" ((FNumber x):(FNumber y):xs) = (FNumber(y*x)) : xs
callPrimitive' "/" ((FNumber x):(FNumber y):xs) = (FNumber(y `div` x)) : xs
callPrimitive' "^" ((FNumber x):(FNumber y):xs) = (FNumber(y^x)) : xs
callPrimitive' "<=" ((FNumber x):(FNumber y):xs) = (FBoolean(y<=x)) : xs
callPrimitive' "abs" ((FNumber x):xs) = (FNumber $ abs x) : xs
callPrimitive' "clear" _ = []
callPrimitive' "datastack" stack = (FArray stack) : stack

callPrimitive' _ s = s


