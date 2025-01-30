```haskell
-- | 2024-01-30 Proving the monad laws for E (error) and L (logging).

-- * E
---------------------------------------------------------------------------

-- First monad law for E

return a >>= f  =  f a

return a >>= f
  = { Definition of return for E }
Value a >>= f
  = { Definition of >>= for E }
f a

-- Second monad law for E

m >>= return  =  m

Proof by cases on m.

- Case m = Value a

  Value a >>= return
    = { Definition of >>= for E }
  return a
    = { Definition of return for E }
  Value a

- Case m = Wrong

  Wrong >>= return
    = { Definition of >>= for E }
  Wrong

-- Third monad law for E

(m >>= f) >>= g  =  m >>= (\ x -> f x >>= g)

Proof by cases on m.

- Case m = Wrong

  (Wrong >>= f) >>= g
    = { Def of >>= }
  Wrong >>= g
    = { Def of >>= }
  Wrong
    = { Def of >>= }
  Wrong >>= (\ x -> f x >>= g)

- Case m = Value a

  (Value a >>= f) >>= g
    = { Def of >>= }
  f a >>= g
    = { Def of >>= }
  Value a >>= (\ x -> f x >>= g)

-- * L
---------------------------------------------------------------------------

-- First monad law for L

return a >>= f  =  f a

return a >>= f
  = { Def of return }
L a [] >>= f
  = { Def of >>= }
let L b msgs = f a in L b ([] ++ msgs)
  = { Left identity law for ++ / Def of ++ }
let L b msgs = f a in L b msgs
  = { Basic law of Haskell }
f a

-- Basic laws of Haskell:
-- * let p = e in p  ==  e
-- * do x <- m; e    == m >>= \ x -> e
-- * (\ x -> e1) e2  == [e2/x] e1
-- * e(let p = e1 in e2)  == let p = e1 in e(e2)

-- Second law for L

m >>= return  =  m

Proof by cases on m:

Case m = L a msgs

L a msgs >>= return
  = { Def of >>= }
let L b msgs2 = return a in L b (msgs ++ msgs2)
  = { Def of return }
let L b msgs2 = L a [] in L b (msgs ++ msgs2)
  = { Haskell law }
L a (msgs ++ [])
  = { Right identity law of ++ }
L a msgs

-- Lemma:
xs ++ [] = xs

Proof of Lemma by (structural) induction on xs:

- Case xs = []:  By definition of ++.

- Case xs = y : ys
  Induction hypothesis: ys ++ [] = ys

  (y : ys) ++ []
    = { By definition of ++ }
  y : (ys ++ [])
    = { By induction hypothesis on ys }
  y : ys

What about infinite xs?  Coinduction!

-- Third monad law for L

(m >>= f) >>= g  = m >>= (\ x -> f x >>= g)

(L a msgs1 >>= f) >>= g

  = { Def of >>= }

(let L b msgs2 = f a in L b (msgs1 ++ msgs2)) >>= g

  = { Haskell law }

let L b msgs2 = f a in L b (msgs1 ++ msgs2) >>= g

  = { Def of >>= }

let L b msgs2 = f a in
let L c msgs3 = g b in L c ((msgs1 ++ msgs2) ++ msgs3)

  = { Associativity of ++ }

let L b msgs2 = f a in
let L c msgs3 = g b in L c (msgs1 ++ (msgs2 ++ msgs3))

  = { Haskell law: renaming of bound variables }

let L b msgs' = f a in
let L c' msgs2' = g b in
in  L c' (msgs1 ++ (msgs' ++ msgs2'))

  = { Haskell law for let: matching }

let L b msgs' = f a in
let L c' msgs2' = g b in
let L c msgs = L c' (msgs' ++ msgs2')
in  L c (msgs1 ++ msgs)

  = { Haskell law for let: floating }

let L b msgs' = f a in
let L c msgs = (let L c' msgs2' = g b in L c' (msgs' ++ msgs2'))
in  L c (msgs1 ++ msgs)

  = { Def of >>= }

let L b msgs' = f a in
let L c msgs = L b msgs' >>= g
in  L c (msgs1 ++ msgs)

  = { Haskell law: let floating }

let L c msgs = (let L b msgs' = f a in L b msgs') >>= g
in  L c (msgs1 ++ msgs)

  = { Haskell law for let }

let L c msgs = f a >>= g
in  L c (msgs1 ++ msgs)

  = { Def of >>= }

L a msgs1 >>= (\ x -> f x >>= g)


class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a

-- Laws

Left identity:   mempty `mappend` a = a
Right identity:  a `mappend` mempty = a
Associativity:   (a `mappend` b) `mappend` c = a `mappend` (b `mappend` c)

```
