Leveraging types to write tagless interpreters
==============================================

Tagless interpreter for uni-typed language
------------------------------------------

Language: plus

[`HuttonsRazor.agda`](HuttonsRazor.agda)

Tagged interpreter
------------------

Language: plus, if

[`TaggedInterpreterAux.agda`](TaggedInterpreterAux.agda)

- no variables for now
- dynamically typed values

[`TaggedInterpreter.agda`](TaggedInterpreter.agda)

- Introduces `case_of_` and `λ where`

[`TaggedInterpreterDo.agda`](TaggedInterpreterDo.agda)

- Introduces `do` and `p ← e where ...`

Tagless interpreter
-------------------

[`Tagless.agda`](Tagless.agda)
