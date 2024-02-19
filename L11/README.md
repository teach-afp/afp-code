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

Types (`Ty`): `nat`, `bool`

[`Tagless.agda`](Tagless.agda)

- Typed terms `Tm : Ty → Type`
- Tagless values `Val : Ty → Type`
- Tagless evaluation
- Type inference
- Deciding type equality

Tagless interpreter with binding
--------------------------------

Add let-binding and variables to the language.

[`Let.agda`](`Let.agda`)

- Contexts `Cxt`
- de Bruijn indices
- Typed terms `Tm : Cxt → Ty → Type`
- Environments for evaluation and type inference

[`AnyAll.agda`](`AnyAll.agda`)

- Introduce `Any` and `All` generalizations
