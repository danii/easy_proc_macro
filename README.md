Easy Procedural Macro
=====================
_**Rust's last proc-macro crate.**_

Easy Procedural Macro is a procedural macro crate that, well, allows you to create procedural macros without having to create a whole dedicated crate for the macros. Hence why this is Rust's last proc-macro crate.

Getting started is fairly easy. Just slap `#[easy_proc_macro::easy_proc_macro]` (or import it first if you prefer) ontop of any old macro definition, and boom, it becomes a procedural macro. Not much has changed on the surface, but your macro has just gained _a super power_.

That super power being token manipulation blocks! Adding a token manipulation block to your macro is pretty straight forward. Just write a normal block with a dollar sign in front as if it was some special macro syntax, like so; `${/* ... */}`. The code inside these blocks will be ran at _compile time_, and operate upon Rust's token's directly, allowing your macros to do a whole lot more than just advanced `Ctrl + C` + `Ctrl + V`.

Tokens you access inside a token manipulation block will be a string, allowing you to modify the tokens to your heart's extent. The value a token manipulation block resolves to must implement [`ToString`], or a `String` or `&str`. The value is what the token manipulation block will expand too.

Nightly Only Features
---------------------
This crate took great care to be able to be compiled on the Rust stable compiler, but there's some things that the Rust stable compiler just can't provide. Hence why these features are only available on a nightly compiler with the `nightly` feature enabled on this crate.

### Better Error Messages
Provides error messages that point at the actual incorrect code rather than the procedural macro invokation.

#### Stable
![cannot borrow `token` as mutable, as it is not declared as mutable, pointing towards `#[easy_proc_macro]`. in this macro invocation, pointing towards `my_stringify!(value, 12)`.](https://love.catboys.space/he8PwZ9zcp.png)

#### Nightly
![cannot borrow `token` as mutable, as it is not declared as mutable, pointing towards `token`. in this macro invocation, pointing towards `my_stringify!(value, 12)`.](https://love.catboys.space/T1jfoVTVOb.png)

Thanks to the `proc_macro_diagnostic` api.

[`ToString`]: https://doc.rust-lang.org/std/string/trait.ToString.html
