Easy Procedural Macro
=====================
_**Rust's last proc-macro crate.**_

![](https://img.shields.io/crates/d/easy_proc_macro?style=for-the-badge) ![](https://img.shields.io/tokei/lines/github/danii/easy_proc_macro?style=for-the-badge) ![](https://img.shields.io/crates/v/easy_proc_macro?label=version&style=for-the-badge) ![](https://img.shields.io/badge/compiler%20version-stable_1.45.0-007EC6?style=for-the-badge)
<br>
[![](https://img.shields.io/badge/crates.io-E6B14C?style=for-the-badge&logo=rust&logoColor=000000)](https://crates.io/crates/easy_proc_macro) [![](https://img.shields.io/badge/lib.rs-282A36?style=for-the-badge&logo=rust)](https://lib.rs/crates/easy_proc_macro) [![](https://img.shields.io/badge/github.com-24292E?style=for-the-badge&logo=github)](https://github.com/danii/easy_proc_macro) [![](https://img.shields.io/badge/sponsor_me-FF69B4?style=for-the-badge&logo=github%20sponsors&logoColor=FFFFFF)](https://github.com/sponsors/danii) [![](https://img.shields.io/badge/telegram_group-26A5E4?style=for-the-badge&logo=telegram)](https://t.me/danii_hangout)

<!--![](https://img.shields.io/badge/dynamic/json?label=total%20size&query=%24.versions%5B%3A1%5D.crate_size&suffix=%20bytes&url=https%3A%2F%2Fcrates.io%2Fapi%2Fv1%2Fcrates%2Feasy_proc_macro?cacheSeconds=86400&style=for-the-badge)-->

Easy Procedural Macro is a procedural macro that allows you to create procedural macros without having to make your own `proc-macro = true` crate.

Getting started is fairly easy. Just slap `#[easy_proc_macro::easy_proc_macro]` ontop of any old macro definition, and boom, it becomes a procedural macro. Not much might have changed on the surface, but your macro has just gained _a super power_.

That super power being _token manipulation blocks_! Adding a token manipulation block to your macro is pretty straight forward. Just write a normal block with a dollar sign in front as if it was some special macro syntax, like so; `${/* ... */}`. Code inside these blocks will be ran at _compile time_, and operate upon Rust's token's directly, allowing your macros to do a whole lot more than just advanced `Ctrl + C` + `Ctrl + V`.

Tokens you access inside a token manipulation block will be a string representation, allowing you to modify them to your heart's extent. The value a token manipulation block resolves to must implement [`ToString`], or be a `String` or `&str` (both implement `ToString`). The returned value is what the token manipulation block will expand too.

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
