//! _**Rust's last proc-macro crate.**_
//!
//! Easy Procedural Macro is a procedural macro that allows you to create
//! procedural macros without having to make your own `proc-macro = true` crate.
//!
//! Getting started is fairly easy. Just slap
//! `#[easy_proc_macro::easy_proc_macro]` ontop of any old macro definition,
//! and boom, it becomes a procedural macro. Not much might have changed on the
//! surface, but your macro has just gained _a super power_.
//!
//! That super power being _token manipulation blocks_! Adding a token
//! manipulation block to your macro is pretty straight forward. Just write a
//! normal block with a dollar sign in front as if it was some special macro
//! syntax, like so; `${/* ... */}`. Code inside these blocks will be ran at
//! _compile time_, and operate upon Rust's token's directly, allowing your
//! macros to do a whole lot more than just advanced `Ctrl + C` + `Ctrl + V`.
//!
//! Tokens you access inside a token manipulation block will be a string
//! representation, allowing you to modify them to your heart's extent. The
//! value a token manipulation block resolves to must implement [`ToString`], or
//! be a `String` or `&str` (both implement `ToString`). The returned value is
//! what the token manipulation block will expand too.
//!
//! What? How?
//! ----------
//! You're probably wondering what makes `easy_proc_macro`'s magic. This macro
//! internally searches through your macro for token manipulation blocks, and
//! replaces those with a macro call to this crates `call_macro_code!`. Note
//! that `call_macro_code!` is an *implementation detail*, and cannot be relied
//! upon by any external crate, and therefor, no public documentation is
//! provided. Despite that, there is [some internal documentation][internal]
//! to how it works.
//!
//! After that, not much happens until your macro is expanded. Macros within
//! macros, and other macro definitions don't actually get expanded, so the
//! `call_macro_code!` macro call doesn't get expanded within your macro
//! definition.
//!
//! Once your macro is expanded at it's eventual call site, `call_macro_code!`
//! is then set to expand on the next macro expansion cycle. It's important to
//! note that at this stage, your macro definition has also expanded all the
//! arguments to `call_macro_code!`, and `call_macro_code!` only has to take
//! those token trees and run your code with them.
//!
//! Now, finally, `call_macro_code!` gets expanded, which internally reaches out
//! to `rustc` to compile a template Rust source file that has had all of the
//! macro arguments and code patched into it. These template Rust files can be
//! found at `src/templates/`. Once compiled, the executable is then ran, and
//! the completed token tree is passed through a TCP connection back to
//! `call_macro_code` (allowing you to have free access over stdin and stdout).
//! The token tree received over TCP is what `call_macro_code!` is expanded too.
//!
//! Remember this entire process is an implementation detail, and none of this
//! should be relied upon for code correctness.
//!
//! ### Security Issues?
//! > Isn't giving any crate I download the ability to run code on my computer
//! > at compile time a security issue?
//!
//! Well, yes, it is, as the compile time executed code can freely access all
//! your systems resources. *Regular old procedural macro compile plugins can do
//! the same thing too.* If you're concerned about security, you should already
//! be checking the normal procedural macros you use in your code.
//!
//! > Isn't transfering token trees over TCP a security issue?
//!
//! Yes, that is too, but provided you have a strong firewall, it should not be
//! open for abuse. If you're curious, the exact TCP port used is `32842`.
//!
//! Nightly Only Features
//! ---------------------
//! This crate took great care to be able to be compiled on the Rust stable
//! compiler, but there's some things that the Rust stable compiler just can't
//! provide. Hence why these features are only available on a nightly compiler
//! with the `nightly` feature enabled on this crate.
//!
//! ### Better Error Messages
//! Provides error messages that point at the actual incorrect code rather than
//! the procedural macro invokation.
//!
//! #### Stable
//! ![cannot borrow `token` as mutable, as it is not declared as mutable,
//! pointing towards `#[easy_proc_macro]`. in this macro invocation, pointing
//! towards `my_stringify!(value, 12)`.
//! ](https://love.catboys.space/he8PwZ9zcp.png)
//!
//! #### Nightly
//! ![cannot borrow `token` as mutable, as it is not declared as mutable,
//! pointing towards `token`. in this macro invocation, pointing towards
//! `my_stringify!(value, 12)`.](https://love.catboys.space/T1jfoVTVOb.png)
//!
//! Thanks to the [`proc_macro_diagnostic`] api.
//!
//! [`ToString`]: https://doc.rust-lang.org/std/string/trait.ToString.html
//! [`proc_macro_diagnostic`]: https://github.com/rust-lang/rust/issues/54140
//! [internal]: ../src/easy_proc_macro/lib.rs.html#251

#![cfg_attr(feature = "nightly", feature(decl_macro, proc_macro_diagnostic, proc_macro_span))]

use self::{
	syntax::{Arguments, CaptureType, MacroData, Rule},
	compile::basic_compile,
	util::error_tokens
};
use itertools::Itertools;
use proc_macro::TokenStream as Stream;
use proc_macro2::{
	Delimiter, Group, Ident, Punct, Spacing, Span, TokenTree, TokenStream
};
use quote::{ToTokens, quote};
use std::{io::Read, iter::Iterator, net::TcpListener, process::Command};
use syn::parse::{Parse, Parser};
use tempfile::tempdir;

mod compile;
mod syntax;
mod util;

/// It's like, the whole point, yeah?
///
/// Hooks onto a macro definition and super charges it with state of the art
/// procedural macro goodness. More information on this macro can be found on
/// the [crate level documentation](crate).
#[proc_macro_attribute]
pub fn easy_proc_macro(_: Stream, input: Stream) -> Stream {
	#[cfg(feature = "trace")]
	eprintln!("!!!!! #[easy_proc_macro] invoked with\n{}\n...", input.clone());

	let mut data = MacroData::parse.parse(input).expect("error");

	data.rules.iter_mut().for_each(|rule| {
		let Rule {matcher, transcriber} = rule;

		let captures = matcher.clone().into_iter()
			.tuple_windows::<(_, _, _, _)>()
			.filter_map(|values| match values {
				(
					TokenTree::Punct(a), TokenTree::Ident(b),
					TokenTree::Punct(c), TokenTree::Ident(d)
				) if a.to_string() == "$" && c.to_string() == ":" => Some((
					b.to_string().into_boxed_str(),
					CaptureType::from(&d.to_string())
				)),
				_ => None
			})
			.collect::<Vec<_>>();

		let mut tokens = Vec::new();
		for token in transcriber.clone().into_iter() {
			match (tokens.last(), &token) {
				(Some(TokenTree::Punct(punct)), TokenTree::Group(group))
						if punct.to_string() == "$" &&
							group.delimiter() == Delimiter::Brace => {
					tokens.pop();

					let code = group.stream();
					let captures = code.clone().into_iter()
						// TODO: Fix flat map nonsense...
						.flat_map(|token| match token {
							TokenTree::Group(group) => group.stream(),
							token => token.into()
						})
						.flat_map(|token| match token {
							TokenTree::Group(group) => group.stream(),
							token => token.into()
						})
						.flat_map(|token| match token {
							TokenTree::Group(group) => group.stream(),
							token => token.into()
						})
						.flat_map(|token| match token {
							TokenTree::Group(group) => group.stream(),
							token => token.into()
						})
						.flat_map(|token| match token {
							TokenTree::Group(group) => group.stream(),
							token => token.into()
						})
						.flat_map(|token| match token {
							TokenTree::Group(group) => group.stream(),
							token => token.into()
						})
						.flat_map(|token| match token {
							TokenTree::Group(group) => group.stream(),
							token => token.into()
						})
						.flat_map(|token| match token {
							TokenTree::Group(group) => group.stream(),
							token => token.into()
						})
						.flat_map(|token| match token {
							TokenTree::Group(group) => group.stream(),
							token => token.into()
						})
						.flat_map(|token| match token {
							TokenTree::Group(group) => group.stream(),
							token => token.into()
						})
						.tuple_windows::<(_, _)>()
						.filter_map(|values| match values {
							(TokenTree::Punct(a), TokenTree::Ident(b))
									if a.as_char() == '$' =>
								Some(b.to_string().into_boxed_str()),
							_ => None
						})
						.dedup()
						.map(|hole| captures.iter().find(|capture| hole == capture.0)
							.expect("error error!"))
						.map(|capture| vec![
							TokenTree::Group(Group::new(Delimiter::Parenthesis, vec![
								TokenTree::Punct(Punct::new('$', Spacing::Alone)),
								Ident::new(&*capture.0, Span::call_site()).into()
							].into_iter().collect())),
							Punct::new(':', Spacing::Alone).into(),
							Ident::new(&*capture.0, Span::call_site()).into(),
							Punct::new(':', Spacing::Alone).into(),
							Ident::new(capture.1.into_str(), Span::call_site()).into()
						].into_iter().collect::<TokenStream>())
						.collect::<TokenStream>();

					// TODO: Fix? Idk?
					fn map(input: TokenStream) -> TokenStream {
						input.into_iter().map(|token| match token {
							TokenTree::Group(group) =>
								Group::new(group.delimiter(), map(group.stream())).into(),
							token => token
						}).filter(|token| match token {
							TokenTree::Punct(punct) if punct.as_char() == '$' => false,
							_ => true
						}).collect()
					}

					let code = map(code);

					tokens.extend(quote! {
						::easy_proc_macro::call_macro_code!(
							#captures; {#code}
						)
					});
				},
				_ => tokens.push(token)
			}
		}

		*transcriber = tokens.into_iter().collect();
	});

	#[cfg(feature = "trace")]
	eprintln!("!!!!! that #[easy_proc_macro] call was expanded to\n{}\n...", data.to_token_stream());

	data.into_token_stream().into()
}

/// *How come [macro@easy_proc_macro] gets all the fame when I do all the
/// work?...*
///
/// This is internal documentation specific only to this version of
/// `easy_proc_macro`.
///
/// Syntax
/// ------
/// Where as a whole, it is syntax, except contents within angle brackets, where
/// the contents are literal english, except contents within parenthesis, where
/// the contents are syntax once more, the syntax is as follows...
/// `<"arguments" 0 or more ((<"argument N" 0 or more tokens of any type>):<"argument N name" 1 identifier>:<"argument N type" 1 identifier>)>;<"code" 0 or more tokens of any type>`
///
/// Within that syntax, the quotated names at the start of literal english
/// sections shall be the label of the tokens of which the section refers to.
///
/// - "code" is the code to be compiled and executed, within a template
/// - "argument N" is the contents of the Nth argument, and it as a string value
///   is what the corresponding "argument N name" is to be bound too, within
///   the scope of the code
/// - "argument N name" is the name of the Nth argument
/// - "argument N type" is currently unused, and may be any identifier
#[doc(hidden)]
#[proc_macro]
pub fn call_macro_code(input: Stream) -> Stream {
	#[cfg(feature = "trace")]
	eprintln!("!!!!! call_macro_code! invoked with\n{}\n...", input.clone());

	let arguments = Arguments::parse.parse(input).expect("error");

	let dir = tempdir().unwrap();
	let bin = dir.path().join("bin");

	match basic_compile(arguments, &bin) {
		Err(error) => return error_tokens(error).into(),
		Ok(()) => ()
	}

	let token_server = TcpListener::bind("[::1]:32842").expect("call_macro_code error");
	let mut plugin = Command::new(bin).spawn().expect("call_macro_code error");
	plugin.wait().expect("call_macro_code error");

	let mut socket = token_server.incoming().next().expect("call_macro_code error").expect("call_macro_code error");
	let mut tokens = String::new();
	socket.read_to_string(&mut tokens).expect("call_macro_code error");
	tokens.parse().expect("call_macro_code error")
}
