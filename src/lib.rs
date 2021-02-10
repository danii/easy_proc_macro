//! Write meta code with ease, no external crates required.

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

/// TODO: ugly code ew
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
