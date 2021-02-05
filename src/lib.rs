//! Write meta code with ease, no external crates required.

use self::syntax::{CaptureType, MacroData, Rule};
use itertools::Itertools;
use proc_macro::TokenStream as Stream;
use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenTree, TokenStream};
use quote::{ToTokens, quote};
use std::iter::Iterator;
use syn::parse::{Parse, Parser};

mod bridge;
mod syntax;

#[proc_macro_attribute]
pub fn easy_proc_macro(_: Stream, input: Stream) -> Stream {
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

	data.into_token_stream().into()
}

/// TODO: ugly code ew
#[doc(hidden)]
#[proc_macro]
pub fn call_macro_code(input: Stream) -> Stream {
	let args = crate::syntax::Arguments::parse.parse(input).expect("error");
	crate::bridge::run(args).parse().expect("error")
}
