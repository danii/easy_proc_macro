use quote::{ToTokens, quote};
use syn::{parse::{Parse, ParseStream, Parser}, punctuated::Punctuated, token::{Colon, FatArrow, Semi}, Ident, ItemMacro, ItemMacro2, Result};
use proc_macro2::{Delimiter, Group, Punct, Spacing, TokenStream, TokenTree};

fn unwrap_delimiter(input: ParseStream, delimiter: Delimiter)
		-> Result<TokenStream> {
	input.step(|cursor| {
		match cursor.token_tree() {
			Some((TokenTree::Group(group), cursor))
					if delimiter == group.delimiter() => {
				Ok((group.stream(), cursor))
			},
			_ => panic!()
		}
	})
}

fn unwrap_any_delimiter(input: ParseStream)
		-> Result<TokenStream> {
	input.step(|cursor| {
		match cursor.token_tree() {
			Some((TokenTree::Group(group), cursor))
					if Delimiter::None != group.delimiter() => {
				Ok((group.stream(), cursor))
			},
			_ => panic!()
		}
	})
}

pub struct MacroData {
	pub modern: bool,
	pub public: bool,
	pub name: Ident,
	pub rules: Vec<Rule>
}

impl ToTokens for MacroData {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let Self {modern, public, name, rules} = self;
		let bang = Punct::new('!', Spacing::Alone);

		tokens.extend(match (modern, public) {
			(false, false) => quote! {
				macro_rules#bang #name {
					#(#rules);*
				}
			},
			_ => todo!()
		});
	}
}

impl Parse for MacroData {
	fn parse(input: ParseStream) -> Result<Self> {
		match input.parse::<ItemMacro2>() {
			Ok(_) => {
				todo!("modern macros aren't supported yet")
			},
			Err(_) => {
				let item = input.parse::<ItemMacro>()?;
				let rules = Punctuated::<LegacyRule, Semi>::parse_terminated
					.parse2(item.mac.tokens)?;

				let name = item.ident.expect("bruh");
				let rules = rules.into_iter().map(|rule| rule.0).collect();
				let public = item.attrs.iter()
					.any(|attr| {
						attr.path.segments.last()
							.map(|path| path.ident.to_string() == "macro_export")
							.unwrap_or_default()
					});
				Ok(Self {modern: false, public, name, rules})
			}
		}
	}
}

#[derive(Debug)]
pub struct Rule {
	pub matcher: TokenStream,
	pub transcriber: TokenStream
}

impl ToTokens for Rule {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		tokens.extend(vec![
			TokenTree::Group(Group::new(Delimiter::Parenthesis, self.matcher.clone())),
			Punct::new('=', Spacing::Joint).into(),
			Punct::new('>', Spacing::Alone).into(),
			Group::new(Delimiter::Brace, self.transcriber.clone()).into()
		]);
	}
}

impl Rule {
	fn parse_legacy(input: ParseStream) -> Result<Self> {
		let matcher = unwrap_any_delimiter(input)?;
		input.parse::<FatArrow>()?;
		let transcriber = unwrap_any_delimiter(input)?;

		Ok(Self {matcher, transcriber})
	}
}

struct LegacyRule(Rule);

impl Parse for LegacyRule {
	fn parse(input: ParseStream) -> Result<Self> {
		Rule::parse_legacy(input).map(Self)
	}
}

pub struct Arguments {
	pub token_arguments: Vec<Argument>,
	pub code: TokenStream
}

impl Parse for Arguments {
	fn parse(input: ParseStream) -> Result<Self> {
		let mut token_arguments = Vec::new();
		while !input.peek(Semi) {
			token_arguments.push(input.parse()?);
		}

		input.parse::<Semi>()?;

		let code = unwrap_delimiter(input, Delimiter::Brace)?;

		Ok(Self {token_arguments, code})
	}
}

pub struct Argument {
	pub tokens: TokenStream,
	pub name: Box<str>,
	pub capture_type: CaptureType
}

impl Parse for Argument {
	fn parse(input: ParseStream) -> Result<Self> {
		let tokens = unwrap_delimiter(input, Delimiter::Parenthesis)?;

		input.parse::<Colon>()?;
		let name = input.parse::<Ident>()?.to_string().into_boxed_str();

		input.parse::<Colon>()?;
		let capture_type = input.parse()?;

		Ok(Self {tokens, name, capture_type})
	}
}

pub enum CaptureType {
	Ident,
	Literal,
	Path,
	Type
}

impl CaptureType {
	pub fn from(input: &str) -> Self {
		match input {
			"ident" => Self::Ident,
			"literal" => Self::Literal,
			"path" => Self::Path,
			"ty" => Self::Type,
			_ => panic!()
		}
	}

	pub fn into_str(&self) -> &'static str {
		match self {
			Self::Ident => "ident",
			Self::Literal => "literal",
			Self::Path => "path",
			Self::Type => "ty"
		}
	}
}

impl Parse for CaptureType {
	fn parse(input: ParseStream) -> Result<Self> {
		Ok(match &input.parse::<Ident>()?.to_string() as &str {
			"ident" => Self::Ident,
			"literal" => Self::Literal,
			"path" => Self::Path,
			"ty" => Self::Type,
			_ => panic!()
		})
	}
}
