use quote::{ToTokens, quote};
use syn::{
	parse::{Parse, ParseStream, Parser, discouraged::Speculative},
	punctuated::Punctuated, token::{Comma, Colon, FatArrow, Semi},
	Attribute, Error, Ident, ItemMacro, ItemMacro2, Result, Visibility
};
use proc_macro2::{Delimiter, Group, Punct, Spacing, TokenStream, TokenTree};

/// Steps the `input` into a group that is delimited by `delimiter`. Returns an
/// error if the next item in `input` does not match.
fn group_delimiter(input: ParseStream, delimiter: Delimiter)
		-> Result<TokenStream> {
	#[cfg(feature = "trace")]
	eprintln!(">>> parsing group_delimiter({}, {:?})", input.clone(), delimiter);

	input.step(|cursor| {
		match cursor.token_tree() {
			Some((TokenTree::Group(group), cursor))
					if delimiter == group.delimiter() =>
				Ok((group.stream(), cursor)),
			Some((TokenTree::Group(group), cursor))
					if Delimiter::None == group.delimiter() =>
				(|new: ParseStream| group_delimiter(new, delimiter))
					.parse2(group.stream()).map(|result| (result, cursor)),
			_ => Err(Error::new(input.span(),
				format!("expected the start of a {:?} group", delimiter))),
		}
	})
}

/// Steps the `input` into a group that is delimited by anything (except
/// [Delimiter::None]). Returns an error if the next item in `input` does not
/// match.
fn any_group_delimiter(input: ParseStream)
		-> Result<TokenStream> {
	#[cfg(feature = "trace")]
	eprintln!(">>> parsing any_group_delimiter({})", input.clone());

	input.step(|cursor| {
		match cursor.token_tree() {
			Some((TokenTree::Group(group), cursor)) => match group.delimiter() {
				Delimiter::None => (|new: ParseStream| any_group_delimiter(new))
					.parse2(group.stream()).map(|result| (result, cursor)),
				_ => Ok((group.stream(), cursor))
			},
			_ => Err(Error::new(input.span(), "expected the start of a group")),
		}
	})
}

/// Represents a macro, whether it be public or private, modern or legacy.
pub struct MacroData {
	pub r#type: MacroType,
	pub attrs: Vec<Attribute>,
	pub name: Ident,
	pub rules: Vec<Rule>
}

impl ToTokens for MacroData {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let Self {r#type, attrs, name, rules} = self;
		let bang = Punct::new('!', Spacing::Alone);
		// macro_rules! triggers rustc to register it even though it's in another
		// macro invocation and shouldn't be parsed... So we use #bang to get around
		// that.

		tokens.extend(match r#type {
			MacroType::Classic => quote! {
				#(#attrs)*
				macro_rules#bang #name {
					#(#rules);*
				}
			},
			MacroType::Modern(vis) => quote! {
				#(#attrs)*
				#vis macro #name {
					#(#rules),*
				}
			}
		});
	}
}

impl Parse for MacroData {
	fn parse(input: ParseStream) -> Result<Self> {
		// .fork() is required so that the ItemMacro parse doesn't get left out on
		// any attributes.
		let fork = input.fork();

		match fork.parse::<ItemMacro2>() {
			Ok(ItemMacro2 {attrs, vis, ident: name, rules, ..}) => {
				// Forward input to stop expected EOF error.
				input.advance_to(&fork);

				match (|rules: ParseStream| group_delimiter(rules, Delimiter::Brace))
						.parse2(rules.clone()) {
					Ok(tokens) => Ok(Self {
						r#type: MacroType::Modern(vis),
						rules: Rule::parse_multiple_modern.parse2(tokens)?
							.into_iter().collect(),
						attrs,
						name
					}),
					Err(_) => Ok(Self {
						r#type: MacroType::Modern(vis),
						rules: vec![Rule::parse_one.parse2(rules)?],
						attrs,
						name
					})
				}
			},
			Err(_) => {
				let ItemMacro {attrs, ident, mac, ..} = input.parse::<ItemMacro>()?;

				Ok(Self {
					r#type: MacroType::Classic,
					attrs,
					name: ident.expect("error"),
					rules: Rule::parse_multiple_legacy.parse2(mac.tokens)?
						.into_iter().collect()
				})
			}
		}
	}
}

pub enum MacroType {
	Classic,
	Modern(Visibility)
}

/// Represents one of a macro's pattern rules.
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
	/// Parses a modern macro's rules. As far as the rule goes, this function has
	/// the same semantics as [parse]. This function expects rules to be delimited
	/// by `,`s, the way modern macro rules are formed.
	///
	/// [parse]: Self::parse
	pub fn parse_multiple_modern(input: ParseStream)
			-> Result<Punctuated<Self, Comma>> {
		Punctuated::<Self, Comma>::parse_terminated_with(input, Self::parse)
	}

	/// Parses a legacy macro's rules. As far as the rule goes, this function has
	/// the same semantics as [parse]. This function expects rules to be delimited
	/// by `;`s, the way legacy macro rules are formed.
	///
	/// [parse]: Self::parse
	pub fn parse_multiple_legacy(input: ParseStream)
			-> Result<Punctuated<Self, Semi>> {
		Punctuated::<Self, Semi>::parse_terminated_with(input, Self::parse)
	}

	/// Parses a modern one pattern macro's rule. This does not parse a fat arrow,
	/// and expects the matcher to be delimited by parenthesis, and the
	/// transcriber to be delimited by braces. For rules of modern or legacy
	/// macros, [parse] should be used instead.
	///
	/// [parse]: Self::parse
	pub fn parse_one(input: ParseStream) -> Result<Self> {
		let matcher = group_delimiter(input, Delimiter::Parenthesis)?;
		let transcriber = group_delimiter(input, Delimiter::Brace)?;
		Ok(Self {matcher, transcriber})
	}

	/// Parses any rule, with any delimiter for it's matcher and transcriber
	/// groups. For rules of modern one pattern macros, [parse_one] should be used
	/// instead. This only parses one rule.
	///
	/// [parse_one]: Self::parse_one
	pub fn parse(input: ParseStream) -> Result<Self> {
		let matcher = any_group_delimiter(input)?;
		input.parse::<FatArrow>()?;
		let transcriber = any_group_delimiter(input)?;
		Ok(Self {matcher, transcriber})
	}
}

/// Represents the argument syntax passed to the [call_macro_code] macro.
///
/// [call_macro_code]: crate::call_macro_code!
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

		let code = group_delimiter(input, Delimiter::Brace)?;

		Ok(Self {token_arguments, code})
	}
}

/// Represents a single argument from a macro rule to be passed to the
/// [call_macro_code] macro.
///
/// [call_macro_code]: crate::call_macro_code!
pub struct Argument {
	pub tokens: TokenStream,
	pub name: Box<str>,
	pub capture_type: CaptureType
}

impl Parse for Argument {
	fn parse(input: ParseStream) -> Result<Self> {
		let tokens = group_delimiter(input, Delimiter::Parenthesis)?;

		input.parse::<Colon>()?;
		let name = input.parse::<Ident>()?.to_string().into_boxed_str();

		input.parse::<Colon>()?;
		let capture_type = input.parse()?;

		Ok(Self {tokens, name, capture_type})
	}
}

/// Represents the possible arguments a macro can match against.
#[non_exhaustive]
pub enum CaptureType {
	/// An `ident`.
	Ident,
	/// A `literal`.
	Literal,
	/// A `path`.
	Path,
	/// A `ty`.
	Ty
}

impl CaptureType {
	pub fn from(input: &str) -> Self {
		match input {
			"ident" => Self::Ident,
			"literal" => Self::Literal,
			"path" => Self::Path,
			"ty" => Self::Ty,
			_ => panic!()
		}
	}

	pub fn into_str(&self) -> &'static str {
		match self {
			Self::Ident => "ident",
			Self::Literal => "literal",
			Self::Path => "path",
			Self::Ty => "ty"
		}
	}
}

impl Parse for CaptureType {
	fn parse(input: ParseStream) -> Result<Self> {
		Ok(match &input.parse::<Ident>()?.to_string() as &str {
			"ident" => Self::Ident,
			"literal" => Self::Literal,
			"path" => Self::Path,
			"ty" => Self::Ty,
			_ => panic!()
		})
	}
}
