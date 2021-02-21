use itertools::Itertools;
use proc_macro2::{
	Delimiter, Group, Ident, Literal, Punct, Span, TokenStream, TokenTree
};
use std::{
	convert::TryFrom,
	hint::unreachable_unchecked,
	iter::{Filter, Sum},
	ops::{Add, Sub}
};

#[cfg(feature = "nightly")]
mod nightly {
	use self::super::{
		super::compile::lints::{Level, Lint, SpannedLint},
		LineColumnOffset
	};
	use proc_macro::LineColumn;
	use proc_macro2::{Delimiter, TokenStream, TokenTree};
	use quote::quote;
	use std::{fmt::Write, hint::unreachable_unchecked, ops::{Add, Sub}};

	pub type CompileError = ();

	pub fn handle_lints(lints: Vec<Lint>, code: TokenStream,
			offset: LineColumnOffset) -> CompileError {
		#[cfg(feature = "trace")]
		eprintln!(">>> handle_lints was called on nightly with {:?} as offset.\n{:#?}", offset, lints);

		lints.into_iter()
			.map(|lint| lint.to_spanned_lint(code.clone(), offset))
			.filter(|lint| lint.level != Level::FailureNote)
			.filter(|lint| !lint.message.starts_with("aborting due"))
			.for_each(SpannedLint::emit);
	}

	pub fn error_tokens(_: CompileError) -> TokenStream {
		quote! {}
	}

	pub fn display_tokens(tokens: TokenStream, writer: &mut impl Write,
			abs_last_end_raw: &mut Option<LineColumnOffset>) {
		#[cfg(feature = "trace")]
		eprintln!(">>> display_tokens was called on nightly.");

		//let writer_offset = *len;
		tokens.into_iter().fold(None, |mut stream_offset, token| {
			{
				let &mut stream_offset =
					stream_offset.get_or_insert_with(|| {
						let value = token.span().unwrap().start().into();
						abs_last_end_raw.get_or_insert(value);
						value
					});

				let abs_last_end = abs_last_end_raw
					.unwrap_or_else(|| unsafe {unreachable_unchecked()});

				match token {
					TokenTree::Group(token) => {
						let (open, close) = match token.delimiter() {
							Delimiter::Brace => ("{", "}"),
							Delimiter::Bracket => ("[", "]"),
							Delimiter::Parenthesis => ("(", ")"),
							Delimiter::None => ("", "")
						};

						#[cfg(feature = "trace")]
						eprintln!("> Entering {:?} group.", token.delimiter());
						write!(writer, "{}", open).expect("error #7");
						let LineColumnOffset {line, column: col} = abs_last_end;
						*abs_last_end_raw = Some(LineColumnOffset {line, column: col + 1});

						display_tokens(token.stream(), writer, abs_last_end_raw);

						#[cfg(feature = "trace")]
						eprintln!("> Leaving {:?} group.", token.delimiter());
						write!(writer, "{}", close).expect("error #7");
						let LineColumnOffset {line, column: col} = abs_last_end_raw
							.unwrap_or_else(|| unsafe {unreachable_unchecked()});
						*abs_last_end_raw = Some(LineColumnOffset {line, column: col + 1});
					},
					token => {
						#[cfg(feature = "trace")]
						eprintln!("> Token '{}' is being displayed.", token);
						let span = token.span().unwrap();
						let abs_beg = LineColumnOffset::from(span.start());
						let abs_end = LineColumnOffset::from(span.end());
						let abs_last_end = LineColumnOffset::from(abs_last_end);
						#[cfg(feature = "trace")]
						eprintln!("Absolute Beginning Of Token: {:?}", abs_beg);
						let beg = (abs_beg - stream_offset).expect("error #3");
						#[cfg(feature = "trace")]
						eprintln!("Absolute End Of Last Token: {:?}\nStream Offset: {:?}", abs_last_end, stream_offset);
						let last_end = (abs_last_end - stream_offset).expect("error #5");
						#[cfg(feature = "trace")]
						eprintln!("Beginning Of Token: {:?}\nEnd Of Last Token: {:?}", beg, last_end);
						let whitespace = (beg - last_end).expect("error #6");
						#[cfg(feature = "trace")]
						eprintln!("Calculated Whitespace: {:?}", whitespace);

						(0..whitespace.line).try_for_each(|_| writer.write_char('\n'))
							.expect("error #1");
						(0..whitespace.column).try_for_each(|_| writer.write_char(' '))
							.expect("error #2");
						write!(writer, "{}", token).expect("error #4");

						*abs_last_end_raw = Some(abs_end.into());
					}
				}
			}

			stream_offset
		});
	}

	impl Add<LineColumn> for LineColumnOffset {
		type Output = Self;

		fn add(self, rhs: LineColumn) -> Self {
			self + Self::from(rhs)
		}
	}

	impl Sub<LineColumn> for LineColumnOffset {
		type Output = Option<Self>;

		fn sub(self, rhs: LineColumn) -> Option<Self> {
			self - Self::from(rhs)
		}
	}

	impl From<LineColumn> for LineColumnOffset {
		fn from(raw: LineColumn) -> Self {
			let LineColumn {line, column} = raw;
			Self {line, column}
		}
	}

	impl From<LineColumnOffset> for LineColumn {
		fn from(offset: LineColumnOffset) -> Self {
			let LineColumnOffset {line, column} = offset;
			Self {line, column}
		}
	}
}

#[cfg(not(feature = "nightly"))]
mod stable {
	use self::super::{super::compile::lints::Lint, LineColumnOffset};
	use proc_macro2::{Span, TokenStream};
	use quote::quote_spanned;
	use std::fmt::Write;

	pub type CompileError = TokenStream;

	pub fn handle_lints(lints: Vec<Lint>, _: TokenStream,
			_: LineColumnOffset) -> CompileError {
		lints.into_iter()
			.map::<TokenStream, _>(|lint| {
				let message = lint.message;
				quote_spanned! {Span::call_site()=>
					compile_error!(#message)
				}
			})
			.collect()
	}

	pub fn error_tokens(error: CompileError) -> TokenStream {
		error
	}

	pub fn display_tokens(tokens: TokenStream, writer: &mut impl Write,
			_: &mut Option<LineColumnOffset>) {
		write!(writer, "{}", tokens).expect("error #9");
	}
}

#[cfg(feature = "nightly")]
pub use nightly::*;

#[cfg(not(feature = "nightly"))]
pub use stable::*;

#[derive(Clone, Copy, Debug)]
pub struct LineColumnOffset {
	pub line: usize,
	pub column: usize
}

impl LineColumnOffset {
	pub fn from_str(text: impl AsRef<str>) -> Self {
		text.as_ref().chars()
			.fold(Self::default(), |value, character| match character {
				'\n' => Self {line: value.line + 1, column: 0},
				_ => Self {line: value.line, column: value.column + 1}
			})
	}
}

impl Default for LineColumnOffset {
	fn default() -> Self {
		Self {line: 0, column: 0}
	}
}

impl Add for LineColumnOffset {
	type Output = Self;

	fn add(self, rhs: Self) -> Self {
		let Self {line: lline, column: lcol} = self;
		let Self {line: rline, column: rcol} = rhs;

		if lline == 0 || rline == 0 {
			Self {line: lline + rline, column: lcol + rcol}
		} else {
			Self {line: lline + rline, column: rcol}
		}
	}
}

impl Sum for LineColumnOffset {
	fn sum<I>(iterator: I) -> Self
			where I: Iterator<Item=Self> {
		iterator.fold(Self {line: 1, column: 0}, |acc, value| acc + value)
	}
}

impl Sub for LineColumnOffset {
	type Output = Option<Self>;

	fn sub(self, rhs: Self) -> Option<Self> {
		let Self {line: lline, column: lcol} = self;
		let Self {line: rline, column: rcol} = rhs;

		if lline > rline {
			Some(Self {
				line: lline - rline,
				column: lcol
			})
		} else {
			Some(Self {
				line: lline.checked_sub(rline)?,
				column: lcol.checked_sub(rcol)?
			})
		}
	}
}

/// Converts a singular iterator into two iterators.
///
/// The first (left) iterator yields every odd element, where the second (right)
/// iterator yields every even element.
pub fn alternate<I, T>(iterator: I)
		-> (impl Iterator<Item=T> + Clone, impl Iterator<Item=T> + Clone)
		where I: Iterator<Item=T> + Clone {
	let mut first_alternate = false;
	let mut second_alternate = true;

	(
		iterator.clone().filter(move |_| {
			first_alternate = !first_alternate;
			first_alternate
		}),
		iterator.filter(move |_| {
			second_alternate = !second_alternate;
			second_alternate
		})
	)
}

#[derive(Clone)]
pub enum FlatTokenTree {
	Opening(Delimiter, Span),
	Closing(Delimiter, Span),
	Ident(Ident),
	Punct(Punct),
	Literal(Literal)
}

impl TryFrom<TokenTree> for FlatTokenTree {
	type Error = Group;

	fn try_from(value: TokenTree) -> Result<Self, Group> {
		Ok(match value {
			TokenTree::Group(group) => return Err(group),
			TokenTree::Ident(ident) => Self::Ident(ident),
			TokenTree::Literal(literal) => Self::Literal(literal),
			TokenTree::Punct(punct) => Self::Punct(punct)
		})
	}
}

impl From<Ident> for FlatTokenTree {
	fn from(ident: Ident) -> Self {
		Self::Ident(ident)
	}
}

impl From<Literal> for FlatTokenTree {
	fn from(literal: Literal) -> Self {
		Self::Literal(literal)
	}
}

impl From<Punct> for FlatTokenTree {
	fn from(punct: Punct) -> Self {
		Self::Punct(punct)
	}
}

pub trait FlatTokenIterExt: Iterator<Item=FlatTokenTree> + Sized {
	fn expand(self) -> Result<TokenStream, Option<(Delimiter, Span)>>;
}

impl<T> FlatTokenIterExt for T
		where T: Iterator<Item=FlatTokenTree> {
	fn expand(self) -> Result<TokenStream, Option<(Delimiter, Span)>> {
		#[cfg_attr(not(feature = "nightly"), allow(unused_variables))]
		fn read_group(iter: &mut dyn Iterator<Item=FlatTokenTree>,
				delimiter: Delimiter, span_open: Span)
					-> Result<Group, Option<(Delimiter, Span)>> {
			let mut span_close = None;

			let stream = iter
				.batching(|iter| match iter.next()? {
					FlatTokenTree::Opening(delimiter, span) =>
						Some(read_group(iter, delimiter, span).map(TokenTree::Group)),
					FlatTokenTree::Closing(delimiter, span) =>
						Some(Err(Some((delimiter, span)))),
					FlatTokenTree::Ident(ident) =>
						Some(Ok(TokenTree::Ident(ident))),
					FlatTokenTree::Literal(literal) =>
						Some(Ok(TokenTree::Literal(literal))),
					FlatTokenTree::Punct(punct) =>
						Some(Ok(TokenTree::Punct(punct)))
				})
				.filter(|token| match token {
					Err(Some((closing, span))) if &delimiter == closing => {
						span_close = Some(*span);
						false
					},
					_ => true
				})
				.try_collect();

			match (stream, span_close) {
				#[cfg(feature = "nightly")]
				(Ok(stream), Some(span_close)) => {
					let mut group = Group::new(delimiter, stream);
					let span = span_open.unwrap().join(span_close.unwrap()).unwrap();
					group.set_span(span.into());
					Ok(group)
				},
				#[cfg(not(feature = "nightly"))]
				(Ok(stream), Some(_)) =>
					Ok(Group::new(delimiter, stream)),
				(Err(error), _) => Err(error),
				(_, None) => Err(None)
			}
		}

		self
			.into_iter()
			.batching(|iter| match iter.next()? {
				FlatTokenTree::Opening(delimiter, span) =>
					Some(read_group(iter, delimiter, span).map(TokenTree::Group)),
				FlatTokenTree::Closing(delimiter, span) =>
					Some(Err(Some((delimiter, span)))),
				FlatTokenTree::Ident(ident) =>
					Some(Ok(TokenTree::Ident(ident))),
				FlatTokenTree::Literal(literal) =>
					Some(Ok(TokenTree::Literal(literal))),
				FlatTokenTree::Punct(punct) =>
					Some(Ok(TokenTree::Punct(punct)))
			})
			.try_collect()
	}
}

pub trait TokenStreamExt: IntoIterator<Item=TokenTree> + Sized {
	// GATs when?
	// A lot of this boilerplate could've been mitigated with GATs...

	fn tree_find<P>(self, predicate: &mut P) -> Option<TokenTree>
		where P: FnMut(&TokenTree) -> bool;
	fn tree_filter<P>(self, predicate: &mut P)
		-> Filter<Flatten<<Self as IntoIterator>::IntoIter>, &mut P>
			where P: FnMut(&FlatTokenTree) -> bool;
	fn tree_flatten(self)
		-> Flatten<<Self as IntoIterator>::IntoIter>;
}

impl<T> TokenStreamExt for T
		where T: IntoIterator<Item=TokenTree>, T::IntoIter: 'static {
	fn tree_find<P>(self, predicate: &mut P) -> Option<TokenTree>
			where P: FnMut(&TokenTree) -> bool {
		self.into_iter().fold(None, |result, token| match (result, token) {
			(Some(result), _) => Some(result),
			(_, TokenTree::Group(group)) =>
				group.stream().tree_find(predicate),
			(_, token) => match predicate(&token) {
				true => Some(token),
				false => None
			}
		})
	}

	fn tree_filter<P>(self, predicate: &mut P)
			-> Filter<Flatten<<Self as IntoIterator>::IntoIter>, &mut P>
				where P: FnMut(&FlatTokenTree) -> bool {
		self
			.tree_flatten()
			.filter(predicate)
	}

	fn tree_flatten(self) -> Flatten<<Self as IntoIterator>::IntoIter> {
		Flatten::new(self.into_iter())
	}
}

pub struct Flatten<I> {
	iter: I,
	group: Option<(
		Box<Flatten<<TokenStream as IntoIterator>::IntoIter>>,
		Group
	)>
}

impl<I> Flatten<I> {
	fn new(iter: I) -> Self {
		Self {iter, group: None}
	}
}

impl<I> Iterator for Flatten<I>
		where I: Iterator<Item=TokenTree> {
	type Item = FlatTokenTree;

	fn next(&mut self) -> Option<FlatTokenTree> {
		Some(match &mut self.group {
			Some((inner, _)) => inner.next().unwrap_or_else(|| {
				// SAFETY: We just checked this was Some.
				let (_, group) = self.group.as_mut()
					.unwrap_or_else(|| unsafe {unreachable_unchecked()});

				let (delimiter, span_close) = (group.delimiter(), group.span_close());
				self.group = None;
				FlatTokenTree::Closing(delimiter, span_close)
			}),
			None => match self.iter.next() {
				Some(token) => match FlatTokenTree::try_from(token) {
					Ok(token) => token,
					Err(group) => {
						let iter = Box::new(Flatten::new(group.stream().into_iter()));

						let (delimiter, span_open) = (group.delimiter(), group.span_open());
						self.group = Some((iter, group));
						FlatTokenTree::Opening(delimiter, span_open)
					}
				}
				None => return None
			}
		})
	}
}
