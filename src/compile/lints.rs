use self::super::super::util::LineColumnOffset;
#[cfg(feature = "nightly")]
use self::super::super::util::TokenStreamExt;
use serde::{
	de::{Deserializer, Error, IgnoredAny, MapAccess, Visitor},
	Deserialize
};
use std::{
	cmp::{Ord, Ordering, PartialEq, PartialOrd},
	fmt::{Formatter, Result as FormatResult}
};
use proc_macro::{TokenStream as Stream, TokenTree};
#[cfg(feature = "nightly")]
use proc_macro::{Diagnostic, Level as ProcMacroLevel};
#[cfg(feature = "nightly")]
use proc_macro2::{TokenStream, Span};

#[derive(Debug, Deserialize)]
pub struct Lint<'d> {
	pub message: &'d str,
	#[serde(default, deserialize_with = "deserialize_code")]
	pub code: Option<&'d str>,
	#[serde(borrow)]
	pub level: Level<'d>,
	pub spans: Vec<RawSpan<'d>>,
	pub children: Vec<Lint<'d>>
}

#[allow(dead_code)]
fn token_stream_find(code: Stream,
		predicate: &mut dyn FnMut(&TokenTree) -> bool) -> Option<TokenTree> {
	code.into_iter().fold(None, |result, token| match (result, token) {
		(Some(result), _) => Some(result),
		(_, TokenTree::Group(group)) =>
			token_stream_find(group.stream(), predicate),
		(_, token) => match predicate(&token) {
			true => Some(token),
			false => None
		}
	})
}

impl<'d> Lint<'d> {
	#[cfg(feature = "nightly")]
	pub fn to_spanned_lint(self, code: TokenStream,
			lint_offset: LineColumnOffset) -> SpannedLint<'d> {
		use itertools::{Either, Itertools};
		use std::hint::unreachable_unchecked;

		#[cfg(feature = "trace")]
		eprintln!(">>> to_spanned_lint was called.");

		let mut code_offset = None;
		let Self {message, code: error_code, level, spans, children} = self;

		let (spans, raw_spans) = spans.into_iter()
			.partition_map::<Vec<Option<Span>>, Vec<_>, _, _, _>(|raw_span| {
				#[cfg(feature = "trace")]
				eprintln!("> Finding a proper span for raw_span ({:?}..{:?})...\n\traw_span: {:?}", (raw_span.start() - lint_offset).expect("trace error"), (raw_span.end() - lint_offset).expect("trace error"), raw_span);
				let start = match code.clone().tree_find(&mut |token| {
					let span: LineColumnOffset = token.span().unwrap().start().into();
					let &mut code_offset = code_offset.get_or_insert(span);
					let span = (span - code_offset).expect("error #12");
					let mut raw_span = (raw_span.start() - lint_offset)
						.expect("error #10");

					// This is required. Why? I don't know.
					if raw_span.line != 0 {raw_span.column = raw_span.column - 1}

					#[cfg(feature = "trace")]
					eprintln!("Checking if '{}' starts ({:?}) at the required location...", token, span);
					raw_span.line == span.line && raw_span.column == span.column
				}) {
					Some(start) => start,
					None => return Either::Right(raw_span)
				};

				#[cfg(feature = "trace")]
				eprintln!("> '{}' starts at the required location!", start);
				let end = match code.clone().tree_find(&mut |token| {
					let span: LineColumnOffset = token.span().unwrap().end().into();
					// SAFETY: If there are no tokens, this never gets ran, and if there
					// are, the previous find should have set it.
					let &code_offset = code_offset.as_ref()
						.unwrap_or_else(|| unsafe {unreachable_unchecked()});
					let span = (span - code_offset).expect("error #13");
					let mut raw_span = (raw_span.end() - lint_offset)
						.expect("error #11");

					// This is required. Why? I don't know.
					if raw_span.line != 0 {raw_span.column = raw_span.column - 1}

					#[cfg(feature = "trace")]
					eprintln!("Checking if '{}' ends ({:?}) at the required location...", token, span);
					raw_span.line == span.line && raw_span.column == span.column
				}) {
					Some(end) => end,
					None => return Either::Right(raw_span)
				};

				#[cfg(feature = "trace")]
				eprintln!("> '{}' ends at the required location!\nCombining those locations into one span...", end);
				Either::Left(start.span().join(end.span()).map(Span::from))
			});

		SpannedLint {
			message,
			code: error_code,
			level,
			raw_spans,
			spans: spans.into_iter().collect::<Option<Vec<_>>>()
				.expect("bad spans?"),
			children: children.into_iter()
				.map(|child| child.to_spanned_lint(code.clone(), lint_offset))
					.collect()
		}
	}
}

#[derive(Debug)]
#[cfg(feature = "nightly")]
pub struct SpannedLint<'d> {
	pub message: &'d str,
	pub code: Option<&'d str>,
	pub level: Level<'d>,
	pub spans: Vec<Span>,
	pub raw_spans: Vec<RawSpan<'d>>,
	pub children: Vec<SpannedLint<'d>>
}

#[cfg(feature = "nightly")]
impl SpannedLint<'_> {
	pub fn emit(self) {
		let Self {message, code, level, spans, raw_spans, ..} = self;
		let spans: Vec<_> = spans.into_iter().map(Span::unwrap).collect();

		let lint = Diagnostic::spanned(spans, level.into(), message);

		let lint = if raw_spans.len() != 0 {
			#[cfg(feature = "trace")]
			eprintln!("==== List of spans that this lint could not display... ====\n{:#?}\n==== The full trace of the span finder can be found above this block. The lint is as follows. ====", raw_spans);

			lint.warning("parts of this error message could not be displayed\nthis is a bug. file a bug report of this output with the \"trace\" feature enabled\n(keep in mind that trace outputs can be very large)")
		} else {
			lint
		};

		let lint = if let Some("E0425") = code {
			lint.note("items outside of a compile time code block cannot be used within them\nthis functionality will be implemented soon")
		} else {
			lint
		};

		lint.emit();
	}
}

#[derive(Debug, Deserialize, Eq, PartialEq)]
pub struct RawSpan<'d> {
	pub file_name: &'d str,
	pub line_start: usize,
	pub line_end: usize,
	pub column_start: usize,
	pub column_end: usize,
	pub is_primary: bool,
	pub label: Option<&'d str>
}

impl RawSpan<'_> {
	#[allow(dead_code)]
	pub fn start(&self) -> LineColumnOffset {
		LineColumnOffset {line: self.line_start, column: self.column_start}
	}

	#[allow(dead_code)]
	pub fn end(&self) -> LineColumnOffset {
		LineColumnOffset {line: self.line_end, column: self.column_end}
	}
}

impl PartialOrd<Self> for RawSpan<'_> {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.line_start.cmp(&other.line_start)
			.then(self.column_start.cmp(&other.column_start))
			.then(self.line_end.cmp(&other.line_end))
			.then(self.column_end.cmp(&other.line_end)))
	}
}

impl Ord for RawSpan<'_> {
	fn cmp(&self, other: &Self) -> Ordering {
		self.line_start.cmp(&other.line_start)
			.then(self.column_start.cmp(&other.column_start))
			.then(self.line_end.cmp(&other.line_end))
			.then(self.column_end.cmp(&other.line_end))
	}
}

#[derive(Debug, Eq, PartialEq)]
pub enum Level<'d> {
	Error,
	Warning,
	Note,
	Help,
	FailureNote,
	Other(&'d str)
}

#[cfg(feature = "nightly")]
impl From<Level<'_>> for ProcMacroLevel {
	fn from(level: Level<'_>) -> Self {
		match level {
			Level::Error => Self::Error,
			Level::FailureNote => Self::Error,
			Level::Warning => Self::Warning,
			Level::Other(_) => Self::Warning,
			Level::Note => Self::Note,
			Level::Help => Self::Help
		}
	}
}

impl<'de, 'd> Deserialize<'de> for Level<'d>
		where 'de: 'd {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
			where D: Deserializer<'de> {
		struct BorrowedStringVisitor;

		impl<'de> Visitor<'de> for BorrowedStringVisitor {
			type Value = &'de str;

			fn expecting(&self, formatter: &mut Formatter) -> FormatResult {
				write!(formatter, "a borrowed string")
			}

			fn visit_borrowed_str<E>(self, string: &'de str) -> Result<&'de str, E>
					where E: Error {
				Ok(string)
			}
		}

		let value = deserializer.deserialize_str(BorrowedStringVisitor)?;
		Ok(match value {
			"error" => Self::Error,
			"warning" => Self::Warning,
			"note" => Self::Note,
			"help" => Self::Help,
			"failure-note" => Self::FailureNote,
			value => Self::Other(value)
		})
	}
}

fn deserialize_code<'de, D>(deserializer: D)
		-> Result<Option<&'de str>, D::Error> where D: Deserializer<'de> {
	struct CodeVisitor;
	
	impl<'de> Visitor<'de> for CodeVisitor {
		type Value = Option<&'de str>;
	
		fn expecting(&self, formatter: &mut Formatter) -> FormatResult {
			write!(formatter, "a map of code")
		}
	
		fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
				where A: MapAccess<'de>, {
			loop {
				match map.next_key()? {
					Some("code") => break {
						let value = map.next_value()?;

						loop {
							match map.next_key()? {
								Some(IgnoredAny) => drop(map.next_value::<IgnoredAny>()?),
								None => break Ok(Some(value))
							}
						}
					},
					None => break Err(A::Error::custom("missing code key-value")),
					_ => drop(map.next_value::<IgnoredAny>()?)
				}
			}
		}

		fn visit_none<E>(self) -> Result<Self::Value, E>
				where E: Error {
			Ok(None)
		}

		fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
				where D: Deserializer<'de> {
			deserializer.deserialize_map(self)
		}
	}

	deserializer.deserialize_option(CodeVisitor)
}
