use self::super::{syntax::Arguments, util::{CompileError, LineColumnOffset, alternate, display_tokens, handle_lints}};
use itertools::Itertools;
use serde_json::Deserializer;
use std::{borrow::Cow, io::{Read, Write}, path::Path, process::{Command, Stdio}};

pub mod lints;

pub fn basic_compile(arguments: Arguments, output: &Path)
		-> Result<(), CompileError> {
	let Arguments {token_arguments, code: raw_code} = arguments;
	let (offset, code): (_, String) = {
		const TEMPLATE: &str = include_str!("templates/basic.rs");
		let (code, inserts) = alternate(TEMPLATE.split("@"));

		let code = code.map(|code| Cow::Borrowed(code));
		let inserts = inserts.map(|insert| match insert {
			"0" => {
				let names = token_arguments.iter()
					.map(|argument| &argument.name).join(", ");
				let values = token_arguments.iter()
					.map(|argument| format!("{:?}", argument.tokens.to_string()))
					.join(", ");

					Cow::Owned(format!("let ({}) = ({});", names, values))
			},
			"1" => {
				let mut code = String::new();
				display_tokens(raw_code.clone(), &mut code, &mut None);
				Cow::Owned(code)
			},
			"2" => Cow::Borrowed("[::1]:32842"),
			_ => unreachable!()
		});

		#[allow(unused_mut)]
		let mut offset: LineColumnOffset = code.clone().interleave(inserts.clone())
			.take(5).map(|item| LineColumnOffset::from_str(item)).sum();
		#[cfg(feature = "nightly")]
		{
			let file_offset: LineColumnOffset = raw_code.clone().into_iter()
				.next().expect("expected tok")
				.span().unwrap()
				.start().into();
			offset.column = file_offset.column;
		}

		(
			offset,
			code.interleave(inserts).collect()
		)
	};

	let mut compile = Command::new("rustc")
		.args(&["/dev/stdin", "--error-format=json", "-o"]).arg(output)
		.stdin(Stdio::piped()).stderr(Stdio::piped()).spawn().expect("cmd error");

	compile.stdin.take().expect("cmd error")
		.write_all(code.as_bytes()).expect("io error");
	let result = compile.wait().expect("cmd error");

	let mut errors = String::new();
	compile.stderr.take().expect("cmd error")
		.read_to_string(&mut errors).expect("io error");
	let errors = Deserializer::from_str(&errors)
		.into_iter().collect::<Result<_, _>>().expect("too lazy to handle errors rn");
	match result.success() {
		false => Err(handle_lints(errors, raw_code, offset)),
		true => {
			handle_lints(errors, raw_code, offset);
			Ok(())
		}
	}
}

#[cfg(feature = "local")]
pub fn local_compile() {
	
}
