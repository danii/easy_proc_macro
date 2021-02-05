use self::super::syntax::{Arguments, Argument, CaptureType};
use std::{io::{Read, Write}, path::{Path, PathBuf}, process::{Command, Stdio}};
use tempfile::tempdir;

pub fn run(arguments: Arguments) -> String {
	let work_dir = tempdir().expect("io error");

	let bin = rustc(arguments, work_dir.path());

	let mut command = Command::new(bin).stdout(Stdio::piped()).spawn()
		.expect("cmd error 1");

	let mut result = String::new();
	command.stdout.take().expect("cmd error 2")
		.read_to_string(&mut result).expect("io error");

	command.wait().expect("cmd error 3");

	result
}

fn rustc(arguments: Arguments, work_dir: &Path) -> PathBuf {
	let Arguments {token_arguments, code} = arguments;

	let token_arguments = token_arguments.into_iter().map(|argument| {
		let Argument {tokens, name, capture_type} = argument;

		let capture_type = match capture_type {
			CaptureType::Ident => "Ident",
			CaptureType::Literal => "Literal",
			CaptureType::Path => "Path",
			CaptureType::Type => "Type"
		};

		let tokens = tokens.to_string();

		format!("let {} = {}::new({:?});\n", name, capture_type, tokens)
	}).collect::<String>();

	let code = code.to_string();

	let code = format!(
		"pub struct Ident<'a> {{
			pub token: &'a str,
			_nonexhaustive: ()
		}}

		impl<'a> Ident<'a> {{
			pub fn new(token: &'a str) -> Self {{
				Self {{token, _nonexhaustive: ()}}
			}}
		}}

		impl<'a> std::fmt::Display for Ident<'a> {{
			fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {{
				write!(f, \"{{}}\", self.token)
			}}
		}}

		pub struct Literal<'a> {{
			pub token: &'a str,
			_nonexhaustive: ()
		}}

		impl<'a> Literal<'a> {{
			pub fn new(token: &'a str) -> Self {{
				Self {{token, _nonexhaustive: ()}}
			}}
		}}

		impl<'a> std::fmt::Display for Literal<'a> {{
			fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {{
				write!(f, \"{{}}\", self.token)
			}}
		}}

		pub struct Path<'a> {{
			pub token: &'a str,
			_nonexhaustive: ()
		}}

		impl<'a> Path<'a> {{
			pub fn new(token: &'a str) -> Self {{
				Self {{token, _nonexhaustive: ()}}
			}}
		}}

		impl<'a> std::fmt::Display for Path<'a> {{
			fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {{
				write!(f, \"{{}}\", self.token)
			}}
		}}

		pub struct Type<'a> {{
			pub token: &'a str,
			_nonexhaustive: ()
		}}

		impl<'a> Type<'a> {{
			pub fn new(token: &'a str) -> Self {{
				Self {{token, _nonexhaustive: ()}}
			}}
		}}

		impl<'a> std::fmt::Display for Type<'a> {{
			fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {{
				write!(f, \"{{}}\", self.token)
			}}
		}}

		fn main() {{
			{}

			print!(\"{{}}\", {{
				{}
			}});
		}}",
		token_arguments,
		code
	);

	let bin = work_dir.join("bin");
	let bin_str = bin.as_os_str().to_str().expect("fs error");

	let mut command = Command::new("rustc")
		.args(&["/dev/stdin", "--color", "always", "-o", bin_str])
		.stdin(Stdio::piped())
		.spawn().expect("cmd error 4");

	command.stdin.take().expect("cmd error 5")
		.write_all(code.as_bytes()).expect("io error");

	command.wait().expect("cmd error 6");

	bin
}
