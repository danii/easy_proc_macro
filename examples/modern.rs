#![feature(decl_macro)]
use easy_proc_macro::easy_proc_macro;

#[easy_proc_macro]
macro multiple {
	($lol:ident) => {
		${
			"\"a large vehicle\""
		}
	}
}

#[easy_proc_macro]
macro single($lol:ident) {
	${
		"\"a smooth criminal\""
	}
}

fn main() {
	println!(multiple!(xd));
}
