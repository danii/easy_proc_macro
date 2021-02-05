use easy_proc_macro::easy_proc_macro;

#[easy_proc_macro]
macro_rules! my_stringify {
	($name:ident, $data:literal) => {
		let $name = ${
			let mut token = $data.token.to_owned();
			token.push_str("... Test!");
			Literal::new(&format!("{:?}", token))
		};
	}
}

fn main() {
	my_stringify!(value, 12);
	println!("{:?}", value);
}
