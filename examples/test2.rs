use easy_proc_macro::easy_proc_macro;

#[easy_proc_macro]
macro_rules! hygine {
	() => {
		${
			"let hygenic = false;"
		}
	};

	($specified:ident) => {
		${
			format!("let {} = true;", $specified)
		}
	}
}

fn main() {
	let hygenic = true;
	hygine!();
	println!("Hygenic is {:?}.", hygenic);

	let pass_through = false;
	hygine!(pass_through);
	println!("Pass through is {:?}.", pass_through);
}
