fn main() {
	use std::{io::Write, net::TcpStream, process::exit};

	let result = code();

	let mut connection = match TcpStream::connect("@2@") {
		Ok(connection) => connection,
		Err(_) => exit(1)
	};

	match connection.write(result.to_string().as_bytes()) {
		Ok(_) => (),
		Err(_) => exit(2)
	}
}

fn code() -> impl ToString {
	#[allow(unused_parens)]
	@0@

	{
		@1@
	}
}
