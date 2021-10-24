use std::borrow::Cow;
use std::path::Path;

use clap::{app_from_crate, crate_authors, crate_description, crate_name, crate_version, Arg};

use kirl_engine::{compile, KirlFileResolver};
use kirl_vm::vm::exec;

struct OsFileResolver;

impl KirlFileResolver for OsFileResolver {
    type ResolveError = std::io::Error;

    fn resolve_file_by_path(&mut self, path: impl AsRef<Path>) -> Result<Cow<str>, Self::ResolveError> {
        std::fs::read_to_string(path).map(Cow::from)
    }
}

fn main() -> Result<(), String> {
    let matches = app_from_crate!().arg(Arg::with_name("entrypoint").help("kirl program file for entrypoint").takes_value(true).required(true)).get_matches();
    let entrypoint = matches.value_of("entrypoint").unwrap();
    match compile(&mut OsFileResolver, &entrypoint) {
        Ok(executable) => {
            exec(&executable);
            Ok(())
        }
        Err(err) => Err(format!("{}", err)),
    }
}
