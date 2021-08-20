use std::borrow::Cow;

use clap::{app_from_crate, crate_authors, crate_description, crate_name, crate_version, Arg};

use kirl_engine::{compile, KirlFileResolver};
use kirl_vm::vm::exec;

struct OsFileResolver;

impl KirlFileResolver for OsFileResolver {
    type ResolveError = std::io::Error;

    fn resolve_file_by_path(&mut self, path: &[String]) -> Result<Cow<str>, Self::ResolveError> {
        std::fs::read_to_string(path.join(&String::from(std::path::MAIN_SEPARATOR))).map(Cow::from)
    }
}

fn main() -> Result<(), anyhow::Error> {
    let matches = app_from_crate!().arg(Arg::with_name("entrypoint").help("kirl program file for entrypoint").takes_value(true).required(true)).get_matches();
    let entrypoint = matches.value_of("entrypoint").unwrap().to_string();
    let executable = compile(&mut OsFileResolver, &[entrypoint])?;
    exec(&executable);
    Ok(())
}
