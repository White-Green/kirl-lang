use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;
use std::ops::DerefMut;

use uuid::Uuid;

use kirl_parser::KirlParser;
use kirl_semantic_analyzer::name_resolver::resolve_statements;
use kirl_semantic_analyzer::syntax_tree_to_hir::analysis_statements;
use kirl_semantic_analyzer::type_checker::decision_type;
use kirl_semantic_analyzer::{collect_top_level_item_with_imports, HIRStatementList, KirlTopLevelItems};
use kirl_stdlib::get_stdlib;
use kirl_vm::bytecode::KirlVMExecutable;
use kirl_vm::lir::LIRStatementList;

pub trait KirlFileResolver {
    type ResolveError: Error + Send + Sync + 'static;
    fn resolve_file_by_path(&mut self, path: &[String]) -> Result<Cow<str>, Self::ResolveError>; //TODO:このpathはimpl AsRef<Path>とかにしたほうが便利そう(ファイルの読み込み用だし)
}

pub fn compile(file_resolver: &mut impl KirlFileResolver, entry_point: &[String]) -> Result<KirlVMExecutable, anyhow::Error> {
    let mut stdlib = get_stdlib().lock().unwrap();
    let mut resolver = HashMap::new();
    resolver.insert("std".to_string(), stdlib.deref_mut());
    let parser = KirlParser::new();
    let entry_point = file_resolver.resolve_file_by_path(entry_point)?;
    let syntax_tree = parser.parse(&entry_point)?;
    let KirlTopLevelItems { statements, .. } = collect_top_level_item_with_imports(syntax_tree); //TODO:
    let hir = analysis_statements(statements)?;
    let hir = resolve_statements(hir, &mut resolver);
    drop(stdlib);
    let hir = decision_type(hir)?;
    let hir = HIRStatementList::from(hir);
    let lir = LIRStatementList::try_from(hir)?.0;
    Ok(KirlVMExecutable::new(Some((Uuid::nil(), lir)), None, &*get_stdlib().lock().unwrap(), Uuid::nil()))
}
