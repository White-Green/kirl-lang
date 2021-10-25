use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::path::{Path, PathBuf};

use uuid::Uuid;

use kirl_parser::kirl_parser::Function;
use kirl_parser::{KirlParseError, KirlParser};
use kirl_semantic_analyzer::name_resolver::{resolve_statements, statement_references, KirlNameResolver};
use kirl_semantic_analyzer::syntax_tree_to_hir::{analysis_function, analysis_statements, AnalysisStatementError, SearchPaths};
use kirl_semantic_analyzer::type_checker::{decision_type, used_functions, DecisionTypeError};
use kirl_semantic_analyzer::{collect_top_level_item_with_imports, HIRStatement, HIRType, KirlTopLevelItems, WithImport};
use kirl_stdlib::get_stdlib;
use kirl_vm::bytecode::KirlVMExecutable;
use kirl_vm::lir::{hir_to_lir, LIRStatementList, LIRStatementListConvertError};

pub trait KirlFileResolver {
    type ResolveError: Error + Send + Sync + 'static;
    fn resolve_file_by_path(&mut self, path: impl AsRef<Path>) -> Result<Cow<str>, Self::ResolveError>;
}

#[derive(Debug)]
pub enum KirlCompileError<F: Error + Send + Sync + 'static> {
    ParseError(KirlParseError),
    DecisionTypeError(DecisionTypeError),
    LIRConvertError(LIRStatementListConvertError),
    AnalysisError(AnalysisStatementError),
    FileResolveError(F),
}

impl<F: Error + Send + Sync + 'static> From<KirlParseError> for KirlCompileError<F> {
    fn from(e: KirlParseError) -> Self {
        KirlCompileError::ParseError(e)
    }
}

impl<F: Error + Send + Sync + 'static> From<DecisionTypeError> for KirlCompileError<F> {
    fn from(e: DecisionTypeError) -> Self {
        KirlCompileError::DecisionTypeError(e)
    }
}

impl<F: Error + Send + Sync + 'static> From<LIRStatementListConvertError> for KirlCompileError<F> {
    fn from(e: LIRStatementListConvertError) -> Self {
        KirlCompileError::LIRConvertError(e)
    }
}

impl<F: Error + Send + Sync + 'static> From<AnalysisStatementError> for KirlCompileError<F> {
    fn from(e: AnalysisStatementError) -> Self {
        KirlCompileError::AnalysisError(e)
    }
}

impl<F: Error + Send + Sync + 'static> Display for KirlCompileError<F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KirlCompileError::ParseError(e) => Display::fmt(e, f),
            KirlCompileError::DecisionTypeError(e) => Display::fmt(e, f),
            KirlCompileError::LIRConvertError(e) => Display::fmt(e, f),
            KirlCompileError::AnalysisError(e) => Display::fmt(e, f),
            KirlCompileError::FileResolveError(e) => Display::fmt(e, f),
        }
    }
}

impl<F: Error + Send + Sync + 'static> Error for KirlCompileError<F> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            KirlCompileError::ParseError(e) => Some(e),
            KirlCompileError::DecisionTypeError(e) => Some(e),
            KirlCompileError::LIRConvertError(e) => Some(e),
            KirlCompileError::AnalysisError(e) => Some(e),
            KirlCompileError::FileResolveError(e) => Some(e),
        }
    }
}

struct InFileResolver<'a> {
    function_types: &'a HashMap<(PathBuf, String), Vec<(Uuid, HIRType)>>,
    base_file_path: &'a Path,
}

impl<'a> KirlNameResolver for InFileResolver<'a> {
    fn resolve(&mut self, full_path: &[String]) -> Vec<(Uuid, HIRType)> {
        let InFileResolver { function_types, base_file_path } = self;
        let key = match full_path {
            [] => unreachable!(),
            [name] => {
                let file_path = base_file_path.to_path_buf();
                (file_path, name.clone())
            }
            [path @ .., name] => {
                let mut file_path = base_file_path.to_path_buf();
                file_path.pop();
                file_path.extend(path);
                let file_path = file_path.with_extension("kirl");
                (file_path, name.clone())
            }
        };
        function_types.get(&key).into_iter().flatten().map(|(id, ty)| (*id, ty.clone())).collect()
    }
}

fn resolve<F: KirlFileResolver>(
    parser: &KirlParser,
    hir: Vec<HIRStatement<SearchPaths>>,
    hir_type: HIRType,
    hir_path: &impl AsRef<Path>,
    file_resolver: &mut F,
    parsed_files: &mut HashSet<PathBuf>,
    mut static_resolver: &mut impl KirlNameResolver,
    function_types: &mut HashMap<(PathBuf, String), Vec<(Uuid, HIRType)>>,
    parsed_functions: &mut HashMap<Uuid, Vec<HIRStatement<SearchPaths>>>,
    loaded_functions: &mut HashMap<Uuid, LIRStatementList>,
) -> Result<Vec<HIRStatement<(Uuid, HIRType)>>, KirlCompileError<F::ResolveError>> {
    let current_references = statement_references(&hir);
    for reference_path in current_references {
        match reference_path {
            [] => unreachable!(),
            [_] => {}
            [reference_file_path @ .., _] => {
                let mut file_path = hir_path.as_ref().to_path_buf();
                file_path.pop();
                file_path.extend(reference_file_path);
                let file_path = file_path.with_extension("kirl");
                if !parsed_files.contains(&file_path) {
                    let file = match file_resolver.resolve_file_by_path(&file_path) {
                        Ok(file) => file,
                        Err(_) => continue,
                    };
                    let KirlTopLevelItems { functions, .. } = collect_top_level_item_with_imports(parser.parse(&file)?);
                    for function in functions {
                        add_parsed_function(function_types, parsed_functions, &file_path, function)?;
                    }
                }
            }
        }
    }
    let loaded = resolve_statements(hir, &mut (InFileResolver { function_types, base_file_path: hir_path.as_ref() }, &mut static_resolver));
    let (argument_types, return_type) = if let HIRType::Function { arguments, result } = &hir_type { (arguments.clone(), result.deref().clone()) } else { unreachable!() };
    let loaded = decision_type(loaded, argument_types, return_type)?;
    let used_function_id = used_functions(&loaded);
    let next_resolve = function_types
        .iter()
        .flat_map(|((path, _), value)| {
            let mut result = Vec::new();
            for (id, ty) in value {
                if used_function_id.contains(id) {
                    if let Some(body) = parsed_functions.remove(id) {
                        result.push((*id, body, ty.clone(), path.to_path_buf()));
                    }
                }
            }
            result
        })
        .collect::<Vec<_>>();
    for (id, statements, ty, path) in next_resolve {
        let argument_count = if let HIRType::Function { arguments, .. } = &ty { arguments.len() } else { unreachable!() };
        let statements = resolve(parser, statements, ty, &path, file_resolver, parsed_files, static_resolver, function_types, parsed_functions, loaded_functions)?;
        loaded_functions.insert(id, hir_to_lir(statements, argument_count)?);
    }
    Ok(loaded)
}

fn add_parsed_function<F: Error + Send + Sync + 'static>(function_types: &mut HashMap<(PathBuf, String), Vec<(Uuid, HIRType)>>, parsed_functions: &mut HashMap<Uuid, Vec<HIRStatement<SearchPaths>>>, file_path: &Path, function: WithImport<Function>) -> Result<(), KirlCompileError<F>> {
    let function_name = function.item.name.clone();
    let (function_body, argument, result) = analysis_function(function)?;
    let id = Uuid::new_v4();
    function_types.entry((file_path.to_path_buf(), function_name)).or_default().push((id, HIRType::Function { arguments: argument, result: Box::new(result) }));
    parsed_functions.insert(id, function_body);
    Ok(())
}

pub fn compile<F: KirlFileResolver>(file_resolver: &mut F, entry_point: impl AsRef<Path>) -> Result<KirlVMExecutable, KirlCompileError<F::ResolveError>> {
    let stdlib = get_stdlib();
    let mut resolver = HashMap::new();
    resolver.insert("std".to_string(), stdlib);
    let parser = KirlParser::new();
    let entry_point_body = file_resolver.resolve_file_by_path(&entry_point).map_err(KirlCompileError::FileResolveError)?;
    let syntax_tree = parser.parse(&entry_point_body)?;
    let KirlTopLevelItems { statements, functions, .. } = collect_top_level_item_with_imports(syntax_tree);
    let hir = analysis_statements(statements)?;
    let mut parsed_files = HashSet::new();
    let mut function_types = HashMap::new();
    let mut parsed_functions = HashMap::new();
    let mut loaded_functions = HashMap::new();
    for function in functions {
        add_parsed_function(&mut function_types, &mut parsed_functions, &entry_point.as_ref().to_path_buf(), function)?;
    }
    let hir = resolve(
        &parser,
        hir,
        HIRType::Function {
            arguments: Vec::new(),
            result: Box::new(HIRType::Or(vec![HIRType::Tuple(Vec::new()), HIRType::Named { path: vec!["Number".to_string()], generics_arguments: Vec::new() }])),
        },
        &entry_point,
        file_resolver,
        &mut parsed_files,
        &mut resolver,
        &mut function_types,
        &mut parsed_functions,
        &mut loaded_functions,
    )?;
    let lir = hir_to_lir(hir, 0)?;
    loaded_functions.insert(Uuid::nil(), lir);
    Ok(KirlVMExecutable::new(loaded_functions.into_iter().map(|(id, code)| (id, code.0)), stdlib.static_values(), stdlib.functions(), Uuid::nil()))
}
