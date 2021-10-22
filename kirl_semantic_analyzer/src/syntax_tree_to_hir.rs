
use std::collections::{BTreeMap, HashSet};
use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::fmt::{Display, Formatter};

use kirl_parser::kirl_parser::{Block, Condition, ConstructStruct, Expression, ExpressionItem, ForStatement, Function, If, ImportPath, LetBinding, Path, Pattern, Statement, StatementItem, WhileStatement};

use crate::{HIRExpression, HIRStatement, HIRType, HIRTypeConvertError, Immediate, ReferenceAccess, StatementReachable, Variable, WithImport};

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
pub struct SearchPaths(pub(crate) Vec<Vec<String>>);

#[derive(Debug)]
pub enum AnalysisStatementError {
    UnImplementedFeature(&'static str),
    TypeConvertError(HIRTypeConvertError),
    DuplicatedMember(String),
    CollisionArgumentName(usize, usize),
}

impl From<HIRTypeConvertError> for AnalysisStatementError {
    fn from(e: HIRTypeConvertError) -> Self {
        AnalysisStatementError::TypeConvertError(e)
    }
}

impl Display for AnalysisStatementError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AnalysisStatementError::UnImplementedFeature(feature) => write!(f, "feature {:?} is not implemented yet.", feature),
            AnalysisStatementError::TypeConvertError(e) => e.fmt(f),
            AnalysisStatementError::DuplicatedMember(name) => write!(f, "member {:?} is duplicated", name),
            AnalysisStatementError::CollisionArgumentName(a, b) => write!(f, "{}th and {}th argument have same name.", a, b),
        }
    }
}

impl Error for AnalysisStatementError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            AnalysisStatementError::TypeConvertError(e) => Some(e),
            _ => None,
        }
    }
}

type AnalysisStatementResult<T> = Result<T, AnalysisStatementError>;

fn get_candidate_paths(path: Vec<String>, imports: &BTreeMap<String, HashSet<Vec<String>>>) -> SearchPaths {
    SearchPaths(Some(path.clone()).into_iter().chain(imports.get(path.first().unwrap()).into_iter().flatten().map(|base| base.iter().chain(path.iter().skip(1)).cloned().collect())).collect())
}

pub fn analysis_function(
    WithImport {
        import,
        item: Function { position, arguments, return_type, expression, .. },
    }: WithImport<Function>,
) -> AnalysisStatementResult<(Vec<HIRStatement<SearchPaths>>, Vec<HIRType>, HIRType)> {
    let (names, types): (Vec<_>, Vec<_>) = arguments.into_iter().unzip();
    Ok((analysis(vec![Statement { position, statement: StatementItem::Return(Some(expression)) }], names, import)?, types.into_iter().map(|ty| ty.try_into().expect("")).collect(), return_type.try_into().expect("")))
}

pub fn analysis_statements(code: Vec<Statement>) -> AnalysisStatementResult<Vec<HIRStatement<SearchPaths>>> {
    analysis(code, Vec::new(), ImportPath::List(Vec::new()))
}

fn analysis(code: Vec<Statement>, argument_names: Vec<String>, import_paths: ImportPath) -> AnalysisStatementResult<Vec<HIRStatement<SearchPaths>>> {
    let mut result = Vec::new();
    let mut variables = BTreeMap::new();
    let mut imports: BTreeMap<String, HashSet<_>> = BTreeMap::new();
    for path in collect_import_path(import_paths, Vec::new()).0 {
        imports.entry(path.last().unwrap().clone()).or_default().insert(path);
    }
    let mut variable_sequence = argument_names.len();
    for (id, name) in argument_names.into_iter().enumerate() {
        variables.insert(name, id).map_or(Ok(()), |old| Err(AnalysisStatementError::CollisionArgumentName(old, id)))?;
    }
    for stmt in code {
        if push_statement(stmt, &mut result, &mut variables, &mut variable_sequence, &mut imports)? != StatementReachable::Reachable {
            break;
        }
    }
    Ok(result)
}

fn push_deconstruct_pattern(pattern: Pattern, begin: Variable<SearchPaths>, result: &mut Vec<HIRStatement<SearchPaths>>, variables: &mut BTreeMap<String, usize>, variable_sequence: &mut usize, imports: &mut BTreeMap<String, HashSet<Vec<String>>>) -> AnalysisStatementResult<()> {
    match pattern {
        Pattern::Variable(name) => match begin {
            begin @ Variable::Named { .. } => {
                result.push(HIRStatement::Binding {
                    variable_id: *variable_sequence,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::AccessVariable(begin),
                });
                variables.insert(name, *variable_sequence);
                *variable_sequence += 1;
            }
            Variable::Unnamed(id) => {
                variables.insert(name, id);
            }
        },
        Pattern::Struct(_, patterns) => {
            for (member, pattern) in patterns {
                result.push(HIRStatement::Binding {
                    variable_id: *variable_sequence,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::AccessMember { variable: begin.clone(), member },
                });
                let current_variable = Variable::Unnamed(*variable_sequence);
                *variable_sequence += 1;
                push_deconstruct_pattern(pattern, current_variable, result, variables, variable_sequence, imports)?;
            }
        }
    }
    Ok(())
}

fn collect_import_path(path: ImportPath, mut base: Vec<String>) -> SearchPaths {
    match path {
        ImportPath::Item(name) => {
            base.push(name);
            SearchPaths(vec![base])
        }
        ImportPath::List(list) => SearchPaths(list.into_iter().flat_map(|path| collect_import_path(path, base.clone()).0).collect()),
        ImportPath::Child(name, child) => {
            base.push(name);
            collect_import_path(*child, base)
        }
    }
}

fn push_statement(Statement { statement, .. }: Statement, result: &mut Vec<HIRStatement<SearchPaths>>, variables: &mut BTreeMap<String, usize>, variable_sequence: &mut usize, imports: &mut BTreeMap<String, HashSet<Vec<String>>>) -> AnalysisStatementResult<StatementReachable> {
    match statement {
        StatementItem::Import(path) => {
            for path in collect_import_path(path, Vec::new()).0 {
                imports.entry(path.last().unwrap().clone()).or_default().insert(path);
            }
            Ok(StatementReachable::Reachable)
        }
        StatementItem::Expression(expression) => Ok(push_expression(expression, result, variables, variable_sequence, imports)?.0),
        StatementItem::LetBinding(LetBinding { pattern, type_hint, expression, .. }) => {
            let (never, variable) = push_expression(*expression, result, variables, variable_sequence, imports)?;
            if never != StatementReachable::Reachable {
                return Ok(never);
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: type_hint.map_or_else(|| HIRType::try_from(&pattern), TryInto::try_into)?,
                expression: HIRExpression::AccessVariable(variable),
            });
            let current_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            push_deconstruct_pattern(pattern, current_variable, result, variables, variable_sequence, imports)?;
            Ok(StatementReachable::Reachable)
        }
        StatementItem::Return(expression) => {
            let return_item = if let Some(expression) = expression {
                push_expression(expression, result, variables, variable_sequence, imports)?.1
            } else {
                push_expression(
                    Expression {
                        expression: ExpressionItem::ConstructTuple(Vec::new()),
                        ..Default::default()
                    },
                    result,
                    variables,
                    variable_sequence,
                    imports,
                )?
                .1
            };
            result.push(HIRStatement::Return(return_item));
            Ok(StatementReachable::UnreachableByReturn)
        }
        StatementItem::Continue(label) => {
            result.push(HIRStatement::Continue(label.clone()));
            Ok(StatementReachable::UnreachableByBreak(label))
        }
        StatementItem::Break(label) => {
            result.push(HIRStatement::Break(label.clone()));
            Ok(StatementReachable::UnreachableByBreak(label))
        }
        StatementItem::For(ForStatement {
            label, pattern, iter, block: Block { statements, last_expression, .. }, ..
        }) => {
            assert!(last_expression.is_none(), "forのブロックは最後の式を持たないはず");
            let iter_position = iter.position.clone();
            let (reachable, iterable) = push_expression(iter, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok(reachable);
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(iter_position.clone(), get_candidate_paths(vec!["_iterator".to_string()], imports)),
                    arguments: vec![iterable],
                },
            });
            let iterator = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            let mut body = Vec::new();
            let mut variables = variables.clone();
            let mut imports = imports.clone();
            body.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Or(vec![HIRType::Tuple(Vec::new()), HIRType::AnonymousStruct([("value".to_string(), HIRType::Infer)].into_iter().collect())]),
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(iter_position, get_candidate_paths(vec!["_next".to_string()], &imports)),
                    arguments: vec![iterator],
                },
            });
            let condition = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            body.push(HIRStatement::Binding {
                variable_id: *variable_sequence + 2,
                variable_type: HIRType::Infer,
                expression: HIRExpression::IfLet {
                    condition_binding: *variable_sequence,
                    pattern_type: HIRType::AnonymousStruct([("value".to_string(), HIRType::Infer)].into_iter().collect()),
                    condition,
                    then: (
                        vec![HIRStatement::Binding {
                            variable_id: *variable_sequence + 1,
                            variable_type: HIRType::Infer,
                            expression: HIRExpression::AccessMember {
                                variable: Variable::Unnamed(*variable_sequence),
                                member: "value".to_string(),
                            },
                        }],
                        Variable::Unnamed(*variable_sequence),
                    ),
                    other: (
                        vec![
                            HIRStatement::Break(None),
                            HIRStatement::Binding {
                                variable_id: *variable_sequence + 2,
                                variable_type: HIRType::Tuple(Vec::new()),
                                expression: HIRExpression::ConstructTuple(Vec::new()),
                            },
                        ],
                        Variable::Unnamed(*variable_sequence + 2),
                    ),
                },
            });
            let iterator_variable = Variable::Unnamed(*variable_sequence + 3);
            *variable_sequence += 3;
            push_deconstruct_pattern(pattern, iterator_variable, &mut body, &mut variables, variable_sequence, &mut imports)?;
            for stmt in statements {
                let reachable = push_statement(stmt, &mut body, &mut variables, variable_sequence, &mut imports)?;
                match reachable {
                    StatementReachable::UnreachableByReturn => return Ok(reachable),
                    StatementReachable::UnreachableByBreak(None) => return Ok(StatementReachable::Reachable),
                    StatementReachable::UnreachableByBreak(break_label) if break_label == label => return Ok(StatementReachable::Reachable),
                    StatementReachable::UnreachableByBreak(break_label) => return Ok(StatementReachable::UnreachableByBreak(break_label)),
                    _ => {}
                }
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Tuple(Vec::new()),
                expression: HIRExpression::Loop(body),
            });
            *variable_sequence += 1;
            Ok(StatementReachable::Reachable)
        }
        StatementItem::While(WhileStatement {
            label,
            condition: Condition::BoolExpression(expression),
            block: Block { statements, last_expression, .. },
            ..
        }) => {
            assert!(last_expression.is_none(), "whileのブロックは最後の式を持たないはず");
            let mut body = Vec::new();
            let mut variables = variables.clone();
            let mut imports = imports.clone();
            let (_, condition) = push_expression(
                Expression {
                    position: Default::default(),
                    expression: ExpressionItem::Not(Box::new(expression)),
                },
                &mut body,
                &mut variables,
                variable_sequence,
                &mut imports,
            )?;
            body.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Tuple(Vec::new()),
                expression: HIRExpression::ConstructTuple(Vec::new()),
            });
            body.push(HIRStatement::Binding {
                variable_id: *variable_sequence + 1,
                variable_type: HIRType::Tuple(Vec::new()),
                expression: HIRExpression::If {
                    condition,
                    then: (vec![HIRStatement::Break(None)], Variable::Unnamed(*variable_sequence)),
                    other: (Vec::new(), Variable::Unnamed(*variable_sequence)),
                },
            });
            *variable_sequence += 2;
            for stmt in statements {
                let reachable = push_statement(stmt, &mut body, &mut variables, variable_sequence, &mut imports)?;
                match reachable {
                    StatementReachable::UnreachableByReturn => return Ok(reachable),
                    StatementReachable::UnreachableByBreak(None) => return Ok(StatementReachable::Reachable),
                    StatementReachable::UnreachableByBreak(break_label) if break_label == label => return Ok(StatementReachable::Reachable),
                    StatementReachable::UnreachableByBreak(break_label) => return Ok(StatementReachable::UnreachableByBreak(break_label)),
                    _ => {}
                }
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Tuple(Vec::new()),
                expression: HIRExpression::Loop(body),
            });
            *variable_sequence += 1;
            Ok(StatementReachable::Reachable)
        }
        StatementItem::While(WhileStatement {
            label,
            condition: Condition::LetBinding(LetBinding { pattern, type_hint, expression, .. }),
            block: Block { statements, last_expression, .. },
            ..
        }) => {
            assert!(last_expression.is_none(), "whileのブロックは最後の式を持たないはず");
            let mut body = Vec::new();
            let mut variables = variables.clone();
            let mut imports = imports.clone();
            let (_, condition) = push_expression(*expression, &mut body, &mut variables, variable_sequence, &mut imports)?;
            let condition_type = type_hint.map(HIRType::try_from).unwrap_or_else(|| HIRType::try_from(&pattern))?;
            body.push(HIRStatement::Binding {
                variable_id: *variable_sequence + 2,
                variable_type: condition_type.clone(),
                expression: HIRExpression::IfLet {
                    condition_binding: *variable_sequence,
                    pattern_type: condition_type,
                    condition,
                    then: (vec![], Variable::Unnamed(*variable_sequence)),
                    other: (
                        vec![
                            HIRStatement::Break(None),
                            HIRStatement::Binding {
                                variable_id: *variable_sequence + 1,
                                variable_type: HIRType::Tuple(Vec::new()),
                                expression: HIRExpression::ConstructTuple(Vec::new()),
                            },
                        ],
                        Variable::Unnamed(*variable_sequence + 1),
                    ),
                },
            });
            let condition = Variable::Unnamed(*variable_sequence + 2);
            *variable_sequence += 3;
            push_deconstruct_pattern(pattern, condition, &mut body, &mut variables, variable_sequence, &mut imports)?;
            for stmt in statements {
                let reachable = push_statement(stmt, &mut body, &mut variables, variable_sequence, &mut imports)?;
                match reachable {
                    StatementReachable::UnreachableByReturn => return Ok(reachable),
                    StatementReachable::UnreachableByBreak(None) => return Ok(StatementReachable::Reachable),
                    StatementReachable::UnreachableByBreak(break_label) if break_label == label => return Ok(StatementReachable::Reachable),
                    StatementReachable::UnreachableByBreak(break_label) => return Ok(StatementReachable::UnreachableByBreak(break_label)),
                    _ => {}
                }
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Tuple(Vec::new()),
                expression: HIRExpression::Loop(body),
            });
            *variable_sequence += 1;
            Ok(StatementReachable::Reachable)
        }
    }
}

fn push_expression(Expression { expression, .. }: Expression, result: &mut Vec<HIRStatement<SearchPaths>>, variables: &mut BTreeMap<String, usize>, variable_sequence: &mut usize, imports: &mut BTreeMap<String, HashSet<Vec<String>>>) -> AnalysisStatementResult<(StatementReachable, Variable<SearchPaths>)> {
    match expression {
        ExpressionItem::AccessVariable(Path { path, position }) => {
            let variable = if let [name] = AsRef::<[String]>::as_ref(&path) { variables.get(name).map(|id| Variable::Unnamed(*id)) } else { None };
            Ok((StatementReachable::Reachable, variable.unwrap_or_else(|| Variable::Named(position, get_candidate_paths(path, imports)))))
        }
        ExpressionItem::StringImmediate(value) => {
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::Immediate(Immediate::String(value)),
            });
            let variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, variable))
        }
        ExpressionItem::NumberImmediate(value) => {
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::Immediate(Immediate::Number(value)),
            });
            let variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, variable))
        }
        ExpressionItem::AccessMember(expression, member) => {
            let (reachable, variable) = push_expression(*expression, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::AccessMember { variable, member },
            });
            let variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, variable))
        }
        ExpressionItem::CallFunction(self_expression, function_name, arguments) => {
            let mut function_arguments = Vec::with_capacity(arguments.len() + 1);
            let function = if let Some(self_expression) = self_expression {
                let self_expression = *self_expression;
                let (reachable, self_variable) = push_expression(self_expression, result, variables, variable_sequence, imports)?;
                if reachable != StatementReachable::Reachable {
                    return Ok((reachable, self_variable));
                }
                if let Some(Path { position, path }) = function_name {
                    function_arguments.push(self_variable);
                    Variable::Named(position, get_candidate_paths(path, imports))
                } else {
                    self_variable
                }
            } else {
                let Path { position, path } = function_name.unwrap();
                Variable::Named(position, get_candidate_paths(path, imports))
            };
            for argument_expression in arguments {
                let (reachable, argument_variable) = push_expression(argument_expression, result, variables, variable_sequence, imports)?;
                if reachable != StatementReachable::Reachable {
                    return Ok((reachable, argument_variable));
                }
                function_arguments.push(argument_variable);
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction { function, arguments: function_arguments },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::Indexer(base_expression, index_expression) => {
            let (reachable, base_variable) = push_expression(*base_expression, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, base_variable));
            }
            let index_expression_position = index_expression.position.clone();
            let (reachable, index_variable) = push_expression(*index_expression, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, index_variable));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(index_expression_position, get_candidate_paths(vec!["_get_item".to_string()], imports)),
                    arguments: vec![base_variable, index_variable],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::ConstructTuple(expressions) => {
            let mut members = Vec::with_capacity(expressions.len());
            for expression in expressions {
                let (reachable, variable) = push_expression(expression, result, variables, variable_sequence, imports)?;
                if reachable != StatementReachable::Reachable {
                    return Ok((reachable, variable));
                }
                members.push(variable);
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::ConstructTuple(members),
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::ConstructArray(expressions) => {
            let mut members = Vec::with_capacity(expressions.len());
            for expression in expressions {
                let (reachable, variable) = push_expression(expression, result, variables, variable_sequence, imports)?;
                if reachable != StatementReachable::Reachable {
                    return Ok((reachable, variable));
                }
                members.push(variable);
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::ConstructArray(members),
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::ConstructStruct(ConstructStruct { items, .. }) => {
            let mut members = BTreeMap::new();
            for (name, expression) in items {
                if members.contains_key(&name) {
                    return Err(AnalysisStatementError::DuplicatedMember(name));
                }
                let (reachable, variable) = push_expression(expression, result, variables, variable_sequence, imports)?;
                if reachable != StatementReachable::Reachable {
                    return Ok((reachable, variable));
                }
                members.insert(name, variable);
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::ConstructStruct(members),
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::Block(Block { statements, last_expression, .. }) => {
            let mut variables = variables.clone();
            let mut imports = imports.clone();
            for statement in statements {
                let reachable = push_statement(statement, result, &mut variables, variable_sequence, &mut imports)?;
                if reachable != StatementReachable::Reachable {
                    result.push(HIRStatement::Binding {
                        variable_id: *variable_sequence,
                        variable_type: HIRType::Tuple(Vec::new()),
                        expression: HIRExpression::ConstructTuple(Vec::new()),
                    });
                    *variable_sequence += 1;
                    return Ok((reachable, Variable::Unnamed(*variable_sequence - 1)));
                }
            }
            if let Some(last_expression) = last_expression {
                push_expression(*last_expression, result, &mut variables, variable_sequence, &mut imports)
            } else {
                result.push(HIRStatement::Binding {
                    variable_id: *variable_sequence,
                    variable_type: HIRType::Tuple(Vec::new()),
                    expression: HIRExpression::ConstructTuple(Vec::new()),
                });
                *variable_sequence += 1;
                Ok((StatementReachable::Reachable, Variable::Unnamed(*variable_sequence - 1)))
            }
        }
        ExpressionItem::Neg(expression) => {
            let expression_position = expression.position.clone();
            let (reachable, variable) = push_expression(*expression, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_neg".to_string()], imports)),
                    arguments: vec![variable],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::Not(expression) => {
            let expression_position = expression.position.clone();
            let (reachable, variable) = push_expression(*expression, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_not".to_string()], imports)),
                    arguments: vec![variable],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::Mul(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_mul".to_string()], imports)),
                    arguments: vec![variable1, variable2],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::Div(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_div".to_string()], imports)),
                    arguments: vec![variable1, variable2],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::Rem(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_rem".to_string()], imports)),
                    arguments: vec![variable1, variable2],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::Add(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_add".to_string()], imports)),
                    arguments: vec![variable1, variable2],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::Sub(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_sub".to_string()], imports)),
                    arguments: vec![variable1, variable2],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::GreaterThan(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_gt".to_string()], imports)),
                    arguments: vec![variable1, variable2],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::LessThan(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_gt".to_string()], imports)),
                    arguments: vec![variable2, variable1],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::GreaterOrEqual(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position.clone(), get_candidate_paths(vec!["_gt".to_string()], imports)),
                    arguments: vec![variable2, variable1],
                },
            });
            let tmp_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_not".to_string()], imports)),
                    arguments: vec![tmp_variable],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::LessOrEqual(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position.clone(), get_candidate_paths(vec!["_gt".to_string()], imports)),
                    arguments: vec![variable1, variable2],
                },
            });
            let tmp_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_not".to_string()], imports)),
                    arguments: vec![tmp_variable],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::Equals(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_eq".to_string()], imports)),
                    arguments: vec![variable1, variable2],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::NotEquals(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position.clone(), get_candidate_paths(vec!["_eq".to_string()], imports)),
                    arguments: vec![variable1, variable2],
                },
            });
            let tmp_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_not".to_string()], imports)),
                    arguments: vec![tmp_variable],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::And(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_and".to_string()], imports)),
                    arguments: vec![variable1, variable2],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::Xor(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_xor".to_string()], imports)),
                    arguments: vec![variable1, variable2],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }
        ExpressionItem::Or(expression1, expression2) => {
            let expression_position = expression1.position.end..expression2.position.start;
            let (reachable, variable1) = push_expression(*expression1, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable1));
            }
            let (reachable, variable2) = push_expression(*expression2, result, variables, variable_sequence, imports)?;
            if reachable != StatementReachable::Reachable {
                return Ok((reachable, variable2));
            }
            result.push(HIRStatement::Binding {
                variable_id: *variable_sequence,
                variable_type: HIRType::Infer,
                expression: HIRExpression::CallFunction {
                    function: Variable::Named(expression_position, get_candidate_paths(vec!["_or".to_string()], imports)),
                    arguments: vec![variable1, variable2],
                },
            });
            let result_variable = Variable::Unnamed(*variable_sequence);
            *variable_sequence += 1;
            Ok((StatementReachable::Reachable, result_variable))
        }

        ExpressionItem::Assign(reference_expression, value_expression) => {
            let value_expression = *value_expression;
            match *reference_expression {
                Expression { expression: ExpressionItem::AccessVariable(Path { position, path }), .. } => {
                    let (reachable, value_variable) = push_expression(value_expression, result, variables, variable_sequence, imports)?;
                    if reachable != StatementReachable::Reachable {
                        return Ok((reachable, value_variable));
                    }
                    let variable_reference = if let [name] = AsRef::<[String]>::as_ref(&path) { variables.get(name) } else { None };
                    result.push(HIRStatement::Binding {
                        variable_id: *variable_sequence,
                        variable_type: HIRType::Infer,
                        expression: HIRExpression::Assign {
                            variable: ReferenceAccess::Variable(variable_reference.copied().map(Variable::Unnamed).unwrap_or_else(|| Variable::Named(position, get_candidate_paths(path, imports)))),
                            value: value_variable,
                        },
                    });
                    let result_variable = Variable::Unnamed(*variable_sequence);
                    *variable_sequence += 1;
                    Ok((StatementReachable::Reachable, result_variable))
                }
                Expression { expression: ExpressionItem::AccessMember(base, member), .. } => {
                    let (reachable, base) = push_expression(*base, result, variables, variable_sequence, imports)?;
                    if reachable != StatementReachable::Reachable {
                        return Ok((reachable, base));
                    }
                    let (reachable, value) = push_expression(value_expression, result, variables, variable_sequence, imports)?;
                    if reachable != StatementReachable::Reachable {
                        return Ok((reachable, value));
                    }
                    result.push(HIRStatement::Binding {
                        variable_id: *variable_sequence,
                        variable_type: HIRType::Infer,
                        expression: HIRExpression::Assign { variable: ReferenceAccess::Member(base, member), value },
                    });
                    let result_variable = Variable::Unnamed(*variable_sequence);
                    *variable_sequence += 1;
                    Ok((StatementReachable::Reachable, result_variable))
                }
                Expression { expression: ExpressionItem::Indexer(base, index), .. } => {
                    let (reachable, base) = push_expression(*base, result, variables, variable_sequence, imports)?;
                    if reachable != StatementReachable::Reachable {
                        return Ok((reachable, base));
                    }
                    let index_position = index.position.clone();
                    let (reachable, index) = push_expression(*index, result, variables, variable_sequence, imports)?;
                    if reachable != StatementReachable::Reachable {
                        return Ok((reachable, index));
                    }
                    let (reachable, value) = push_expression(value_expression, result, variables, variable_sequence, imports)?;
                    if reachable != StatementReachable::Reachable {
                        return Ok((reachable, value));
                    }
                    result.push(HIRStatement::Binding {
                        variable_id: *variable_sequence,
                        variable_type: HIRType::Infer,
                        expression: HIRExpression::CallFunction {
                            function: Variable::Named(index_position, get_candidate_paths(vec!["_set_item".to_string()], imports)),
                            arguments: vec![base, index, value.clone()],
                        },
                    });
                    *variable_sequence += 1;
                    Ok((StatementReachable::Reachable, value))
                }
                _ => unreachable!(),
            }
        }
        ExpressionItem::Closure(_) => Err(AnalysisStatementError::UnImplementedFeature("ConstructClosure")),
        ExpressionItem::If(If { condition, then, other, .. }) => match *condition {
            Condition::BoolExpression(condition) => {
                let (reachable, condition) = push_expression(condition, result, variables, variable_sequence, imports)?;
                if reachable != StatementReachable::Reachable {
                    return Ok((reachable, condition));
                }
                let mut then_statements = Vec::new();
                let (mut reachable, then_result) = push_expression(*then, &mut then_statements, variables, variable_sequence, imports)?;
                let then = (then_statements, then_result);
                let other = if let Some(other) = other {
                    let mut other_statements = Vec::new();
                    let (other_reachable, other_result) = push_expression(*other, &mut other_statements, variables, variable_sequence, imports)?;
                    reachable.combine(other_reachable);
                    (other_statements, other_result)
                } else {
                    reachable = StatementReachable::Reachable;
                    *variable_sequence += 1;
                    (
                        vec![HIRStatement::Binding {
                            variable_id: *variable_sequence - 1,
                            variable_type: HIRType::Tuple(Vec::new()),
                            expression: HIRExpression::ConstructTuple(Vec::new()),
                        }],
                        Variable::Unnamed(*variable_sequence - 1),
                    )
                };
                result.push(HIRStatement::Binding {
                    variable_id: *variable_sequence,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::If { condition, then, other },
                });
                let result = Variable::Unnamed(*variable_sequence);
                *variable_sequence += 1;
                Ok((reachable, result))
            }
            Condition::LetBinding(LetBinding { pattern, type_hint, expression, .. }) => {
                let (reachable, condition_variable) = push_expression(*expression, result, variables, variable_sequence, imports)?;
                if reachable != StatementReachable::Reachable {
                    return Ok((reachable, condition_variable));
                }
                let condition_binding = *variable_sequence;
                *variable_sequence += 1;
                let pattern_type = type_hint.map_or_else(|| HIRType::try_from(&pattern), HIRType::try_from)?;
                let mut then_statements = Vec::new();
                push_deconstruct_pattern(pattern, Variable::Unnamed(condition_binding), &mut then_statements, variables, variable_sequence, imports)?;
                let (mut reachable, then_result) = push_expression(*then, &mut then_statements, variables, variable_sequence, imports)?;
                let then = (then_statements, then_result);
                let other = if let Some(other) = other {
                    let mut other_statements = Vec::new();
                    let (other_reachable, other_result) = push_expression(*other, &mut other_statements, variables, variable_sequence, imports)?;
                    reachable.combine(other_reachable);
                    (other_statements, other_result)
                } else {
                    reachable = StatementReachable::Reachable;
                    *variable_sequence += 1;
                    (
                        vec![HIRStatement::Binding {
                            variable_id: *variable_sequence - 1,
                            variable_type: HIRType::Tuple(Vec::new()),
                            expression: HIRExpression::ConstructTuple(Vec::new()),
                        }],
                        Variable::Unnamed(*variable_sequence - 1),
                    )
                };
                result.push(HIRStatement::Binding {
                    variable_id: *variable_sequence,
                    variable_type: HIRType::Infer,
                    expression: HIRExpression::IfLet {
                        condition_binding,
                        pattern_type,
                        condition: condition_variable,
                        then,
                        other,
                    },
                });
                let result = Variable::Unnamed(*variable_sequence);
                *variable_sequence += 1;
                Ok((reachable, result))
            }
        },
        ExpressionItem::Match(_) => Err(AnalysisStatementError::UnImplementedFeature("MatchExpression")),
    }
}
