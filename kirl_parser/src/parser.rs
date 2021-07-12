use std::collections::HashMap;
use std::mem;
use std::ops::Range;

use parser::{LR1Parser, Rule, Syntax};
use parser::enum_index;
use parser::enum_index_derive::*;
use parser::Symbol::{NonTerminal, Terminal};

use crate::CharacterPosition;
use crate::parser::Symbol::ValidKirlCode;
use crate::tokenizer::Token;

#[derive(Debug, Default)]
pub struct Statement {
    position: Range<CharacterPosition>,
    statement: StatementItem,
}

#[derive(Debug)]
pub enum StatementItem {
    Import(ImportPath),
    Expression(Expression),
    LetBinding(LetBinding),
    Return(Option<Expression>),
    Continue(Option<String>),
    Break(Option<String>),
    For(ForStatement),
    While(WhileStatement),
    A,
}

impl Default for StatementItem {
    fn default() -> Self {
        StatementItem::A
    }
}

#[derive(Debug, Default)]
pub struct ForStatement {
    label: Option<String>,
    position: Range<CharacterPosition>,
    pattern: Pattern,
    iter: Expression,
    block: Block,
}

#[derive(Debug)]
pub enum Condition {
    BoolExpression(Expression),
    LetBinding(LetBinding),
}

impl Default for Condition {
    fn default() -> Self {
        Condition::BoolExpression(Default::default())
    }
}

#[derive(Debug, Default)]
pub struct WhileStatement {
    label: Option<String>,
    position: Range<CharacterPosition>,
    condition: Condition,
    block: Block,
}

#[derive(Debug, Default)]
pub struct Block {
    position: Range<CharacterPosition>,
    statements: Vec<Statement>,
    last_expression: Option<Box<Expression>>,
}

#[derive(Debug, Default)]
pub struct Function {
    position: Range<CharacterPosition>,
    name: String,
    generics_arguments: Vec<String>,
    arguments: Vec<(String, Type)>,
    return_type: Type,
    expression: Expression,
}

#[derive(Debug, Default)]
pub struct Struct {
    name: String,
    generics_arguments: Vec<String>,
    members: Vec<(String, Type)>,
}

#[derive(Debug)]
pub enum ImportPath {
    Item(String),
    List(Vec<ImportPath>),
    Child(String, Box<ImportPath>),
}

impl Default for ImportPath {
    fn default() -> Self {
        ImportPath::Item(Default::default())
    }
}

#[derive(Debug, Default)]
pub struct Path {
    position: Range<CharacterPosition>,
    path: Vec<String>,
}

#[derive(Debug, Default)]
pub struct NamedType {
    position: Range<CharacterPosition>,
    path: Vec<String>,
    generics_arguments: Vec<Type>,
}

#[derive(Debug, Default)]
pub struct FunctionType {
    position: Range<CharacterPosition>,
    argument: Vec<Type>,
    result: Box<Type>,
}

#[derive(Debug, Default)]
pub struct AnonymousStructType {
    position: Range<CharacterPosition>,
    members: Vec<(String, Type)>,
}

#[derive(Debug)]
pub enum Type {
    None,
    Unreachable(Range<CharacterPosition>),
    NamedType(NamedType),
    Tuple(Range<CharacterPosition>, Vec<Type>),
    Array(Range<CharacterPosition>, Box<Type>),
    Function(FunctionType),
    AnonymousStruct(AnonymousStructType),
    Or(Range<CharacterPosition>, Vec<Type>),
}

impl Default for Type {
    fn default() -> Self {
        Type::None
    }
}

#[derive(Debug)]
pub enum StructName {
    Anonymous,
    Named(NamedType),
}

impl Default for StructName {
    fn default() -> Self {
        StructName::Anonymous
    }
}

#[derive(Debug, Default)]
pub struct Expression {
    position: Range<CharacterPosition>,
    expression: ExpressionItem,
}

#[derive(Debug)]
pub enum ExpressionItem {
    AccessVariable(Path),
    StringImmediate(String),
    IntegerImmediate(i64),
    FloatImmediate(f64),
    AccessMember(Box<Expression>, String),
    CallFunction(Box<Expression>, Option<Path>, Vec<Expression>),
    ConstructTuple(Vec<Expression>),
    ConstructArray(Vec<Expression>),
    Block(Block),
    Neg(Box<Expression>),
    Not(Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Rem(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    GreaterOrEqual(Box<Expression>, Box<Expression>),
    LessOrEqual(Box<Expression>, Box<Expression>),
    Equals(Box<Expression>, Box<Expression>),
    NotEquals(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Xor(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Closure(Closure),
    If(If),
    Match(Match),
}

impl Default for ExpressionItem {
    fn default() -> Self {
        ExpressionItem::IntegerImmediate(Default::default())
    }
}

#[derive(Debug, Default)]
pub struct ConstructStruct {
    name: StructName,
    items: Vec<(String, Expression)>,
}

#[derive(Debug)]
pub enum KirlTopLevelStatement {
    Statement((Range<CharacterPosition>, Statement)),
    FunctionDefinition((Range<CharacterPosition>, Function)),
    StructDefinition((Range<CharacterPosition>, Struct)),
}

impl Default for KirlTopLevelStatement {
    fn default() -> Self {
        KirlTopLevelStatement::Statement(Default::default())
    }
}

#[derive(Debug)]
pub enum Pattern {
    Variable(String),
    Struct(StructName, Vec<(String, Pattern)>),
}

impl Default for Pattern {
    fn default() -> Self {
        Pattern::Variable(Default::default())
    }
}

#[derive(Debug, Default)]
pub struct LetBinding {
    position: Range<CharacterPosition>,
    pattern: Pattern,
    type_hint: Option<Type>,
    expression: Expression,
}

#[derive(Debug, Default)]
pub struct Closure {
    position: Range<CharacterPosition>,
    arguments: Vec<(String, Option<Type>)>,
    expression: Box<Expression>,
}

#[derive(Debug, Default)]
pub struct If {
    position: Range<CharacterPosition>,
    condition: Box<Condition>,
    then: Box<Expression>,
    other: Option<Box<Expression>>,
}

#[derive(Debug, Default)]
pub struct Match {
    position: Range<CharacterPosition>,
    condition: Box<Expression>,
    items: Vec<(Pattern, Expression)>,
}

#[derive(Debug, EnumIndex)]
pub enum Symbol {
    ValidKirlCode((Range<CharacterPosition>, Vec<KirlTopLevelStatement>)),
    Type((Range<CharacterPosition>, Type)),
    FullPath((Range<CharacterPosition>, Path)),
    CommaSeparatedTypes((Range<CharacterPosition>, Vec<Type>)),
    OrSeparatedTypes((Range<CharacterPosition>, Vec<Type>)),
    StructDefinitionItems((Range<CharacterPosition>, Vec<(String, Type)>)),
    ConstructStruct((Range<CharacterPosition>, ConstructStruct)),
    StructName((Range<CharacterPosition>, StructName)),
    StructDefinition((Range<CharacterPosition>, Struct)),
    ConstructStructItems((Range<CharacterPosition>, Vec<(String, Expression)>)),
    LetBinding((Range<CharacterPosition>, LetBinding)),
    Statement((Range<CharacterPosition>, Statement)),
    StatementList((Range<CharacterPosition>, Vec<Statement>)),
    Block((Range<CharacterPosition>, Block)),
    Expression8((Range<CharacterPosition>, Expression)),
    Expression7((Range<CharacterPosition>, Expression)),
    Expression6((Range<CharacterPosition>, Expression)),
    Expression5((Range<CharacterPosition>, Expression)),
    Expression4((Range<CharacterPosition>, Expression)),
    Expression3((Range<CharacterPosition>, Expression)),
    Expression2((Range<CharacterPosition>, Expression)),
    Expression1((Range<CharacterPosition>, Expression)),
    Expression0((Range<CharacterPosition>, Expression)),
    Expression((Range<CharacterPosition>, Expression)),
    ClosureExpression((Range<CharacterPosition>, Closure)),
    IfExpression((Range<CharacterPosition>, If)),
    MatchExpression((Range<CharacterPosition>, Match)),
    MatchPatterns((Range<CharacterPosition>, Vec<(Pattern, Expression)>)),
    ForStatement((Range<CharacterPosition>, ForStatement)),
    WhileStatement((Range<CharacterPosition>, WhileStatement)),
    Pattern((Range<CharacterPosition>, Pattern)),
    PatternStructItems((Range<CharacterPosition>, Vec<(String, Pattern)>)),
    ClosureArguments((Range<CharacterPosition>, Vec<(String, Option<Type>)>)),
    CommaSeparatedExpressions((Range<CharacterPosition>, Vec<Expression>)),
    FunctionDefinition((Range<CharacterPosition>, Function)),
    FunctionArguments((Range<CharacterPosition>, Vec<(String, Type)>)),
    ImportStatement((Range<CharacterPosition>, ImportPath)),
    ImportPath((Range<CharacterPosition>, ImportPath)),
    ImportPathChild((Range<CharacterPosition>, ImportPath)),
    CommaSeparatedImportPathChildren((Range<CharacterPosition>, Vec<ImportPath>)),
    GenericsTypeArguments((Range<CharacterPosition>, Vec<String>)),
}

fn get_syntax() -> Syntax<Symbol, Token> {
    Syntax::builder()
        .rule(Rule::new(Symbol::ValidKirlCode(Default::default()), &[],
                        |list| if let [] = list {
                            Symbol::ValidKirlCode((Default::default()))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ValidKirlCode(Default::default()), &[NonTerminal(Symbol::ValidKirlCode(Default::default())), NonTerminal(Symbol::Statement(Default::default()))],
                        |list| match list {
                            [NonTerminal(Symbol::ValidKirlCode((_, list))), NonTerminal(Symbol::Statement((range, statement)))] if list.len() == 0 => Symbol::ValidKirlCode((range.clone(), vec![KirlTopLevelStatement::Statement((mem::take(range), mem::take(statement)))])),
                            [NonTerminal(Symbol::ValidKirlCode((Range { start, .. }, list))), NonTerminal(Symbol::Statement((range, statement)))] => Symbol::ValidKirlCode((*start..range.end, {
                                list.push(KirlTopLevelStatement::Statement((mem::take(range), mem::take(statement))));
                                mem::take(list)
                            })),
                            _ => unreachable!(),
                        }))
        .rule(Rule::new(Symbol::ValidKirlCode(Default::default()), &[NonTerminal(Symbol::ValidKirlCode(Default::default())), NonTerminal(Symbol::FunctionDefinition(Default::default()))],
                        |list| match list {
                            [NonTerminal(Symbol::ValidKirlCode((_, list))), NonTerminal(Symbol::FunctionDefinition((range, function)))] if list.len() == 0 => Symbol::ValidKirlCode((range.clone(), vec![KirlTopLevelStatement::FunctionDefinition((mem::take(range), mem::take(function)))])),
                            [NonTerminal(Symbol::ValidKirlCode((Range { start, .. }, list))), NonTerminal(Symbol::FunctionDefinition((range, function)))] => Symbol::ValidKirlCode((*start..range.end, {
                                list.push(KirlTopLevelStatement::FunctionDefinition((mem::take(range), mem::take(function))));
                                mem::take(list)
                            })),
                            _ => unreachable!(),
                        }))
        .rule(Rule::new(Symbol::ValidKirlCode(Default::default()), &[NonTerminal(Symbol::ValidKirlCode(Default::default())), NonTerminal(Symbol::StructDefinition(Default::default()))],
                        |list| match list {
                            [NonTerminal(Symbol::ValidKirlCode((_, list))), NonTerminal(Symbol::StructDefinition((range, s)))] if list.len() == 0 => Symbol::ValidKirlCode((range.clone(), vec![KirlTopLevelStatement::StructDefinition((mem::take(range), mem::take(s)))])),
                            [NonTerminal(Symbol::ValidKirlCode((Range { start, .. }, list))), NonTerminal(Symbol::StructDefinition((range, s)))] => Symbol::ValidKirlCode((*start..range.end, {
                                list.push(KirlTopLevelStatement::StructDefinition((mem::take(range), mem::take(s))));
                                mem::take(list)
                            })),
                            _ => unreachable!(),
                        }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((position, ident)))] = list {
                            Symbol::Type((position.clone(), Type::NamedType(NamedType {
                                position: position.clone(),
                                path: vec![ident.clone()],
                                generics_arguments: Vec::new(),
                            })))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::Not(Default::default()))],
                        |list| if let [Terminal(Token::Not(position))] = list {
                            Symbol::Type((position.clone(), Type::Unreachable(position.clone())))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::GreaterThan(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, _, NonTerminal(Symbol::CommaSeparatedTypes((_, types))), Terminal(Token::GreaterThan((Range { end, .. })))] = list {
                            Symbol::Type((*start..*end, Type::NamedType(NamedType {
                                position: *start..*end,
                                path: vec![name.clone()],
                                generics_arguments: mem::take(types),
                            })))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[NonTerminal(Symbol::FullPath(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FullPath((symbol_position, Path { position, path })))] = list {
                            Symbol::Type((mem::take(symbol_position), Type::NamedType(NamedType {
                                position: mem::take(position),
                                path: mem::take(path),
                                generics_arguments: Vec::new(),
                            })))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[NonTerminal(Symbol::FullPath(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::GreaterThan(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FullPath((Range { start, .. }, Path { path, .. }))), _, _, NonTerminal(Symbol::CommaSeparatedTypes((_, types))), Terminal(Token::GreaterThan((Range { end, .. })))] = list {
                            Symbol::Type((*start..*end, Type::NamedType(NamedType {
                                position: *start..*end,
                                path: mem::take(path),
                                generics_arguments: mem::take(types),
                            })))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::SquareBracketOpen(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::SquareBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::SquareBracketOpen((Range { start, .. }))), NonTerminal(Symbol::Type((_, inner_type))), Terminal(Token::SquareBracketClose(Range { end, .. }))] = list {
                            Symbol::Type((*start..*end, Type::Array(*start..*end, Box::new(mem::take(inner_type)))))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen((Range { start, .. }))), NonTerminal(Symbol::Type((_, inner_type))), Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Type((*start..*end, mem::take(inner_type)))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen((Range { start, .. }))), NonTerminal(Symbol::Type((_, inner_type))), _, Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Type((*start..*end, Type::Tuple(*start..*end, vec![mem::take(inner_type)])))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen((Range { start, .. }))), NonTerminal(Symbol::CommaSeparatedTypes((_, types))), Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Type((*start..*end, Type::Tuple(*start..*end, mem::take(types))))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::Sharp(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::StructDefinitionItems(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Sharp((Range { start, .. }))), _, NonTerminal(Symbol::StructDefinitionItems((_, members))), Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::Type((*start..*end, Type::AnonymousStruct(AnonymousStructType { position: *start..*end, members: mem::take(members) })))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen((Range { start, .. }))), _, _, NonTerminal(Symbol::Type((Range { end, .. }, return_type)))] = list {
                            Symbol::Type((*start..*end, Type::Function(FunctionType {
                                position: *start..*end,
                                argument: Vec::new(),
                                result: Box::new(mem::take(return_type)),
                            })))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen((Range { start, .. }))), NonTerminal(Symbol::Type((_, argument_type))), _, _, NonTerminal(Symbol::Type((Range { end, .. }, return_type)))] = list {
                            Symbol::Type((*start..*end, Type::Function(FunctionType {
                                position: *start..*end,
                                argument: vec![mem::take(argument_type)],
                                result: Box::new(mem::take(return_type)),
                            })))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen((Range { start, .. }))), NonTerminal(Symbol::CommaSeparatedTypes((_, arguments))), _, _, NonTerminal(Symbol::Type((Range { end, .. }, return_type)))] = list {
                            Symbol::Type((*start..*end, Type::Function(FunctionType {
                                position: *start..*end,
                                argument: mem::take(arguments),
                                result: Box::new(mem::take(return_type)),
                            })))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::OrSeparatedTypes(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen((Range { start, .. }))), NonTerminal(Symbol::OrSeparatedTypes((_, types))), Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Type((*start..*end, Type::Or(*start..*end, mem::take(types))))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FullPath(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((Range { start, .. }, first))), _, Terminal(Token::Identifier((Range { end, .. }, second)))] = list {
                            Symbol::FullPath((*start..*end, Path { position: *start..*end, path: vec![first.clone(), second.clone()] }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FullPath(Default::default()), &[NonTerminal(Symbol::FullPath(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FullPath((Range { start, .. }, Path { path, .. }))), _, Terminal(Token::Identifier((Range { end, .. }, last)))] = list {
                            Symbol::FullPath((*start..*end, Path {
                                position: *start..*end,
                                path: {
                                    path.push(last.clone());
                                    mem::take(path)
                                },
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::CommaSeparatedTypes(Default::default()), &[NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Comma(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Type((Range { start, .. }, first))), _, NonTerminal(Symbol::Type((Range { end, .. }, second)))] = list {
                            Symbol::CommaSeparatedTypes((*start..*end, vec![mem::take(first), mem::take(second)]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::CommaSeparatedTypes(Default::default()), &[NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::Comma(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::CommaSeparatedTypes((Range { start, .. }, types))), _, NonTerminal(Symbol::Type((Range { end, .. }, last)))] = list {
                            Symbol::CommaSeparatedTypes((*start..*end, {
                                types.push(mem::take(last));
                                mem::take(types)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::OrSeparatedTypes(Default::default()), &[NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Type((Range { start, .. }, first))), _, NonTerminal(Symbol::Type((Range { end, .. }, second)))] = list {
                            Symbol::OrSeparatedTypes((*start..*end, vec![mem::take(first), mem::take(second)]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::OrSeparatedTypes(Default::default()), &[NonTerminal(Symbol::OrSeparatedTypes(Default::default())), Terminal(Token::Or(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::OrSeparatedTypes((Range { start, .. }, types))), _, NonTerminal(Symbol::Type((Range { end, .. }, last)))] = list {
                            Symbol::OrSeparatedTypes((*start..*end, {
                                types.push(mem::take(last));
                                mem::take(types)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructDefinitionItems(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, item_type)))] = list {
                            Symbol::StructDefinitionItems((*start..*end, vec![(name.clone(), mem::take(item_type))]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructDefinitionItems(Default::default()), &[NonTerminal(Symbol::StructDefinitionItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructDefinitionItems((Range { start, .. }, list))), _, Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, item_type)))] = list {
                            Symbol::StructDefinitionItems((*start..*end, {
                                list.push((name.clone(), mem::take(item_type)));
                                mem::take(list)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStruct(Default::default()), &[NonTerminal(Symbol::StructName(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::ConstructStructItems(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, NonTerminal(Symbol::ConstructStructItems((_, items))), Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::ConstructStruct((*start..*end, ConstructStruct { name: mem::take(name), items: mem::take(items) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStruct(Default::default()), &[NonTerminal(Symbol::StructName(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::ConstructStructItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, NonTerminal(Symbol::ConstructStructItems((_, items))), _, Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::ConstructStruct((*start..*end, ConstructStruct { name: mem::take(name), items: mem::take(items) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStruct(Default::default()), &[NonTerminal(Symbol::StructName(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::ConstructStruct((*start..*end, ConstructStruct { name: mem::take(name), items: Vec::new() }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructName(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((position, name)))] = list {
                            Symbol::StructName((position.clone(), StructName::Named(NamedType { position: position.clone(), path: vec![name.clone()], generics_arguments: Vec::new() })))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructName(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::GreaterThan(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, _, NonTerminal(Symbol::CommaSeparatedTypes((_, types))), Terminal(Token::GreaterThan((Range { end, .. })))] = list {
                            Symbol::StructName((*start..*end, StructName::Named(NamedType { position: *start..*end, path: vec![name.clone()], generics_arguments: mem::take(types) })))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructName(Default::default()), &[NonTerminal(Symbol::FullPath(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FullPath((position, path)))] = list {
                            Symbol::StructName((position.clone(), StructName::Named(NamedType { position: position.clone(), path: mem::take(&mut path.path), generics_arguments: Vec::new() })))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructName(Default::default()), &[NonTerminal(Symbol::FullPath(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::GreaterThan(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FullPath((Range { start, .. }, path))), _, _, NonTerminal(Symbol::CommaSeparatedTypes((_, types))), Terminal(Token::GreaterThan((Range { end, .. })))] = list {
                            Symbol::StructName((*start..*end, StructName::Named(NamedType { position: *start..*end, path: mem::take(&mut path.path), generics_arguments: mem::take(types) })))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructName(Default::default()), &[Terminal(Token::Sharp(Default::default()))],
                        |list| if let [Terminal(Token::Sharp(position))] = list {
                            Symbol::StructName((position.clone(), StructName::Anonymous))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructDefinition(Default::default()), &[Terminal(Token::Struct(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Struct((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::StructDefinition((*start..*end, Struct { name: name.clone(), generics_arguments: Vec::new(), members: Vec::new() }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructDefinition(Default::default()), &[Terminal(Token::Struct(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Struct((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, types))), _, _, Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::StructDefinition((*start..*end, Struct { name: name.clone(), generics_arguments: mem::take(types), members: Vec::new() }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructDefinition(Default::default()), &[Terminal(Token::Struct(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::StructDefinitionItems(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Struct((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::StructDefinitionItems((_, items))), Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::StructDefinition((*start..*end, Struct { name: name.clone(), generics_arguments: Vec::new(), members: mem::take(items) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructDefinition(Default::default()), &[Terminal(Token::Struct(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::StructDefinitionItems(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Struct((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, types))), _, _, NonTerminal(Symbol::StructDefinitionItems((_, items))), Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::StructDefinition((*start..*end, Struct { name: name.clone(), generics_arguments: mem::take(types), members: mem::take(items) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStructItems(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
                            Symbol::ConstructStructItems((*start..*end, vec![(name.clone(), mem::take(expression))]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStructItems(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((position, name)))] = list {
                            Symbol::ConstructStructItems((position.clone(), vec![(name.clone(), Expression { position: position.clone(), expression: ExpressionItem::AccessVariable(Path { position: position.clone(), path: vec![name.clone()] }) })]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStructItems(Default::default()), &[NonTerminal(Symbol::ConstructStructItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ConstructStructItems((Range { start, .. }, items))), _, Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
                            Symbol::ConstructStructItems((*start..*end, {
                                items.push((name.clone(), mem::take(expression)));
                                mem::take(items)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStructItems(Default::default()), &[NonTerminal(Symbol::ConstructStructItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ConstructStructItems((Range { start, .. }, items))), _, Terminal(Token::Identifier((position, name)))] = list {
                            Symbol::ConstructStructItems((*start..position.end, {
                                items.push((name.clone(), Expression {
                                    position: position.clone(),
                                    expression: ExpressionItem::AccessVariable(Path { position: position.clone(), path: vec![name.clone()] }),
                                }));
                                mem::take(items)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::LetBinding(Default::default()), &[Terminal(Token::Let(Default::default())), NonTerminal(Symbol::Pattern(Default::default())), Terminal(Token::Assign(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [Terminal(Token::Let((Range { start, .. }))), NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
                            Symbol::LetBinding((*start..*end, LetBinding { position: *start..*end, pattern: mem::take(pattern), type_hint: None, expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::LetBinding(Default::default()), &[Terminal(Token::Let(Default::default())), NonTerminal(Symbol::Pattern(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Assign(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [Terminal(Token::Let((Range { start, .. }))), NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Type((_, type_hint))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
                            Symbol::LetBinding((*start..*end, LetBinding { position: *start..*end, pattern: mem::take(pattern), type_hint: Some(mem::take(type_hint)), expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[NonTerminal(Symbol::ImportStatement(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ImportStatement((position, path)))] = list {
                            Symbol::Statement((position.clone(), Statement { position: mem::take(position), statement: StatementItem::Import(mem::take(path)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression((Range { start, .. }, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Expression(mem::take(expression)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[NonTerminal(Symbol::LetBinding(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [NonTerminal(Symbol::LetBinding((Range { start, .. }, let_binding))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::LetBinding(mem::take(let_binding)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[Terminal(Token::Return(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Return((Range { start, .. }))), NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Return(Some(mem::take(expression))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[Terminal(Token::Return(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Return((Range { start, .. }))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Return(None) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[Terminal(Token::Continue(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Continue((Range { start, .. }))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Continue(None) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[Terminal(Token::Continue(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Continue((Range { start, .. }))), Terminal(Token::Identifier((_, label))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Continue(Some(label.clone())) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[Terminal(Token::Break(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Break((Range { start, .. }))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Break(None) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[Terminal(Token::Break(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Break((Range { start, .. }))), Terminal(Token::Identifier((_, label))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Break(Some(label.clone())) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[NonTerminal(Symbol::ForStatement(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ForStatement((position, for_statement)))] = list {
                            Symbol::Statement((position.clone(), Statement { position: mem::take(position), statement: StatementItem::For(mem::take(for_statement)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[NonTerminal(Symbol::WhileStatement(Default::default()))],
                        |list| if let [NonTerminal(Symbol::WhileStatement((position, while_statement)))] = list {
                            Symbol::Statement((position.clone(), Statement { position: mem::take(position), statement: StatementItem::While(mem::take(while_statement)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StatementList(Default::default()), &[NonTerminal(Symbol::Statement(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Statement((position, statement)))] = list {
                            Symbol::StatementList((mem::take(position), vec![mem::take(statement)]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StatementList(Default::default()), &[NonTerminal(Symbol::StatementList(Default::default())), NonTerminal(Symbol::Statement(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StatementList((Range { start, .. }, list))), NonTerminal(Symbol::Statement((Range { end, .. }, statement)))] = list {
                            Symbol::StatementList((*start..*end, {
                                list.push(mem::take(statement));
                                mem::take(list)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Block(Default::default()), &[Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::StatementList(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::WaveBracketOpen((Range { start, .. }))), NonTerminal(Symbol::StatementList((_, list))), NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::Block((*start..*end, Block {
                                position: *start..*end,
                                statements: mem::take(list),
                                last_expression: Some(Box::new(mem::take(expression))),
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Block(Default::default()), &[Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::StatementList(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::WaveBracketOpen((Range { start, .. }))), NonTerminal(Symbol::StatementList((_, list))), Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::Block((*start..*end, Block {
                                position: *start..*end,
                                statements: mem::take(list),
                                last_expression: None,
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Block(Default::default()), &[Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::WaveBracketOpen((Range { start, .. }))), NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::Block((*start..*end, Block {
                                position: *start..*end,
                                statements: Vec::new(),
                                last_expression: Some(Box::new(mem::take(expression))),
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Block(Default::default()), &[Terminal(Token::WaveBracketOpen(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::WaveBracketOpen((Range { start, .. }))), Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::Block((*start..*end, Block {
                                position: *start..*end,
                                statements: Vec::new(),
                                last_expression: None,
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((position, variable)))] = list {
                            Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::AccessVariable(Path { position: position.clone(), path: vec![variable.clone()] }) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::StringImmediate(Default::default()))],
                        |list| if let [Terminal(Token::StringImmediate((position, value)))] = list {
                            Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::StringImmediate(value.clone()) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::IntegerImmediate(Default::default()))],
                        |list| if let [Terminal(Token::IntegerImmediate((position, value)))] = list {
                            Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::IntegerImmediate(*value) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::FloatImmediate(Default::default()))],
                        |list| if let [Terminal(Token::FloatImmediate((position, value)))] = list {
                            Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::FloatImmediate(*value) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::Dot(Default::default())), NonTerminal(Symbol::FullPath(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, NonTerminal(Symbol::FullPath((_, path))), _, Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Box::new(mem::take(expression)), Some(mem::take(path)), Vec::new()) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Box::new(mem::take(expression)), None, Vec::new()) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::Dot(Default::default())), NonTerminal(Symbol::FullPath(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, NonTerminal(Symbol::FullPath((_, path))), _, NonTerminal(Symbol::Expression((_, argument))), Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Box::new(mem::take(expression)), Some(mem::take(path)), vec![mem::take(argument)]) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, NonTerminal(Symbol::Expression((_, argument))), Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Box::new(mem::take(expression)), None, vec![mem::take(argument)]) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::Dot(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, Terminal(Token::Identifier((Range { end, .. }, member)))] = list {
                            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::AccessMember(Box::new(mem::take(expression)), member.clone()) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::Dot(Default::default())), NonTerminal(Symbol::FullPath(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedExpressions(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, NonTerminal(Symbol::FullPath((_, path))), _, NonTerminal(Symbol::CommaSeparatedExpressions((_, arguments))), Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Box::new(mem::take(expression)), Some(mem::take(path)), mem::take(arguments)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedExpressions(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, NonTerminal(Symbol::CommaSeparatedExpressions((_, arguments))), Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Box::new(mem::take(expression)), None, mem::take(arguments)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedExpressions(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen((Range { start, .. }))), NonTerminal(Symbol::CommaSeparatedExpressions((_, expressions))), Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructTuple(mem::take(expressions)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::SquareBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedExpressions(Default::default())), Terminal(Token::SquareBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::SquareBracketOpen((Range { start, .. }))), NonTerminal(Symbol::CommaSeparatedExpressions((_, expressions))), Terminal(Token::SquareBracketClose((Range { end, .. })))] = list {
                            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructArray(mem::take(expressions)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen((Range { start, .. }))), NonTerminal(Symbol::Expression((_, item))), _, Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructTuple(vec![mem::take(item)]) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::SquareBracketOpen(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::SquareBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::SquareBracketOpen((Range { start, .. }))), NonTerminal(Symbol::Expression((_, item))), Terminal(Token::SquareBracketClose((Range { end, .. })))] = list {
                            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructArray(vec![mem::take(item)]) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen((Range { start, .. }))), Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructTuple(Vec::new()) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::SquareBracketOpen(Default::default())), Terminal(Token::SquareBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::SquareBracketOpen((Range { start, .. }))), Terminal(Token::SquareBracketClose((Range { end, .. })))] = list {
                            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructArray(Vec::new()) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen((Range { start, .. }))), NonTerminal(Symbol::Expression((_, item))), Terminal(Token::RoundBracketClose((Range { end, .. })))] = list {
                            Symbol::Expression8((*start..*end, mem::take(item)))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Block((position, block)))] = list {
                            Symbol::Expression8((position.clone(), Expression { position: mem::take(position), expression: ExpressionItem::Block(mem::take(block)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::FullPath(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FullPath((position, path)))] = list {
                            Symbol::Expression8((position.clone(), Expression { position: mem::take(position), expression: ExpressionItem::AccessVariable(mem::take(path)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression7(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8(item))] = list {
                            Symbol::Expression7(mem::take(item))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression7(Default::default()), &[Terminal(Token::Sub(Default::default())), NonTerminal(Symbol::Expression7(Default::default()))],
                        |list| if let [Terminal(Token::Sub((Range { start, .. }))), NonTerminal(Symbol::Expression7((Range { end, .. }, expression)))] = list {
                            Symbol::Expression7((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Neg(Box::new(mem::take(expression))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression7(Default::default()), &[Terminal(Token::Not(Default::default())), NonTerminal(Symbol::Expression7(Default::default()))],
                        |list| if let [Terminal(Token::Not((Range { start, .. }))), NonTerminal(Symbol::Expression7((Range { end, .. }, expression)))] = list {
                            Symbol::Expression7((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Not(Box::new(mem::take(expression))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression6(Default::default()), &[NonTerminal(Symbol::Expression7(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression7(item))] = list {
                            Symbol::Expression6(mem::take(item))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression6(Default::default()), &[NonTerminal(Symbol::Expression6(Default::default())), Terminal(Token::Mul(Default::default())), NonTerminal(Symbol::Expression7(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression6((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression7((Range { end, .. }, second)))] = list {
                            Symbol::Expression6((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Mul(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression6(Default::default()), &[NonTerminal(Symbol::Expression6(Default::default())), Terminal(Token::Div(Default::default())), NonTerminal(Symbol::Expression7(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression6((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression7((Range { end, .. }, second)))] = list {
                            Symbol::Expression6((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Div(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression6(Default::default()), &[NonTerminal(Symbol::Expression6(Default::default())), Terminal(Token::Rem(Default::default())), NonTerminal(Symbol::Expression7(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression6((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression7((Range { end, .. }, second)))] = list {
                            Symbol::Expression6((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Rem(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression5(Default::default()), &[NonTerminal(Symbol::Expression6(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression6(item))] = list {
                            Symbol::Expression5(mem::take(item))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression5(Default::default()), &[NonTerminal(Symbol::Expression5(Default::default())), Terminal(Token::Add(Default::default())), NonTerminal(Symbol::Expression6(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression5((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression6((Range { end, .. }, second)))] = list {
                            Symbol::Expression5((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Add(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression5(Default::default()), &[NonTerminal(Symbol::Expression5(Default::default())), Terminal(Token::Sub(Default::default())), NonTerminal(Symbol::Expression6(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression5((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression6((Range { end, .. }, second)))] = list {
                            Symbol::Expression5((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Sub(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression4(Default::default()), &[NonTerminal(Symbol::Expression5(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression5(item))] = list {
                            Symbol::Expression4(mem::take(item))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression4(Default::default()), &[NonTerminal(Symbol::Expression4(Default::default())), Terminal(Token::GreaterThan(Default::default())), NonTerminal(Symbol::Expression5(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression4((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression5((Range { end, .. }, second)))] = list {
                            Symbol::Expression4((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::GreaterThan(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression4(Default::default()), &[NonTerminal(Symbol::Expression4(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::Expression5(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression4((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression5((Range { end, .. }, second)))] = list {
                            Symbol::Expression4((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::LessThan(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression4(Default::default()), &[NonTerminal(Symbol::Expression4(Default::default())), Terminal(Token::GreaterOrEqual(Default::default())), NonTerminal(Symbol::Expression5(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression4((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression5((Range { end, .. }, second)))] = list {
                            Symbol::Expression4((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::GreaterOrEqual(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression4(Default::default()), &[NonTerminal(Symbol::Expression4(Default::default())), Terminal(Token::LessOrEqual(Default::default())), NonTerminal(Symbol::Expression5(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression4((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression5((Range { end, .. }, second)))] = list {
                            Symbol::Expression4((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::LessOrEqual(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression3(Default::default()), &[NonTerminal(Symbol::Expression4(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression4(item))] = list {
                            Symbol::Expression3(mem::take(item))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression3(Default::default()), &[NonTerminal(Symbol::Expression3(Default::default())), Terminal(Token::Equals(Default::default())), NonTerminal(Symbol::Expression4(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression3((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression4((Range { end, .. }, second)))] = list {
                            Symbol::Expression3((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Equals(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression3(Default::default()), &[NonTerminal(Symbol::Expression3(Default::default())), Terminal(Token::NotEquals(Default::default())), NonTerminal(Symbol::Expression4(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression3((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression4((Range { end, .. }, second)))] = list {
                            Symbol::Expression3((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::NotEquals(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression2(Default::default()), &[NonTerminal(Symbol::Expression3(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression3(item))] = list {
                            Symbol::Expression2(mem::take(item))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression2(Default::default()), &[NonTerminal(Symbol::Expression2(Default::default())), Terminal(Token::And(Default::default())), NonTerminal(Symbol::Expression3(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression2((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression3((Range { end, .. }, second)))] = list {
                            Symbol::Expression2((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::And(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression1(Default::default()), &[NonTerminal(Symbol::Expression2(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression2(item))] = list {
                            Symbol::Expression1(mem::take(item))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression1(Default::default()), &[NonTerminal(Symbol::Expression1(Default::default())), Terminal(Token::Xor(Default::default())), NonTerminal(Symbol::Expression2(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression1((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression2((Range { end, .. }, second)))] = list {
                            Symbol::Expression1((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Xor(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression0(Default::default()), &[NonTerminal(Symbol::Expression1(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression1(item))] = list {
                            Symbol::Expression0(mem::take(item))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression0(Default::default()), &[NonTerminal(Symbol::Expression0(Default::default())), Terminal(Token::Or(Default::default())), NonTerminal(Symbol::Expression1(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression0((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression1((Range { end, .. }, second)))] = list {
                            Symbol::Expression0((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Or(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression(Default::default()), &[NonTerminal(Symbol::Expression0(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression0(item))] = list {
                            Symbol::Expression(mem::take(item))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression(Default::default()), &[NonTerminal(Symbol::ClosureExpression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ClosureExpression((position, closure)))] = list {
                            Symbol::Expression((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::Closure(mem::take(closure)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression(Default::default()), &[NonTerminal(Symbol::IfExpression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::IfExpression((position, expression)))] = list {
                            Symbol::Expression((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::If(mem::take(expression)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression(Default::default()), &[NonTerminal(Symbol::MatchExpression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::MatchExpression((position, expression)))] = list {
                            Symbol::Expression((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::Match(mem::take(expression)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ClosureExpression(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::ClosureArguments(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), _, NonTerminal(Symbol::ClosureArguments((_, arguments))), _, _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
                            Symbol::ClosureExpression((*start..*end, Closure { position: *start..*end, arguments: mem::take(arguments), expression: Box::new(mem::take(expression)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ClosureExpression(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Colon(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), _, _, _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
                            Symbol::ClosureExpression((*start..*end, Closure { position: *start..*end, arguments: Vec::new(), expression: Box::new(mem::take(expression)) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::IfExpression(Default::default()), &[Terminal(Token::If(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::If((Range { start, .. }))), NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
                            Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::BoolExpression(mem::take(condition))), then: Box::new(Expression { position: block.position.clone(), expression: ExpressionItem::Block(mem::take(block)) }), other: None }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::IfExpression(Default::default()), &[Terminal(Token::If(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default())), Terminal(Token::Else(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::If((Range { start, .. }))), NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::Block((_, then))), _, NonTerminal(Symbol::Block((Range { end, .. }, other)))] = list {
                            Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::BoolExpression(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: Some(Box::new(Expression { position: other.position.clone(), expression: ExpressionItem::Block(mem::take(other)) })) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::IfExpression(Default::default()), &[Terminal(Token::If(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default())), Terminal(Token::Else(Default::default())), NonTerminal(Symbol::IfExpression(Default::default()))],
                        |list| if let [Terminal(Token::If((Range { start, .. }))), NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::Block((_, then))), _, NonTerminal(Symbol::IfExpression((Range { end, .. }, expression)))] = list {
                            Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::BoolExpression(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: Some(Box::new(Expression { position: expression.position.clone(), expression: ExpressionItem::If(mem::take(expression)) })) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::IfExpression(Default::default()), &[Terminal(Token::If(Default::default())), NonTerminal(Symbol::LetBinding(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::If((Range { start, .. }))), NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::Block((Range { end, .. }, then)))] = list {
                            Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::LetBinding(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: None }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::IfExpression(Default::default()), &[Terminal(Token::If(Default::default())), NonTerminal(Symbol::LetBinding(Default::default())), NonTerminal(Symbol::Block(Default::default())), Terminal(Token::Else(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::If((Range { start, .. }))), NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::Block((_, then))), _, NonTerminal(Symbol::Block((Range { end, .. }, other)))] = list {
                            Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::LetBinding(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: Some(Box::new(Expression { position: other.position.clone(), expression: ExpressionItem::Block(mem::take(other)) })) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::IfExpression(Default::default()), &[Terminal(Token::If(Default::default())), NonTerminal(Symbol::LetBinding(Default::default())), NonTerminal(Symbol::Block(Default::default())), Terminal(Token::Else(Default::default())), NonTerminal(Symbol::IfExpression(Default::default()))],
                        |list| if let [Terminal(Token::If((Range { start, .. }))), NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::Block((_, then))), _, NonTerminal(Symbol::IfExpression((Range { end, .. }, other)))] = list {
                            Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::LetBinding(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: Some(Box::new(Expression { position: other.position.clone(), expression: ExpressionItem::If(mem::take(other)) })) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::MatchExpression(Default::default()), &[Terminal(Token::Match(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::MatchPatterns(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Match((Range { start, .. }))), NonTerminal(Symbol::Expression((_, condition))), _, NonTerminal(Symbol::MatchPatterns((_, items))), Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::MatchExpression((*start..*end, Match { position: *start..*end, condition: Box::new(mem::take(condition)), items: mem::take(items) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::MatchExpression(Default::default()), &[Terminal(Token::Match(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::MatchPatterns(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Match((Range { start, .. }))), NonTerminal(Symbol::Expression((_, condition))), _, NonTerminal(Symbol::MatchPatterns((_, items))), _, Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::MatchExpression((*start..*end, Match { position: *start..*end, condition: Box::new(mem::take(condition)), items: mem::take(items) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::MatchPatterns(Default::default()), &[NonTerminal(Symbol::Pattern(Default::default())), Terminal(Token::MatchArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Pattern((Range { start, .. }, pattern))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
                            Symbol::MatchPatterns((*start..*end, vec![(mem::take(pattern), mem::take(expression))]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::MatchPatterns(Default::default()), &[NonTerminal(Symbol::MatchPatterns(Default::default())), Terminal(Token::Comma(Default::default())), NonTerminal(Symbol::Pattern(Default::default())), Terminal(Token::MatchArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::MatchPatterns((Range { start, .. }, patterns))), _, NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
                            Symbol::MatchPatterns((*start..*end, {
                                patterns.push((mem::take(pattern), mem::take(expression)));
                                mem::take(patterns)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ForStatement(Default::default()), &[Terminal(Token::For(Default::default())), NonTerminal(Symbol::Pattern(Default::default())), Terminal(Token::In(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::For((Range { start, .. }))), NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Expression((_, iter))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
                            Symbol::ForStatement((*start..*end, ForStatement { label: None, position: *start..*end, pattern: mem::take(pattern), iter: mem::take(iter), block: mem::take(block) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ForStatement(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), Terminal(Token::For(Default::default())), NonTerminal(Symbol::Pattern(Default::default())), Terminal(Token::In(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((Range { start, .. }, label))), _, _, NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Expression((_, iter))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
                            Symbol::ForStatement((*start..*end, ForStatement { label: Some(label.clone()), position: *start..*end, pattern: mem::take(pattern), iter: mem::take(iter), block: mem::take(block) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::WhileStatement(Default::default()), &[Terminal(Token::While(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::While((Range { start, .. }))), NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
                            Symbol::WhileStatement((*start..*end, WhileStatement { label: None, position: *start..*end, condition: Condition::BoolExpression(mem::take(condition)), block: mem::take(block) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::WhileStatement(Default::default()), &[Terminal(Token::While(Default::default())), NonTerminal(Symbol::LetBinding(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::While((Range { start, .. }))), NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
                            Symbol::WhileStatement((*start..*end, WhileStatement { label: None, position: *start..*end, condition: Condition::LetBinding(mem::take(condition)), block: mem::take(block) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::WhileStatement(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), Terminal(Token::While(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((Range { start, .. }, label))), _, _, NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
                            Symbol::WhileStatement((*start..*end, WhileStatement { label: Some(label.clone()), position: *start..*end, condition: Condition::BoolExpression(mem::take(condition)), block: mem::take(block) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::WhileStatement(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), Terminal(Token::While(Default::default())), NonTerminal(Symbol::LetBinding(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((Range { start, .. }, label))), _, _, NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
                            Symbol::WhileStatement((*start..*end, WhileStatement { label: Some(label.clone()), position: *start..*end, condition: Condition::LetBinding(mem::take(condition)), block: mem::take(block) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Pattern(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((position, pattern)))] = list {
                            Symbol::Pattern((position.clone(), Pattern::Variable(pattern.clone())))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Pattern(Default::default()), &[NonTerminal(Symbol::StructName(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::Pattern((*start..*end, Pattern::Struct(mem::take(name), Vec::new())))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Pattern(Default::default()), &[NonTerminal(Symbol::StructName(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::PatternStructItems(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, NonTerminal(Symbol::PatternStructItems((_, items))), Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::Pattern((*start..*end, Pattern::Struct(mem::take(name), mem::take(items))))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Pattern(Default::default()), &[NonTerminal(Symbol::StructName(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::PatternStructItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, NonTerminal(Symbol::PatternStructItems((_, items))), _, Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::Pattern((*start..*end, Pattern::Struct(mem::take(name), mem::take(items))))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::PatternStructItems(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((position, name)))] = list {
                            Symbol::PatternStructItems((position.clone(), vec![(name.clone(), Pattern::Variable(name.clone()))]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::PatternStructItems(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Pattern(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::Pattern((Range { end, .. }, pattern)))] = list {
                            Symbol::PatternStructItems((*start..*end, vec![(name.clone(), mem::take(pattern))]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::PatternStructItems(Default::default()), &[NonTerminal(Symbol::PatternStructItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [NonTerminal(Symbol::PatternStructItems((Range { start, .. }, items))), _, Terminal(Token::Identifier((Range { end, .. }, name)))] = list {
                            Symbol::PatternStructItems((*start..*end, {
                                items.push((name.clone(), Pattern::Variable(name.clone())));
                                mem::take(items)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::PatternStructItems(Default::default()), &[NonTerminal(Symbol::PatternStructItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Pattern(Default::default()))],
                        |list| if let [NonTerminal(Symbol::PatternStructItems((Range { start, .. }, items))), _, Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::Pattern((Range { end, .. }, pattern)))] = list {
                            Symbol::PatternStructItems((*start..*end, {
                                items.push((name.clone(), mem::take(pattern)));
                                mem::take(items)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ClosureArguments(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((position, name)))] = list {
                            Symbol::ClosureArguments((position.clone(), vec![(name.clone(), None)]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ClosureArguments(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, t)))] = list {
                            Symbol::ClosureArguments((*start..*end, vec![(name.clone(), Some(mem::take(t)))]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ClosureArguments(Default::default()), &[NonTerminal(Symbol::ClosureArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ClosureArguments((Range { start, .. }, list))), _, Terminal(Token::Identifier((Range { end, .. }, name)))] = list {
                            Symbol::ClosureArguments((*start..*end, {
                                list.push((name.clone(), None));
                                mem::take(list)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ClosureArguments(Default::default()), &[NonTerminal(Symbol::ClosureArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ClosureArguments((Range { start, .. }, list))), _, Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, t)))] = list {
                            Symbol::ClosureArguments((*start..*end, {
                                list.push((name.clone(), Some(mem::take(t))));
                                mem::take(list)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::CommaSeparatedExpressions(Default::default()), &[NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Comma(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression((Range { end, .. }, second)))] = list {
                            Symbol::CommaSeparatedExpressions((*start..*end, vec![mem::take(first), mem::take(second)]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::CommaSeparatedExpressions(Default::default()), &[NonTerminal(Symbol::CommaSeparatedExpressions(Default::default())), Terminal(Token::Comma(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::CommaSeparatedExpressions((Range { start, .. }, list))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
                            Symbol::CommaSeparatedExpressions((*start..*end, {
                                list.push(mem::take(expression));
                                mem::take(list)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: Vec::new(), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: Vec::new(), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: Vec::new(), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, _, NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: Vec::new(), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Type((_, t))), Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: mem::take(t), expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: Type::None, expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Type((_, t))), Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: mem::take(t), expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: Type::None, expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), Terminal(Token::Comma(_)), _, _, NonTerminal(Symbol::Type((_, t))), Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: mem::take(t), expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), Terminal(Token::Comma(_)), _, Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: Type::None, expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), Terminal(Token::Comma(_)), _, _, NonTerminal(Symbol::Type((_, t))), Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: mem::take(t), expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), Terminal(Token::Comma(_)), _, Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: Type::None, expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, _, NonTerminal(Symbol::Type((_, t))), Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: Vec::new(), return_type: mem::take(t), expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: Vec::new(), return_type: Type::None, expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, _, _, NonTerminal(Symbol::Type((_, t))), Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: Vec::new(), return_type: mem::take(t), expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn((Range { start, .. }))), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, _, Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: Vec::new(), return_type: Type::None, expression: mem::take(expression) }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionArguments(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, t)))] = list {
                            Symbol::FunctionArguments((*start..*end, vec![(name.clone(), mem::take(t))]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionArguments(Default::default()), &[NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FunctionArguments((Range { start, .. }, list))), _, Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, t)))] = list {
                            Symbol::FunctionArguments((*start..*end, {
                                list.push((name.clone(), mem::take(t)));
                                mem::take(list)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ImportStatement(Default::default()), &[Terminal(Token::Import(Default::default())), NonTerminal(Symbol::ImportPath(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Import((Range { start, .. }))), NonTerminal(Symbol::ImportPath((_, path))), Terminal(Token::Semicolon((Range { end, .. })))] = list {
                            Symbol::ImportStatement((*start..*end, mem::take(path)))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ImportPath(Default::default()), &[NonTerminal(Symbol::ImportPathChild(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ImportPathChild(item))] = list {
                            Symbol::ImportPath(mem::take(item))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ImportPath(Default::default()), &[Terminal(Token::StringImmediate(Default::default())), Terminal(Token::DoubleColon(Default::default())), NonTerminal(Symbol::ImportPathChild(Default::default()))],
                        |list| if let [Terminal(Token::StringImmediate((Range { start, .. }, name))), _, NonTerminal(Symbol::ImportPathChild((Range { end, .. }, path)))] = list {
                            Symbol::ImportPath((*start..*end, ImportPath::Child(name.clone(), Box::new(mem::take(path)))))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ImportPathChild(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((position, name)))] = list {
                            Symbol::ImportPathChild((position.clone(), ImportPath::Item(name.clone())))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ImportPathChild(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), NonTerminal(Symbol::ImportPathChild(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::ImportPathChild((Range { end, .. }, path)))] = list {
                            Symbol::ImportPathChild((*start..*end, ImportPath::Child(name.clone(), Box::new(mem::take(path)))))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ImportPathChild(Default::default()), &[Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedImportPathChildren(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::WaveBracketOpen((Range { start, .. }))), NonTerminal(Symbol::CommaSeparatedImportPathChildren((_, paths))), Terminal(Token::WaveBracketClose((Range { end, .. })))] = list {
                            Symbol::ImportPathChild((*start..*end, ImportPath::List(mem::take(paths))))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::CommaSeparatedImportPathChildren(Default::default()), &[NonTerminal(Symbol::ImportPathChild(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ImportPathChild((position, path)))] = list {
                            Symbol::CommaSeparatedImportPathChildren((position.clone(), vec![mem::take(path)]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::CommaSeparatedImportPathChildren(Default::default()), &[NonTerminal(Symbol::CommaSeparatedImportPathChildren(Default::default())), Terminal(Token::Comma(Default::default())), NonTerminal(Symbol::ImportPathChild(Default::default()))],
                        |list| if let [NonTerminal(Symbol::CommaSeparatedImportPathChildren((Range { start, .. }, list))), _, NonTerminal(Symbol::ImportPathChild((Range { end, .. }, path)))] = list {
                            Symbol::CommaSeparatedImportPathChildren((*start..*end, {
                                list.push(mem::take(path));
                                mem::take(list)
                            }))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::GenericsTypeArguments(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier((position, name)))] = list {
                            Symbol::GenericsTypeArguments((position.clone(), vec![name.clone()]))
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::GenericsTypeArguments(Default::default()), &[NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [NonTerminal(Symbol::GenericsTypeArguments((Range { start, .. }, list))), _, Terminal(Token::Identifier((Range { end, .. }, name)))] = list {
                            Symbol::GenericsTypeArguments((*start..*end, {
                                list.push(name.clone());
                                mem::take(list)
                            }))
                        } else { unreachable!() }))
        .build(Symbol::ValidKirlCode(Default::default()))
}

pub fn get_parser() -> LR1Parser<Symbol, Token> {
    LR1Parser::new(get_syntax()).0
}

#[cfg(test)]
mod tests {
    use parser::LR1Parser;

    use crate::parser::{get_parser, get_syntax};

    #[test]
    fn test_parse() {
        let (parser, warning) = LR1Parser::new(get_syntax());
        assert_eq!(warning.len(), 0, "{}", warning.into_iter().map(|w| format!("{:?}", w)).collect::<Vec<_>>().join("\n"));
    }
}
