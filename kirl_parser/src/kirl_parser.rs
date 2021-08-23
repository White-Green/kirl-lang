use std::mem;
use std::ops::Range;

use parser::enum_index;
use parser::enum_index_derive::*;
use parser::Symbol::{NonTerminal, Terminal};
use parser_generator::parser;

use crate::kirl_tokenizer::Token;
use crate::CharacterPosition;
use dec::Decimal128;

#[cfg(test)]
mod tests;

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Statement {
    pub position: Range<CharacterPosition>,
    pub statement: StatementItem,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementItem {
    Import(ImportPath),
    Expression(Expression),
    LetBinding(LetBinding),
    Return(Option<Expression>),
    Continue(Option<String>),
    Break(Option<String>),
    For(ForStatement),
    While(WhileStatement),
}

impl Default for StatementItem {
    fn default() -> Self {
        StatementItem::Return(None)
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct ForStatement {
    pub label: Option<String>,
    pub position: Range<CharacterPosition>,
    pub pattern: Pattern,
    pub iter: Expression,
    pub block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Condition {
    BoolExpression(Expression),
    LetBinding(LetBinding),
}

impl Default for Condition {
    fn default() -> Self {
        Condition::BoolExpression(Default::default())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct WhileStatement {
    pub label: Option<String>,
    pub position: Range<CharacterPosition>,
    pub condition: Condition,
    pub block: Block,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Block {
    pub position: Range<CharacterPosition>,
    pub statements: Vec<Statement>,
    pub last_expression: Option<Box<Expression>>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Function {
    pub position: Range<CharacterPosition>,
    pub name: String,
    pub generics_arguments: Vec<String>,
    pub arguments: Vec<(String, Type)>,
    pub return_type: Type,
    pub expression: Expression,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Struct {
    pub name: String,
    pub generics_arguments: Vec<String>,
    pub members: Vec<(String, Type)>,
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Path {
    pub position: Range<CharacterPosition>,
    pub path: Vec<String>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct NamedType {
    pub position: Range<CharacterPosition>,
    pub path: Vec<String>,
    pub generics_arguments: Vec<Type>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct FunctionType {
    pub position: Range<CharacterPosition>,
    pub argument: Vec<Type>,
    pub result: Box<Type>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct AnonymousStructType {
    pub position: Range<CharacterPosition>,
    pub members: Vec<(String, Type)>,
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum StructName {
    Anonymous,
    Named(NamedType),
}

impl Default for StructName {
    fn default() -> Self {
        StructName::Anonymous
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Expression {
    pub position: Range<CharacterPosition>,
    pub expression: ExpressionItem,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionItem {
    AccessVariable(Path),
    StringImmediate(String),
    NumberImmediate(Decimal128),
    AccessMember(Box<Expression>, String),
    CallFunction(Option<Box<Expression>>, Option<Path>, Vec<Expression>),
    Indexer(Box<Expression>, Box<Expression>),
    ConstructTuple(Vec<Expression>),
    ConstructArray(Vec<Expression>),
    ConstructStruct(ConstructStruct),
    Block(Block),
    Neg(Box<Expression>),
    // _neg
    Not(Box<Expression>),
    // _not
    Mul(Box<Expression>, Box<Expression>),
    // _mul
    Div(Box<Expression>, Box<Expression>),
    // _div
    Rem(Box<Expression>, Box<Expression>),
    // _rem
    Add(Box<Expression>, Box<Expression>),
    // _add
    Sub(Box<Expression>, Box<Expression>),
    // _sub
    GreaterThan(Box<Expression>, Box<Expression>),
    // _gt
    LessThan(Box<Expression>, Box<Expression>),
    // _gt (左右逆)
    GreaterOrEqual(Box<Expression>, Box<Expression>),
    // _not(_gt (左右逆))
    LessOrEqual(Box<Expression>, Box<Expression>),
    // _not(_gt)
    Equals(Box<Expression>, Box<Expression>),
    // _eq
    NotEquals(Box<Expression>, Box<Expression>),
    // _not(_eq)
    And(Box<Expression>, Box<Expression>),
    // _and
    Xor(Box<Expression>, Box<Expression>),
    // _xor
    Or(Box<Expression>, Box<Expression>),
    // _or
    Assign(Box<Expression>, Box<Expression>),
    Closure(Closure),
    If(If),
    Match(Match),
}

impl Default for ExpressionItem {
    fn default() -> Self {
        ExpressionItem::NumberImmediate(Default::default())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct ConstructStruct {
    pub name: StructName,
    pub items: Vec<(String, Expression)>,
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Variable(String),
    Struct(StructName, Vec<(String, Pattern)>),
}

impl Default for Pattern {
    fn default() -> Self {
        Pattern::Variable(Default::default())
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct LetBinding {
    pub position: Range<CharacterPosition>,
    pub pattern: Pattern,
    pub type_hint: Option<Type>,
    pub expression: Box<Expression>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Closure {
    pub position: Range<CharacterPosition>,
    pub arguments: Vec<(String, Option<Type>)>,
    pub expression: Box<Expression>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct If {
    pub position: Range<CharacterPosition>,
    pub condition: Box<Condition>,
    pub then: Box<Expression>,
    pub other: Option<Box<Expression>>,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Match {
    pub position: Range<CharacterPosition>,
    pub condition: Box<Expression>,
    pub items: Vec<(Pattern, Expression)>,
}

#[derive(Debug, EnumIndex, PartialEq, Clone)]
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

parser! {
    token Token{
        Import(Default::default(),),
        Fn(Default::default(),),
        Struct(Default::default(),),
        Let(Default::default(),),
        Var(Default::default(),),
        If(Default::default(),),
        Else(Default::default(),),
        Match(Default::default(),),
        For(Default::default(),),
        In(Default::default(),),
        While(Default::default(),),
        Return(Default::default(),),
        Break(Default::default(),),
        Continue(Default::default(),),
        Identifier(Default::default(),),
        StringImmediate(Default::default(),),
        NumberImmediate(Default::default(),),
        Not(Default::default(),),
        Dot(Default::default(),),
        Comma(Default::default(),),
        Sharp(Default::default(),),
        DoubleColon(Default::default(),),
        Colon(Default::default(),),
        Semicolon(Default::default(),),
        GreaterThan(Default::default(),),
        LessThan(Default::default(),),
        GreaterOrEqual(Default::default(),),
        LessOrEqual(Default::default(),),
        Assign(Default::default(),),
        Equals(Default::default(),),
        NotEquals(Default::default(),),
        Add(Default::default(),),
        Sub(Default::default(),),
        Mul(Default::default(),),
        Div(Default::default(),),
        Rem(Default::default(),),
        And(Default::default(),),
        Or(Default::default(),),
        Xor(Default::default(),),
        RoundBracketOpen(Default::default(),),
        RoundBracketClose(Default::default(),),
        SquareBracketOpen(Default::default(),),
        SquareBracketClose(Default::default(),),
        WaveBracketOpen(Default::default(),),
        WaveBracketClose(Default::default(),),
        FunctionArrow(Default::default(),),
        MatchArrow(Default::default(),),
    }
    symbol Symbol{
        ValidKirlCode(Default::default(),),
        Type(Default::default(),),
        FullPath(Default::default(),),
        CommaSeparatedTypes(Default::default(),),
        OrSeparatedTypes(Default::default(),),
        StructDefinitionItems(Default::default(),),
        ConstructStruct(Default::default(),),
        StructName(Default::default(),),
        StructDefinition(Default::default(),),
        ConstructStructItems(Default::default(),),
        LetBinding(Default::default(),),
        Statement(Default::default(),),
        StatementList(Default::default(),),
        Block(Default::default(),),
        Expression8(Default::default(),),
        Expression7(Default::default(),),
        Expression6(Default::default(),),
        Expression5(Default::default(),),
        Expression4(Default::default(),),
        Expression3(Default::default(),),
        Expression2(Default::default(),),
        Expression1(Default::default(),),
        Expression0(Default::default(),),
        Expression(Default::default(),),
        ClosureExpression(Default::default(),),
        IfExpression(Default::default(),),
        MatchExpression(Default::default(),),
        MatchPatterns(Default::default(),),
        ForStatement(Default::default(),),
        WhileStatement(Default::default(),),
        Pattern(Default::default(),),
        PatternStructItems(Default::default(),),
        ClosureArguments(Default::default(),),
        CommaSeparatedExpressions(Default::default(),),
        FunctionDefinition(Default::default(),),
        FunctionArguments(Default::default(),),
        ImportStatement(Default::default(),),
        ImportPath(Default::default(),),
        ImportPathChild(Default::default(),),
        CommaSeparatedImportPathChildren(Default::default(),),
        GenericsTypeArguments(Default::default(),),
    }
    ValidKirlCode
    <ValidKirlCode>::=()
        (|list| if let [] = list {
            Symbol::ValidKirlCode(Default::default())
        } else { unreachable!() })
    <ValidKirlCode>::=(<ValidKirlCode> <Statement>)
        (|list| match list {
            [NonTerminal(Symbol::ValidKirlCode((_, list))), NonTerminal(Symbol::Statement((range, statement)))] if list.is_empty() => Symbol::ValidKirlCode((range.clone(), vec![KirlTopLevelStatement::Statement((mem::take(range), mem::take(statement)))])),
            [NonTerminal(Symbol::ValidKirlCode((Range { start, .. }, list))), NonTerminal(Symbol::Statement((range, statement)))] => Symbol::ValidKirlCode((*start..range.end, {
                list.push(KirlTopLevelStatement::Statement((mem::take(range), mem::take(statement))));
                mem::take(list)
            })),
            _ => unreachable!(),
        })
    <ValidKirlCode>::=(<ValidKirlCode> <FunctionDefinition>)
        (|list| match list {
            [NonTerminal(Symbol::ValidKirlCode((_, list))), NonTerminal(Symbol::FunctionDefinition((range, function)))] if list.is_empty() => Symbol::ValidKirlCode((range.clone(), vec![KirlTopLevelStatement::FunctionDefinition((mem::take(range), mem::take(function)))])),
            [NonTerminal(Symbol::ValidKirlCode((Range { start, .. }, list))), NonTerminal(Symbol::FunctionDefinition((range, function)))] => Symbol::ValidKirlCode((*start..range.end, {
                list.push(KirlTopLevelStatement::FunctionDefinition((mem::take(range), mem::take(function))));
                mem::take(list)
            })),
            _ => unreachable!(),
        })
    <ValidKirlCode>::=(<ValidKirlCode> <StructDefinition>)
        (|list| match list {
            [NonTerminal(Symbol::ValidKirlCode((_, list))), NonTerminal(Symbol::StructDefinition((range, s)))] if list.is_empty() => Symbol::ValidKirlCode((range.clone(), vec![KirlTopLevelStatement::StructDefinition((mem::take(range), mem::take(s)))])),
            [NonTerminal(Symbol::ValidKirlCode((Range { start, .. }, list))), NonTerminal(Symbol::StructDefinition((range, s)))] => Symbol::ValidKirlCode((*start..range.end, {
                list.push(KirlTopLevelStatement::StructDefinition((mem::take(range), mem::take(s))));
                mem::take(list)
            })),
            _ => unreachable!(),
        })
    <Type>::=([Identifier])
        (|list| if let [Terminal(Token::Identifier((position, ident)))] = list {
            Symbol::Type((position.clone(), Type::NamedType(NamedType { position: position.clone(), path: vec![ident.clone()], generics_arguments: Vec::new() })))
        } else { unreachable!() })
    <Type>::=([Not])
        (|list| if let [Terminal(Token::Not(position))] = list {
            Symbol::Type((position.clone(), Type::Unreachable(position.clone())))
        } else { unreachable!() })
    <Type>::=([Identifier] [DoubleColon] [LessThan] <CommaSeparatedTypes> [GreaterThan])
        (|list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, _, NonTerminal(Symbol::CommaSeparatedTypes((_, types))), Terminal(Token::GreaterThan(Range { end, .. }))] = list {
            Symbol::Type((*start..*end, Type::NamedType(NamedType { position: *start..*end, path: vec![name.clone()], generics_arguments: mem::take(types) })))
        } else { unreachable!() })
    <Type>::=(<FullPath>)
        (|list| if let [NonTerminal(Symbol::FullPath((symbol_position, Path { position, path })))] = list {
            Symbol::Type((mem::take(symbol_position), Type::NamedType(NamedType {
                position: position.clone(),
                path: mem::take(path),
                generics_arguments: Vec::new(),
            })))
        } else { unreachable!() })
    <Type>::=(<FullPath> [DoubleColon] [LessThan] <CommaSeparatedTypes> [GreaterThan])
        (|list| if let [NonTerminal(Symbol::FullPath((Range { start, .. }, Path { path, .. }))), _, _, NonTerminal(Symbol::CommaSeparatedTypes((_, types))), Terminal(Token::GreaterThan(Range { end, .. }))] = list {
            Symbol::Type((*start..*end, Type::NamedType(NamedType {
                position: *start..*end,
                path: mem::take(path),
                generics_arguments: mem::take(types),
            })))
        } else { unreachable!() })
    <Type>::=([SquareBracketOpen] <Type> [SquareBracketClose])
        (|list| if let [Terminal(Token::SquareBracketOpen(Range { start, .. })), NonTerminal(Symbol::Type((_, inner_type))), Terminal(Token::SquareBracketClose(Range { end, .. }))] = list {
            Symbol::Type((*start..*end, Type::Array(*start..*end, Box::new(mem::take(inner_type)))))
        } else { unreachable!() })
    <Type>::=([RoundBracketOpen] <Type> [RoundBracketClose])
        (|list| if let [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::Type((_, inner_type))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Type((*start..*end, mem::take(inner_type)))
        } else { unreachable!() })
    <Type>::=([RoundBracketOpen] <Type> [Comma] [RoundBracketClose])
        (|list| if let [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::Type((_, inner_type))), _, Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Type((*start..*end, Type::Tuple(*start..*end, vec![mem::take(inner_type)])))
        } else { unreachable!() })
    <Type>::=([RoundBracketOpen] <CommaSeparatedTypes> [RoundBracketClose])
        (|list| if let [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedTypes((_, types))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Type((*start..*end, Type::Tuple(*start..*end, mem::take(types))))
        } else { unreachable!() })
    <Type>::=([Sharp] [WaveBracketOpen] <StructDefinitionItems> [WaveBracketClose])
        (|list| if let [Terminal(Token::Sharp(Range { start, .. })), _, NonTerminal(Symbol::StructDefinitionItems((_, members))), Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::Type((*start..*end, Type::AnonymousStruct(AnonymousStructType { position: *start..*end, members: mem::take(members) })))
        } else { unreachable!() })
    <Type>::=([RoundBracketOpen] [RoundBracketClose] [FunctionArrow] <Type>)
        (|list| if let [Terminal(Token::RoundBracketOpen(Range { start, .. })), _, _, NonTerminal(Symbol::Type((Range { end, .. }, return_type)))] = list {
            Symbol::Type((*start..*end, Type::Function(FunctionType {
                position: *start..*end,
                argument: Vec::new(),
                result: Box::new(mem::take(return_type)),
            })))
        } else { unreachable!() })
    <Type>::=([RoundBracketOpen] <Type> [RoundBracketClose] [FunctionArrow] <Type>)
        (|list| if let [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::Type((_, argument_type))), _, _, NonTerminal(Symbol::Type((Range { end, .. }, return_type)))] = list {
            Symbol::Type((*start..*end, Type::Function(FunctionType {
                position: *start..*end,
                argument: vec![mem::take(argument_type)],
                result: Box::new(mem::take(return_type)),
            })))
        } else { unreachable!() })
    <Type>::=([RoundBracketOpen] <CommaSeparatedTypes> [RoundBracketClose] [FunctionArrow] <Type>)
        (|list| if let [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedTypes((_, arguments))), _, _, NonTerminal(Symbol::Type((Range { end, .. }, return_type)))] = list {
            Symbol::Type((*start..*end, Type::Function(FunctionType {
                position: *start..*end,
                argument: mem::take(arguments),
                result: Box::new(mem::take(return_type)),
            })))
        } else { unreachable!() })
    <Type>::=([RoundBracketOpen] <OrSeparatedTypes> [RoundBracketClose])
        (|list| if let [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::OrSeparatedTypes((_, types))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Type((*start..*end, Type::Or(*start..*end, mem::take(types))))
        } else { unreachable!() })
    <FullPath>::=([Identifier] [DoubleColon] [Identifier])
        (|list| if let [Terminal(Token::Identifier((Range { start, .. }, first))), _, Terminal(Token::Identifier((Range { end, .. }, second)))] = list {
            Symbol::FullPath((*start..*end, Path { position: *start..*end, path: vec![first.clone(), second.clone()] }))
        } else { unreachable!() })
    <FullPath>::=(<FullPath> [DoubleColon] [Identifier])
        (|list| if let [NonTerminal(Symbol::FullPath((Range { start, .. }, Path { path, .. }))), _, Terminal(Token::Identifier((Range { end, .. }, last)))] = list {
            Symbol::FullPath((*start..*end, Path {
                position: *start..*end,
                path: {
                    path.push(last.clone());
                    mem::take(path)
                },
            }))
        } else { unreachable!() })
    <CommaSeparatedTypes>::=(<Type> [Comma] <Type>)
        (|list| if let [NonTerminal(Symbol::Type((Range { start, .. }, first))), _, NonTerminal(Symbol::Type((Range { end, .. }, second)))] = list {
            Symbol::CommaSeparatedTypes((*start..*end, vec![mem::take(first), mem::take(second)]))
        } else { unreachable!() })
    <CommaSeparatedTypes>::=(<CommaSeparatedTypes> [Comma] <Type>)
        (|list| if let [NonTerminal(Symbol::CommaSeparatedTypes((Range { start, .. }, types))), _, NonTerminal(Symbol::Type((Range { end, .. }, last)))] = list {
            Symbol::CommaSeparatedTypes((*start..*end, {
                types.push(mem::take(last));
                mem::take(types)
            }))
        } else { unreachable!() })
    <OrSeparatedTypes>::=(<Type> [Or] <Type>)
        (|list| if let [NonTerminal(Symbol::Type((Range { start, .. }, first))), _, NonTerminal(Symbol::Type((Range { end, .. }, second)))] = list {
            Symbol::OrSeparatedTypes((*start..*end, vec![mem::take(first), mem::take(second)]))
        } else { unreachable!() })
    <OrSeparatedTypes>::=(<OrSeparatedTypes> [Or] <Type>)
        (|list| if let [NonTerminal(Symbol::OrSeparatedTypes((Range { start, .. }, types))), _, NonTerminal(Symbol::Type((Range { end, .. }, last)))] = list {
            Symbol::OrSeparatedTypes((*start..*end, {
                types.push(mem::take(last));
                mem::take(types)
            }))
        } else { unreachable!() })
    <StructDefinitionItems>::=([Identifier] [Colon] <Type>)
        (|list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, item_type)))] = list {
            Symbol::StructDefinitionItems((*start..*end, vec![(name.clone(), mem::take(item_type))]))
        } else { unreachable!() })
    <StructDefinitionItems>::=(<StructDefinitionItems> [Comma] [Identifier] [Colon] <Type>)
        (|list| if let [NonTerminal(Symbol::StructDefinitionItems((Range { start, .. }, list))), _, Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, item_type)))] = list {
            Symbol::StructDefinitionItems((*start..*end, {
                list.push((name.clone(), mem::take(item_type)));
                mem::take(list)
            }))
        } else { unreachable!() })
    <ConstructStruct>::=(<StructName> [WaveBracketOpen] <ConstructStructItems> [WaveBracketClose])
        (|list| if let [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, NonTerminal(Symbol::ConstructStructItems((_, items))), Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::ConstructStruct((*start..*end, ConstructStruct { name: mem::take(name), items: mem::take(items) }))
        } else { unreachable!() })
    <ConstructStruct>::=(<StructName> [WaveBracketOpen] <ConstructStructItems> [Comma] [WaveBracketClose])
        (|list| if let [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, NonTerminal(Symbol::ConstructStructItems((_, items))), _, Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::ConstructStruct((*start..*end, ConstructStruct { name: mem::take(name), items: mem::take(items) }))
        } else { unreachable!() })
    <ConstructStruct>::=(<StructName> [WaveBracketOpen] [WaveBracketClose])
        (|list| if let [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::ConstructStruct((*start..*end, ConstructStruct { name: mem::take(name), items: Vec::new() }))
        } else { unreachable!() })
    // <StructName>::=([Identifier])
    //     (|list| if let [Terminal(Token::Identifier((position, name)))] = list {
    //                     Symbol::StructName((position.clone(), StructName::Named(NamedType { position: position.clone(), path: vec![name.clone()], generics_arguments: Vec::new() })))
    //                 } else { unreachable!() })
    // <StructName>::=([Identifier] [DoubleColon] [LessThan] <CommaSeparatedTypes> [GreaterThan])
    //     (|list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, _, NonTerminal(Symbol::CommaSeparatedTypes((_, types))), Terminal(Token::GreaterThan(Range { end, .. }))] = list {
    //                     Symbol::StructName((*start..*end, StructName::Named(NamedType { position: *start..*end, path: vec![name.clone()], generics_arguments: mem::take(types) })))
    //                 } else { unreachable!() })
    // <StructName>::=(<FullPath>)
    //     (|list| if let [NonTerminal(Symbol::FullPath((position, path)))] = list {
    //                     Symbol::StructName((position.clone(), StructName::Named(NamedType { position: position.clone(), path: mem::take(&mut path.path), generics_arguments: Vec::new() })))
    //                 } else { unreachable!() })
    // <StructName>::=(<FullPath> [DoubleColon] [LessThan] <CommaSeparatedTypes> [GreaterThan])
    //     (|list| if let [NonTerminal(Symbol::FullPath((Range { start, .. }, path))), _, _, NonTerminal(Symbol::CommaSeparatedTypes((_, types))), Terminal(Token::GreaterThan(Range { end, .. }))] = list {
    //                     Symbol::StructName((*start..*end, StructName::Named(NamedType { position: *start..*end, path: mem::take(&mut path.path), generics_arguments: mem::take(types) })))
    //                 } else { unreachable!() })
    <StructName>::=([Sharp])
        (|list| if let [Terminal(Token::Sharp(position))] = list {
            Symbol::StructName((position.clone(), StructName::Anonymous))
        } else { unreachable!() })
    <StructDefinition>::=([Struct] [Identifier] [WaveBracketOpen] [WaveBracketClose])
        (|list| if let [Terminal(Token::Struct(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::StructDefinition((*start..*end, Struct { name: name.clone(), generics_arguments: Vec::new(), members: Vec::new() }))
        } else { unreachable!() })
    <StructDefinition>::=([Struct] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [WaveBracketOpen] [WaveBracketClose])
        (|list| if let [Terminal(Token::Struct(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, types))), _, _, Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::StructDefinition((*start..*end, Struct { name: name.clone(), generics_arguments: mem::take(types), members: Vec::new() }))
        } else { unreachable!() })
    <StructDefinition>::=([Struct] [Identifier] [WaveBracketOpen] <StructDefinitionItems> [WaveBracketClose])
        (|list| if let [Terminal(Token::Struct(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::StructDefinitionItems((_, items))), Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::StructDefinition((*start..*end, Struct { name: name.clone(), generics_arguments: Vec::new(), members: mem::take(items) }))
        } else { unreachable!() })
    <StructDefinition>::=([Struct] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [WaveBracketOpen] <StructDefinitionItems> [WaveBracketClose])
        (|list| if let [Terminal(Token::Struct(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, types))), _, _, NonTerminal(Symbol::StructDefinitionItems((_, items))), Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::StructDefinition((*start..*end, Struct { name: name.clone(), generics_arguments: mem::take(types), members: mem::take(items) }))
        } else { unreachable!() })
    <ConstructStructItems>::=([Identifier] [Colon] <Expression>)
        (|list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
            Symbol::ConstructStructItems((*start..*end, vec![(name.clone(), mem::take(expression))]))
        } else { unreachable!() })
    <ConstructStructItems>::=([Identifier])
        (|list| if let [Terminal(Token::Identifier((position, name)))] = list {
            Symbol::ConstructStructItems((position.clone(), vec![(name.clone(), Expression { position: position.clone(), expression: ExpressionItem::AccessVariable(Path { position: position.clone(), path: vec![name.clone()] }) })]))
        } else { unreachable!() })
    <ConstructStructItems>::=(<ConstructStructItems> [Comma] [Identifier] [Colon] <Expression>)
        (|list| if let [NonTerminal(Symbol::ConstructStructItems((Range { start, .. }, items))), _, Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
            Symbol::ConstructStructItems((*start..*end, {
                items.push((name.clone(), mem::take(expression)));
                mem::take(items)
            }))
        } else { unreachable!() })
    <ConstructStructItems>::=(<ConstructStructItems> [Comma] [Identifier])
        (|list| if let [NonTerminal(Symbol::ConstructStructItems((Range { start, .. }, items))), _, Terminal(Token::Identifier((position, name)))] = list {
            Symbol::ConstructStructItems((*start..position.end, {
                items.push((name.clone(), Expression {
                    position: position.clone(),
                    expression: ExpressionItem::AccessVariable(Path { position: position.clone(), path: vec![name.clone()] }),
                }));
                mem::take(items)
            }))
        } else { unreachable!() })
    <LetBinding>::=([Var] <Pattern> [Assign] <Expression>)
        (|list| if let [Terminal(Token::Var(Range { start, .. })), NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
            Symbol::LetBinding((*start..*end, LetBinding { position: *start..*end, pattern: mem::take(pattern), type_hint: None, expression: Box::new(mem::take(expression)) }))
        } else { unreachable!() })
    <LetBinding>::=([Var] <Pattern> [Colon] <Type> [Assign] <Expression>)
        (|list| if let [Terminal(Token::Var(Range { start, .. })), NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Type((_, type_hint))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
            Symbol::LetBinding((*start..*end, LetBinding { position: *start..*end, pattern: mem::take(pattern), type_hint: Some(mem::take(type_hint)), expression: Box::new(mem::take(expression)) }))
        } else { unreachable!() })
    <Statement>::=(<ImportStatement>)
        (|list| if let [NonTerminal(Symbol::ImportStatement((position, path)))] = list {
            Symbol::Statement((position.clone(), Statement { position: position.clone(), statement: StatementItem::Import(mem::take(path)) }))
        } else { unreachable!() })
    <Statement>::=(<Expression> [Semicolon])
        (|list| if let [NonTerminal(Symbol::Expression((Range { start, .. }, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Expression(mem::take(expression)) }))
        } else { unreachable!() })
    <Statement>::=(<LetBinding> [Semicolon])
        (|list| if let [NonTerminal(Symbol::LetBinding((Range { start, .. }, let_binding))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::LetBinding(mem::take(let_binding)) }))
        } else { unreachable!() })
    <Statement>::=([Return] <Expression> [Semicolon])
        (|list| if let [Terminal(Token::Return(Range { start, .. })), NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Return(Some(mem::take(expression))) }))
        } else { unreachable!() })
    <Statement>::=([Return] [Semicolon])
        (|list| if let [Terminal(Token::Return(Range { start, .. })), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Return(None) }))
        } else { unreachable!() })
    <Statement>::=([Continue] [Semicolon])
        (|list| if let [Terminal(Token::Continue(Range { start, .. })), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Continue(None) }))
        } else { unreachable!() })
    <Statement>::=([Continue] [Identifier] [Semicolon])
        (|list| if let [Terminal(Token::Continue(Range { start, .. })), Terminal(Token::Identifier((_, label))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Continue(Some(label.clone())) }))
        } else { unreachable!() })
    <Statement>::=([Break] [Semicolon])
        (|list| if let [Terminal(Token::Break(Range { start, .. })), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Break(None) }))
        } else { unreachable!() })
    <Statement>::=([Break] [Identifier] [Semicolon])
        (|list| if let [Terminal(Token::Break(Range { start, .. })), Terminal(Token::Identifier((_, label))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::Statement((*start..*end, Statement { position: *start..*end, statement: StatementItem::Break(Some(label.clone())) }))
        } else { unreachable!() })
    <Statement>::=(<ForStatement>)
        (|list| if let [NonTerminal(Symbol::ForStatement((position, for_statement)))] = list {
            Symbol::Statement((position.clone(), Statement { position: position.clone(), statement: StatementItem::For(mem::take(for_statement)) }))
        } else { unreachable!() })
    <Statement>::=(<WhileStatement>)
        (|list| if let [NonTerminal(Symbol::WhileStatement((position, while_statement)))] = list {
            Symbol::Statement((position.clone(), Statement { position: position.clone(), statement: StatementItem::While(mem::take(while_statement)) }))
        } else { unreachable!() })
    <StatementList>::=(<Statement>)
        (|list| if let [NonTerminal(Symbol::Statement((position, statement)))] = list {
            Symbol::StatementList((position.clone(), vec![mem::take(statement)]))
        } else { unreachable!() })
    <StatementList>::=(<StatementList> <Statement>)
        (|list| if let [NonTerminal(Symbol::StatementList((Range { start, .. }, list))), NonTerminal(Symbol::Statement((Range { end, .. }, statement)))] = list {
            Symbol::StatementList((*start..*end, {
                list.push(mem::take(statement));
                mem::take(list)
            }))
        } else { unreachable!() })
    <Block>::=([WaveBracketOpen] <StatementList> <Expression> [WaveBracketClose])
        (|list| if let [Terminal(Token::WaveBracketOpen(Range { start, .. })), NonTerminal(Symbol::StatementList((_, list))), NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::Block((*start..*end, Block {
                position: *start..*end,
                statements: mem::take(list),
                last_expression: Some(Box::new(mem::take(expression))),
            }))
        } else { unreachable!() })
    <Block>::=([WaveBracketOpen] <StatementList> [WaveBracketClose])
        (|list| if let [Terminal(Token::WaveBracketOpen(Range { start, .. })), NonTerminal(Symbol::StatementList((_, list))), Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::Block((*start..*end, Block {
                position: *start..*end,
                statements: mem::take(list),
                last_expression: None,
            }))
        } else { unreachable!() })
    <Block>::=([WaveBracketOpen] <Expression> [WaveBracketClose])
        (|list| if let [Terminal(Token::WaveBracketOpen(Range { start, .. })), NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::Block((*start..*end, Block {
                position: *start..*end,
                statements: Vec::new(),
                last_expression: Some(Box::new(mem::take(expression))),
            }))
        } else { unreachable!() })
    <Block>::=([WaveBracketOpen] [WaveBracketClose])
        (|list| if let [Terminal(Token::WaveBracketOpen(Range { start, .. })), Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::Block((*start..*end, Block {
                position: *start..*end,
                statements: Vec::new(),
                last_expression: None,
            }))
        } else { unreachable!() })
    <Expression8>::=([Identifier])
        (|list| if let [Terminal(Token::Identifier((position, variable)))] = list {
            Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::AccessVariable(Path { position: position.clone(), path: vec![variable.clone()] }) }))
        } else { unreachable!() })
    <Expression8>::=([StringImmediate])
        (|list| if let [Terminal(Token::StringImmediate((position, value)))] = list {
            Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::StringImmediate(value.clone()) }))
        } else { unreachable!() })
    <Expression8>::=([NumberImmediate])
        (|list| if let [Terminal(Token::NumberImmediate((position, value)))] = list {
            Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::NumberImmediate(*value) }))
        } else { unreachable!() })
    <Expression8>::=(<Expression8> [Dot] <FullPath> [RoundBracketOpen] [RoundBracketClose])
        (|list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, NonTerminal(Symbol::FullPath((_, path))), _, Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Some(Box::new(mem::take(expression))), Some(mem::take(path)), Vec::new()) }))
        } else { unreachable!() })
    <Expression8>::=(<Expression8> [Dot] [Identifier] [RoundBracketOpen] [RoundBracketClose])
        (|list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, Terminal(Token::Identifier((position, path))), _, Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Some(Box::new(mem::take(expression))), Some(Path { position: position.clone(), path: vec![path.clone()] }), Vec::new()) }))
        } else { unreachable!() })
    <Expression8>::=(<FullPath> [RoundBracketOpen] [RoundBracketClose])
        (|list| if let [NonTerminal(Symbol::FullPath((Range { start, .. }, path))), _, Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(None, Some(mem::take(path)), Vec::new()) }))
        } else { unreachable!() })
    <Expression8>::=([Identifier] [RoundBracketOpen] [RoundBracketClose])
        (|list| if let [Terminal(Token::Identifier((position, path))), _, Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((position.start..*end, Expression { position: position.start..*end, expression: ExpressionItem::CallFunction(None, Some(Path { position: position.clone(), path: vec![path.clone()] }), Vec::new()) }))
        } else { unreachable!() })
    <Expression8>::=(<Expression8> [Dot] [RoundBracketOpen] [RoundBracketClose])
        (|list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, _, Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Some(Box::new(mem::take(expression))), None, Vec::new()) }))
        } else { unreachable!() })
    <Expression8>::=(<Expression8> [Dot] <FullPath> [RoundBracketOpen] <Expression> [RoundBracketClose])
        (|list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, NonTerminal(Symbol::FullPath((_, path))), _, NonTerminal(Symbol::Expression((_, argument))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Some(Box::new(mem::take(expression))), Some(mem::take(path)), vec![mem::take(argument)]) }))
        } else { unreachable!() })
    <Expression8>::=(<Expression8> [Dot] [Identifier] [RoundBracketOpen] <Expression> [RoundBracketClose])
        (|list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, Terminal(Token::Identifier((position, path))), _, NonTerminal(Symbol::Expression((_, argument))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Some(Box::new(mem::take(expression))), Some(Path { position: position.clone(), path: vec![path.clone()] }), vec![mem::take(argument)]) }))
        } else { unreachable!() })
    <Expression8>::=(<FullPath> [RoundBracketOpen] <Expression> [RoundBracketClose])
        (|list| if let [NonTerminal(Symbol::FullPath((Range { start, .. }, path))), _, NonTerminal(Symbol::Expression((_, argument))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(None, Some(mem::take(path)), vec![mem::take(argument)]) }))
        } else { unreachable!() })
    <Expression8>::=([Identifier] [RoundBracketOpen] <Expression> [RoundBracketClose])
        (|list| if let [Terminal(Token::Identifier((position, path))), _, NonTerminal(Symbol::Expression((_, argument))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((position.start..*end, Expression { position: position.start..*end, expression: ExpressionItem::CallFunction(None, Some(Path { position: position.clone(), path: vec![path.clone()] }), vec![mem::take(argument)]) }))
        } else { unreachable!() })
    <Expression8>::=(<Expression8> [Dot] [RoundBracketOpen] <Expression> [RoundBracketClose])
        (|list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, _, NonTerminal(Symbol::Expression((_, argument))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Some(Box::new(mem::take(expression))), None, vec![mem::take(argument)]) }))
        } else { unreachable!() })
    <Expression8>::=(<Expression8> [Dot] [Identifier])
        (|list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, Terminal(Token::Identifier((Range { end, .. }, member)))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::AccessMember(Box::new(mem::take(expression)), member.clone()) }))
        } else { unreachable!() })
    <Expression8>::=(<Expression8> [Dot] <FullPath> [RoundBracketOpen] <CommaSeparatedExpressions> [RoundBracketClose])
        (|list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, NonTerminal(Symbol::FullPath((_, path))), _, NonTerminal(Symbol::CommaSeparatedExpressions((_, arguments))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Some(Box::new(mem::take(expression))), Some(mem::take(path)), mem::take(arguments)) }))
        } else { unreachable!() })
    <Expression8>::=(<Expression8> [Dot] [Identifier] [RoundBracketOpen] <CommaSeparatedExpressions> [RoundBracketClose])
        (|list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, Terminal(Token::Identifier((position, path))), _, NonTerminal(Symbol::CommaSeparatedExpressions((_, arguments))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Some(Box::new(mem::take(expression))), Some(Path { position: position.clone(), path: vec![path.clone()] }), mem::take(arguments)) }))
        } else { unreachable!() })
    <Expression8>::=(<FullPath> [RoundBracketOpen] <CommaSeparatedExpressions> [RoundBracketClose])
        (|list| if let [NonTerminal(Symbol::FullPath((Range { start, .. }, path))), _, NonTerminal(Symbol::CommaSeparatedExpressions((_, arguments))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(None, Some(mem::take(path)), mem::take(arguments)) }))
        } else { unreachable!() })
    <Expression8>::=([Identifier] [RoundBracketOpen] <CommaSeparatedExpressions> [RoundBracketClose])
        (|list| if let [Terminal(Token::Identifier((position, path))), _, NonTerminal(Symbol::CommaSeparatedExpressions((_, arguments))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((position.start..*end, Expression { position: position.start..*end, expression: ExpressionItem::CallFunction(None, Some(Path { position: position.clone(), path: vec![path.clone()] }), mem::take(arguments)) }))
        } else { unreachable!() })
    <Expression8>::=(<Expression8> [Dot] [RoundBracketOpen] <CommaSeparatedExpressions> [RoundBracketClose])
        (|list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, _, NonTerminal(Symbol::CommaSeparatedExpressions((_, arguments))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::CallFunction(Some(Box::new(mem::take(expression))), None, mem::take(arguments)) }))
        } else { unreachable!() })
    <Expression8>::=(<Expression8> [SquareBracketOpen] <Expression> [SquareBracketClose])
        (|list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, expression))), _, NonTerminal(Symbol::Expression((_, index))), Terminal(Token::SquareBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Indexer(Box::new(mem::take(expression)), Box::new(mem::take(index))) }))
        } else { unreachable!() })
    <Expression8>::=([RoundBracketOpen] <CommaSeparatedExpressions> [RoundBracketClose])
        (|list| if let [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedExpressions((_, expressions))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructTuple(mem::take(expressions)) }))
        } else { unreachable!() })
    <Expression8>::=([SquareBracketOpen] <CommaSeparatedExpressions> [SquareBracketClose])
        (|list| if let [Terminal(Token::SquareBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedExpressions((_, expressions))), Terminal(Token::SquareBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructArray(mem::take(expressions)) }))
        } else { unreachable!() })
    <Expression8>::=([RoundBracketOpen] <Expression> [Comma] [RoundBracketClose])
        (|list| if let [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::Expression((_, item))), _, Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructTuple(vec![mem::take(item)]) }))
        } else { unreachable!() })
    <Expression8>::=([SquareBracketOpen] <Expression> [SquareBracketClose])
        (|list| if let [Terminal(Token::SquareBracketOpen(Range { start, .. })), NonTerminal(Symbol::Expression((_, item))), Terminal(Token::SquareBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructArray(vec![mem::take(item)]) }))
        } else { unreachable!() })
    <Expression8>::=([RoundBracketOpen] [RoundBracketClose])
        (|list| if let [Terminal(Token::RoundBracketOpen(Range { start, .. })), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructTuple(Vec::new()) }))
        } else { unreachable!() })
    <Expression8>::=([SquareBracketOpen] [SquareBracketClose])
        (|list| if let [Terminal(Token::SquareBracketOpen(Range { start, .. })), Terminal(Token::SquareBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::ConstructArray(Vec::new()) }))
        } else { unreachable!() })
    <Expression8>::=([RoundBracketOpen] <Expression> [RoundBracketClose])
        (|list| if let [Terminal(Token::RoundBracketOpen(Range { start, .. })), NonTerminal(Symbol::Expression((_, item))), Terminal(Token::RoundBracketClose(Range { end, .. }))] = list {
            Symbol::Expression8((*start..*end, mem::take(item)))
        } else { unreachable!() })
    <Expression8>::=(<Block>)
        (|list| if let [NonTerminal(Symbol::Block((position, block)))] = list {
            Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::Block(mem::take(block)) }))
        } else { unreachable!() })
    <Expression8>::=(<FullPath>)
        (|list| if let [NonTerminal(Symbol::FullPath((position, path)))] = list {
            Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::AccessVariable(mem::take(path)) }))
        } else { unreachable!() })
    <Expression8>::=(<ConstructStruct>)
        (|list| if let [NonTerminal(Symbol::ConstructStruct((position, item)))] = list {
            Symbol::Expression8((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::ConstructStruct(mem::take(item)) }))
        } else { unreachable!() })
    <Expression7>::=(<Expression8>)
        (|list| if let [NonTerminal(Symbol::Expression8(item))] = list {
            Symbol::Expression7(mem::take(item))
        } else { unreachable!() })
    <Expression7>::=([Sub] <Expression7>)
        (|list| if let [Terminal(Token::Sub(Range { start, .. })), NonTerminal(Symbol::Expression7((Range { end, .. }, expression)))] = list {
            Symbol::Expression7((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Neg(Box::new(mem::take(expression))) }))
        } else { unreachable!() })
    <Expression7>::=([Not] <Expression7>)
        (|list| if let [Terminal(Token::Not(Range { start, .. })), NonTerminal(Symbol::Expression7((Range { end, .. }, expression)))] = list {
            Symbol::Expression7((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Not(Box::new(mem::take(expression))) }))
        } else { unreachable!() })
    <Expression6>::=(<Expression7>)
        (|list| if let [NonTerminal(Symbol::Expression7(item))] = list {
            Symbol::Expression6(mem::take(item))
        } else { unreachable!() })
    <Expression6>::=(<Expression6> [Mul] <Expression7>)
        (|list| if let [NonTerminal(Symbol::Expression6((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression7((Range { end, .. }, second)))] = list {
            Symbol::Expression6((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Mul(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression6>::=(<Expression6> [Div] <Expression7>)
        (|list| if let [NonTerminal(Symbol::Expression6((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression7((Range { end, .. }, second)))] = list {
            Symbol::Expression6((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Div(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression6>::=(<Expression6> [Rem] <Expression7>)
        (|list| if let [NonTerminal(Symbol::Expression6((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression7((Range { end, .. }, second)))] = list {
            Symbol::Expression6((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Rem(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression5>::=(<Expression6>)
        (|list| if let [NonTerminal(Symbol::Expression6(item))] = list {
            Symbol::Expression5(mem::take(item))
        } else { unreachable!() })
    <Expression5>::=(<Expression5> [Add] <Expression6>)
        (|list| if let [NonTerminal(Symbol::Expression5((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression6((Range { end, .. }, second)))] = list {
            Symbol::Expression5((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Add(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression5>::=(<Expression5> [Sub] <Expression6>)
        (|list| if let [NonTerminal(Symbol::Expression5((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression6((Range { end, .. }, second)))] = list {
            Symbol::Expression5((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Sub(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression4>::=(<Expression5>)
        (|list| if let [NonTerminal(Symbol::Expression5(item))] = list {
            Symbol::Expression4(mem::take(item))
        } else { unreachable!() })
    <Expression4>::=(<Expression4> [GreaterThan] <Expression5>)
        (|list| if let [NonTerminal(Symbol::Expression4((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression5((Range { end, .. }, second)))] = list {
            Symbol::Expression4((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::GreaterThan(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression4>::=(<Expression4> [LessThan] <Expression5>)
        (|list| if let [NonTerminal(Symbol::Expression4((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression5((Range { end, .. }, second)))] = list {
            Symbol::Expression4((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::LessThan(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression4>::=(<Expression4> [GreaterOrEqual] <Expression5>)
        (|list| if let [NonTerminal(Symbol::Expression4((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression5((Range { end, .. }, second)))] = list {
            Symbol::Expression4((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::GreaterOrEqual(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression4>::=(<Expression4> [LessOrEqual] <Expression5>)
        (|list| if let [NonTerminal(Symbol::Expression4((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression5((Range { end, .. }, second)))] = list {
            Symbol::Expression4((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::LessOrEqual(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression3>::=(<Expression4>)
        (|list| if let [NonTerminal(Symbol::Expression4(item))] = list {
            Symbol::Expression3(mem::take(item))
        } else { unreachable!() })
    <Expression3>::=(<Expression3> [Equals] <Expression4>)
        (|list| if let [NonTerminal(Symbol::Expression3((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression4((Range { end, .. }, second)))] = list {
            Symbol::Expression3((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Equals(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression3>::=(<Expression3> [NotEquals] <Expression4>)
        (|list| if let [NonTerminal(Symbol::Expression3((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression4((Range { end, .. }, second)))] = list {
            Symbol::Expression3((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::NotEquals(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression2>::=(<Expression3>)
        (|list| if let [NonTerminal(Symbol::Expression3(item))] = list {
            Symbol::Expression2(mem::take(item))
        } else { unreachable!() })
    <Expression2>::=(<Expression2> [And] <Expression3>)
        (|list| if let [NonTerminal(Symbol::Expression2((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression3((Range { end, .. }, second)))] = list {
            Symbol::Expression2((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::And(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression1>::=(<Expression2>)
        (|list| if let [NonTerminal(Symbol::Expression2(item))] = list {
            Symbol::Expression1(mem::take(item))
        } else { unreachable!() })
    <Expression1>::=(<Expression1> [Xor] <Expression2>)
        (|list| if let [NonTerminal(Symbol::Expression1((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression2((Range { end, .. }, second)))] = list {
            Symbol::Expression1((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Xor(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression0>::=(<Expression1>)
        (|list| if let [NonTerminal(Symbol::Expression1(item))] = list {
            Symbol::Expression0(mem::take(item))
        } else { unreachable!() })
    <Expression0>::=(<Expression0> [Or] <Expression1>)
        (|list| if let [NonTerminal(Symbol::Expression0((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression1((Range { end, .. }, second)))] = list {
            Symbol::Expression0((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Or(Box::new(mem::take(first)), Box::new(mem::take(second))) }))
        } else { unreachable!() })
    <Expression>::=(<Expression0>)
        (|list| if let [NonTerminal(Symbol::Expression0(item))] = list {
            Symbol::Expression(mem::take(item))
        } else { unreachable!() })
    <Expression>::=(<ClosureExpression>)
        (|list| if let [NonTerminal(Symbol::ClosureExpression((position, closure)))] = list {
            Symbol::Expression((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::Closure(mem::take(closure)) }))
        } else { unreachable!() })
    <Expression>::=(<IfExpression>)
        (|list| if let [NonTerminal(Symbol::IfExpression((position, expression)))] = list {
            Symbol::Expression((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::If(mem::take(expression)) }))
        } else { unreachable!() })
    <Expression>::=(<MatchExpression>)
        (|list| if let [NonTerminal(Symbol::MatchExpression((position, expression)))] = list {
            Symbol::Expression((position.clone(), Expression { position: position.clone(), expression: ExpressionItem::Match(mem::take(expression)) }))
        } else { unreachable!() })
    <Expression>::=(<Expression8> [Assign] <Expression>)
        (|list| if let [NonTerminal(Symbol::Expression8((Range { start, .. }, variable))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
            Symbol::Expression((*start..*end, Expression { position: *start..*end, expression: ExpressionItem::Assign(Box::new(mem::take(variable)), Box::new(mem::take(expression))) }))
        } else { unreachable!() })
    <ClosureExpression>::=([Fn] [Colon] <ClosureArguments> [Or] [FunctionArrow] <Expression>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), _, NonTerminal(Symbol::ClosureArguments((_, arguments))), _, _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
            Symbol::ClosureExpression((*start..*end, Closure { position: *start..*end, arguments: mem::take(arguments), expression: Box::new(mem::take(expression)) }))
        } else { unreachable!() })
    <ClosureExpression>::=([Fn] [Colon] [Or] [FunctionArrow] <Expression>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), _, _, _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
            Symbol::ClosureExpression((*start..*end, Closure { position: *start..*end, arguments: Vec::new(), expression: Box::new(mem::take(expression)) }))
        } else { unreachable!() })
    <IfExpression>::=([If] <Expression> <Block>)
        (|list| if let [Terminal(Token::If(Range { start, .. })), NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
            Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::BoolExpression(mem::take(condition))), then: Box::new(Expression { position: block.position.clone(), expression: ExpressionItem::Block(mem::take(block)) }), other: None }))
        } else { unreachable!() })
    <IfExpression>::=([If] <Expression> <Block> [Else] <Block>)
        (|list| if let [Terminal(Token::If(Range { start, .. })), NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::Block((_, then))), _, NonTerminal(Symbol::Block((Range { end, .. }, other)))] = list {
            Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::BoolExpression(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: Some(Box::new(Expression { position: other.position.clone(), expression: ExpressionItem::Block(mem::take(other)) })) }))
        } else { unreachable!() })
    <IfExpression>::=([If] <Expression> <Block> [Else] <IfExpression>)
        (|list| if let [Terminal(Token::If(Range { start, .. })), NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::Block((_, then))), _, NonTerminal(Symbol::IfExpression((Range { end, .. }, expression)))] = list {
            Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::BoolExpression(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: Some(Box::new(Expression { position: expression.position.clone(), expression: ExpressionItem::If(mem::take(expression)) })) }))
        } else { unreachable!() })
    <IfExpression>::=([If] <LetBinding> <Block>)
        (|list| if let [Terminal(Token::If(Range { start, .. })), NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::Block((Range { end, .. }, then)))] = list {
            Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::LetBinding(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: None }))
        } else { unreachable!() })
    <IfExpression>::=([If] <LetBinding> <Block> [Else] <Block>)
        (|list| if let [Terminal(Token::If(Range { start, .. })), NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::Block((_, then))), _, NonTerminal(Symbol::Block((Range { end, .. }, other)))] = list {
            Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::LetBinding(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: Some(Box::new(Expression { position: other.position.clone(), expression: ExpressionItem::Block(mem::take(other)) })) }))
        } else { unreachable!() })
    <IfExpression>::=([If] <LetBinding> <Block> [Else] <IfExpression>)
        (|list| if let [Terminal(Token::If(Range { start, .. })), NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::Block((_, then))), _, NonTerminal(Symbol::IfExpression((Range { end, .. }, other)))] = list {
            Symbol::IfExpression((*start..*end, If { position: *start..*end, condition: Box::new(Condition::LetBinding(mem::take(condition))), then: Box::new(Expression { position: then.position.clone(), expression: ExpressionItem::Block(mem::take(then)) }), other: Some(Box::new(Expression { position: other.position.clone(), expression: ExpressionItem::If(mem::take(other)) })) }))
        } else { unreachable!() })
    <MatchExpression>::=([Match] <Expression> [WaveBracketOpen] <MatchPatterns> [WaveBracketClose])
        (|list| if let [Terminal(Token::Match(Range { start, .. })), NonTerminal(Symbol::Expression((_, condition))), _, NonTerminal(Symbol::MatchPatterns((_, items))), Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::MatchExpression((*start..*end, Match { position: *start..*end, condition: Box::new(mem::take(condition)), items: mem::take(items) }))
        } else { unreachable!() })
    <MatchExpression>::=([Match] <Expression> [WaveBracketOpen] <MatchPatterns> [Comma] [WaveBracketClose])
        (|list| if let [Terminal(Token::Match(Range { start, .. })), NonTerminal(Symbol::Expression((_, condition))), _, NonTerminal(Symbol::MatchPatterns((_, items))), _, Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::MatchExpression((*start..*end, Match { position: *start..*end, condition: Box::new(mem::take(condition)), items: mem::take(items) }))
        } else { unreachable!() })
    <MatchPatterns>::=(<Pattern> [MatchArrow] <Expression>)
        (|list| if let [NonTerminal(Symbol::Pattern((Range { start, .. }, pattern))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
            Symbol::MatchPatterns((*start..*end, vec![(mem::take(pattern), mem::take(expression))]))
        } else { unreachable!() })
    <MatchPatterns>::=(<MatchPatterns> [Comma] <Pattern> [MatchArrow] <Expression>)
        (|list| if let [NonTerminal(Symbol::MatchPatterns((Range { start, .. }, patterns))), _, NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
            Symbol::MatchPatterns((*start..*end, {
                patterns.push((mem::take(pattern), mem::take(expression)));
                mem::take(patterns)
            }))
        } else { unreachable!() })
    <ForStatement>::=([For] <Pattern> [In] <Expression> <Block>)
        (|list| if let [Terminal(Token::For(Range { start, .. })), NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Expression((_, iter))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
            Symbol::ForStatement((*start..*end, ForStatement { label: None, position: *start..*end, pattern: mem::take(pattern), iter: mem::take(iter), block: mem::take(block) }))
        } else { unreachable!() })
    <ForStatement>::=([Identifier] [Colon] [For] <Pattern> [In] <Expression> <Block>)
        (|list| if let [Terminal(Token::Identifier((Range { start, .. }, label))), _, _, NonTerminal(Symbol::Pattern((_, pattern))), _, NonTerminal(Symbol::Expression((_, iter))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
            Symbol::ForStatement((*start..*end, ForStatement { label: Some(label.clone()), position: *start..*end, pattern: mem::take(pattern), iter: mem::take(iter), block: mem::take(block) }))
        } else { unreachable!() })
    <WhileStatement>::=([While] <Expression> <Block>)
        (|list| if let [Terminal(Token::While(Range { start, .. })), NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
            Symbol::WhileStatement((*start..*end, WhileStatement { label: None, position: *start..*end, condition: Condition::BoolExpression(mem::take(condition)), block: mem::take(block) }))
        } else { unreachable!() })
    <WhileStatement>::=([While] <LetBinding> <Block>)
        (|list| if let [Terminal(Token::While(Range { start, .. })), NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
            Symbol::WhileStatement((*start..*end, WhileStatement { label: None, position: *start..*end, condition: Condition::LetBinding(mem::take(condition)), block: mem::take(block) }))
        } else { unreachable!() })
    <WhileStatement>::=([Identifier] [Colon] [While] <Expression> <Block>)
        (|list| if let [Terminal(Token::Identifier((Range { start, .. }, label))), _, _, NonTerminal(Symbol::Expression((_, condition))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
            Symbol::WhileStatement((*start..*end, WhileStatement { label: Some(label.clone()), position: *start..*end, condition: Condition::BoolExpression(mem::take(condition)), block: mem::take(block) }))
        } else { unreachable!() })
    <WhileStatement>::=([Identifier] [Colon] [While] <LetBinding> <Block>)
        (|list| if let [Terminal(Token::Identifier((Range { start, .. }, label))), _, _, NonTerminal(Symbol::LetBinding((_, condition))), NonTerminal(Symbol::Block((Range { end, .. }, block)))] = list {
            Symbol::WhileStatement((*start..*end, WhileStatement { label: Some(label.clone()), position: *start..*end, condition: Condition::LetBinding(mem::take(condition)), block: mem::take(block) }))
        } else { unreachable!() })
    <Pattern>::=([Identifier])
        (|list| if let [Terminal(Token::Identifier((position, pattern)))] = list {
            Symbol::Pattern((position.clone(), Pattern::Variable(pattern.clone())))
        } else { unreachable!() })
    <Pattern>::=(<StructName> [WaveBracketOpen] [WaveBracketClose])
        (|list| if let [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::Pattern((*start..*end, Pattern::Struct(mem::take(name), Vec::new())))
        } else { unreachable!() })
    <Pattern>::=(<StructName> [WaveBracketOpen] <PatternStructItems> [WaveBracketClose])
        (|list| if let [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, NonTerminal(Symbol::PatternStructItems((_, items))), Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::Pattern((*start..*end, Pattern::Struct(mem::take(name), mem::take(items))))
        } else { unreachable!() })
    <Pattern>::=(<StructName> [WaveBracketOpen] <PatternStructItems> [Comma] [WaveBracketClose])
        (|list| if let [NonTerminal(Symbol::StructName((Range { start, .. }, name))), _, NonTerminal(Symbol::PatternStructItems((_, items))), _, Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::Pattern((*start..*end, Pattern::Struct(mem::take(name), mem::take(items))))
        } else { unreachable!() })
    <PatternStructItems>::=([Identifier])
        (|list| if let [Terminal(Token::Identifier((position, name)))] = list {
            Symbol::PatternStructItems((position.clone(), vec![(name.clone(), Pattern::Variable(name.clone()))]))
        } else { unreachable!() })
    <PatternStructItems>::=([Identifier] [Colon] <Pattern>)
        (|list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::Pattern((Range { end, .. }, pattern)))] = list {
            Symbol::PatternStructItems((*start..*end, vec![(name.clone(), mem::take(pattern))]))
        } else { unreachable!() })
    <PatternStructItems>::=(<PatternStructItems> [Comma] [Identifier])
        (|list| if let [NonTerminal(Symbol::PatternStructItems((Range { start, .. }, items))), _, Terminal(Token::Identifier((Range { end, .. }, name)))] = list {
            Symbol::PatternStructItems((*start..*end, {
                items.push((name.clone(), Pattern::Variable(name.clone())));
                mem::take(items)
            }))
        } else { unreachable!() })
    <PatternStructItems>::=(<PatternStructItems> [Comma] [Identifier] [Colon] <Pattern>)
        (|list| if let [NonTerminal(Symbol::PatternStructItems((Range { start, .. }, items))), _, Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::Pattern((Range { end, .. }, pattern)))] = list {
            Symbol::PatternStructItems((*start..*end, {
                items.push((name.clone(), mem::take(pattern)));
                mem::take(items)
            }))
        } else { unreachable!() })
    <ClosureArguments>::=([Identifier])
        (|list| if let [Terminal(Token::Identifier((position, name)))] = list {
            Symbol::ClosureArguments((position.clone(), vec![(name.clone(), None)]))
        } else { unreachable!() })
    <ClosureArguments>::=([Identifier] [Colon] <Type>)
        (|list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, t)))] = list {
            Symbol::ClosureArguments((*start..*end, vec![(name.clone(), Some(mem::take(t)))]))
        } else { unreachable!() })
    <ClosureArguments>::=(<ClosureArguments> [Comma] [Identifier])
        (|list| if let [NonTerminal(Symbol::ClosureArguments((Range { start, .. }, list))), _, Terminal(Token::Identifier((Range { end, .. }, name)))] = list {
            Symbol::ClosureArguments((*start..*end, {
                list.push((name.clone(), None));
                mem::take(list)
            }))
        } else { unreachable!() })
    <ClosureArguments>::=(<ClosureArguments> [Comma] [Identifier] [Colon] <Type>)
        (|list| if let [NonTerminal(Symbol::ClosureArguments((Range { start, .. }, list))), _, Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, t)))] = list {
            Symbol::ClosureArguments((*start..*end, {
                list.push((name.clone(), Some(mem::take(t))));
                mem::take(list)
            }))
        } else { unreachable!() })
    <CommaSeparatedExpressions>::=(<Expression> [Comma] <Expression>)
        (|list| if let [NonTerminal(Symbol::Expression((Range { start, .. }, first))), _, NonTerminal(Symbol::Expression((Range { end, .. }, second)))] = list {
            Symbol::CommaSeparatedExpressions((*start..*end, vec![mem::take(first), mem::take(second)]))
        } else { unreachable!() })
    <CommaSeparatedExpressions>::=(<CommaSeparatedExpressions> [Comma] <Expression>)
        (|list| if let [NonTerminal(Symbol::CommaSeparatedExpressions((Range { start, .. }, list))), _, NonTerminal(Symbol::Expression((Range { end, .. }, expression)))] = list {
            Symbol::CommaSeparatedExpressions((*start..*end, {
                list.push(mem::take(expression));
                mem::take(list)
            }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [RoundBracketOpen] <FunctionArguments> [RoundBracketClose] [FunctionArrow] <Type> <Block>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [RoundBracketOpen] <FunctionArguments> [RoundBracketClose] <Block>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [RoundBracketOpen] <FunctionArguments> [RoundBracketClose] [FunctionArrow] <Type> <Block>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [RoundBracketOpen] <FunctionArguments> [RoundBracketClose] <Block>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [RoundBracketOpen] <FunctionArguments> [Comma] [RoundBracketClose] [FunctionArrow] <Type> <Block>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [RoundBracketOpen] <FunctionArguments> [Comma] [RoundBracketClose] <Block>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [RoundBracketOpen] <FunctionArguments> [Comma] [RoundBracketClose] [FunctionArrow] <Type> <Block>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [RoundBracketOpen] <FunctionArguments> [Comma] [RoundBracketClose] <Block>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [RoundBracketOpen] [RoundBracketClose] [FunctionArrow] <Type> <Block>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: Vec::new(), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [RoundBracketOpen] [RoundBracketClose] <Block>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: Vec::new(), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [RoundBracketOpen] [RoundBracketClose] [FunctionArrow] <Type> <Block>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, _, _, NonTerminal(Symbol::Type((_, t))), NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: Vec::new(), return_type: mem::take(t), expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [RoundBracketOpen] [RoundBracketClose] <Block>)
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, _, NonTerminal(Symbol::Block((Range { end, .. }, expression)))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: Vec::new(), return_type: Type::None, expression: Expression { position: expression.position.clone(), expression: ExpressionItem::Block(mem::take(expression)) } }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [RoundBracketOpen] <FunctionArguments> [RoundBracketClose] [FunctionArrow] <Type> [Or] [FunctionArrow] <Expression> [Semicolon])
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Type((_, t))), Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: mem::take(t), expression: mem::take(expression) }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [RoundBracketOpen] <FunctionArguments> [RoundBracketClose] [Or] [FunctionArrow] <Expression> [Semicolon])
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: Type::None, expression: mem::take(expression) }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [RoundBracketOpen] <FunctionArguments> [RoundBracketClose] [FunctionArrow] <Type> [Or] [FunctionArrow] <Expression> [Semicolon])
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, _, NonTerminal(Symbol::Type((_, t))), Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: mem::take(t), expression: mem::take(expression) }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [RoundBracketOpen] <FunctionArguments> [RoundBracketClose] [Or] [FunctionArrow] <Expression> [Semicolon])
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), _, Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: Type::None, expression: mem::take(expression) }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [RoundBracketOpen] <FunctionArguments> [Comma] [RoundBracketClose] [FunctionArrow] <Type> [Or] [FunctionArrow] <Expression> [Semicolon])
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), Terminal(Token::Comma(_)), _, _, NonTerminal(Symbol::Type((_, t))), Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: mem::take(t), expression: mem::take(expression) }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [RoundBracketOpen] <FunctionArguments> [Comma] [RoundBracketClose] [Or] [FunctionArrow] <Expression> [Semicolon])
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::FunctionArguments((_, arguments))), Terminal(Token::Comma(_)), _, Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: mem::take(arguments), return_type: Type::None, expression: mem::take(expression) }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [RoundBracketOpen] <FunctionArguments> [Comma] [RoundBracketClose] [FunctionArrow] <Type> [Or] [FunctionArrow] <Expression> [Semicolon])
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), Terminal(Token::Comma(_)), _, _, NonTerminal(Symbol::Type((_, t))), Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: mem::take(t), expression: mem::take(expression) }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [RoundBracketOpen] <FunctionArguments> [Comma] [RoundBracketClose] [Or] [FunctionArrow] <Expression> [Semicolon])
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, NonTerminal(Symbol::FunctionArguments((_, arguments))), Terminal(Token::Comma(_)), _, Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: mem::take(arguments), return_type: Type::None, expression: mem::take(expression) }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [RoundBracketOpen] [RoundBracketClose] [FunctionArrow] <Type> [Or] [FunctionArrow] <Expression> [Semicolon])
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, _, NonTerminal(Symbol::Type((_, t))), Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: Vec::new(), return_type: mem::take(t), expression: mem::take(expression) }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [RoundBracketOpen] [RoundBracketClose] [Or] [FunctionArrow] <Expression> [Semicolon])
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: Vec::new(), arguments: Vec::new(), return_type: Type::None, expression: mem::take(expression) }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [RoundBracketOpen] [RoundBracketClose] [FunctionArrow] <Type> [Or] [FunctionArrow] <Expression> [Semicolon])
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, _, _, NonTerminal(Symbol::Type((_, t))), Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: Vec::new(), return_type: mem::take(t), expression: mem::take(expression) }))
        } else { unreachable!() })
    <FunctionDefinition>::=([Fn] [Identifier] [DoubleColon] [LessThan] <GenericsTypeArguments> [GreaterThan] [RoundBracketOpen] [RoundBracketClose] [Or] [FunctionArrow] <Expression> [Semicolon])
        (|list| if let [Terminal(Token::Fn(Range { start, .. })), Terminal(Token::Identifier((_, name))), _, _, NonTerminal(Symbol::GenericsTypeArguments((_, generics_arguments))), _, _, _, Terminal(Token::Or(_)), _, NonTerminal(Symbol::Expression((_, expression))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::FunctionDefinition((*start..*end, Function { position: *start..*end, name: name.clone(), generics_arguments: mem::take(generics_arguments), arguments: Vec::new(), return_type: Type::None, expression: mem::take(expression) }))
        } else { unreachable!() })
    <FunctionArguments>::=([Identifier] [Colon] <Type>)
        (|list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, t)))] = list {
            Symbol::FunctionArguments((*start..*end, vec![(name.clone(), mem::take(t))]))
        } else { unreachable!() })
    <FunctionArguments>::=(<FunctionArguments> [Comma] [Identifier] [Colon] <Type>)
        (|list| if let [NonTerminal(Symbol::FunctionArguments((Range { start, .. }, list))), _, Terminal(Token::Identifier((_, name))), _, NonTerminal(Symbol::Type((Range { end, .. }, t)))] = list {
            Symbol::FunctionArguments((*start..*end, {
                list.push((name.clone(), mem::take(t)));
                mem::take(list)
            }))
        } else { unreachable!() })
    <ImportStatement>::=([Import] <ImportPath> [Semicolon])
        (|list| if let [Terminal(Token::Import(Range { start, .. })), NonTerminal(Symbol::ImportPath((_, path))), Terminal(Token::Semicolon(Range { end, .. }))] = list {
            Symbol::ImportStatement((*start..*end, mem::take(path)))
        } else { unreachable!() })
    <ImportPath>::=(<ImportPathChild>)
        (|list| if let [NonTerminal(Symbol::ImportPathChild(item))] = list {
            Symbol::ImportPath(mem::take(item))
        } else { unreachable!() })
    <ImportPath>::=([StringImmediate] [DoubleColon] <ImportPathChild>)
        (|list| if let [Terminal(Token::StringImmediate((Range { start, .. }, name))), _, NonTerminal(Symbol::ImportPathChild((Range { end, .. }, path)))] = list {
            Symbol::ImportPath((*start..*end, ImportPath::Child(name.clone(), Box::new(mem::take(path)))))
        } else { unreachable!() })
    <ImportPathChild>::=([Identifier])
        (|list| if let [Terminal(Token::Identifier((position, name)))] = list {
            Symbol::ImportPathChild((position.clone(), ImportPath::Item(name.clone())))
        } else { unreachable!() })
    <ImportPathChild>::=([Identifier] [DoubleColon] <ImportPathChild>)
        (|list| if let [Terminal(Token::Identifier((Range { start, .. }, name))), _, NonTerminal(Symbol::ImportPathChild((Range { end, .. }, path)))] = list {
            Symbol::ImportPathChild((*start..*end, ImportPath::Child(name.clone(), Box::new(mem::take(path)))))
        } else { unreachable!() })
    <ImportPathChild>::=([WaveBracketOpen] <CommaSeparatedImportPathChildren> [WaveBracketClose])
        (|list| if let [Terminal(Token::WaveBracketOpen(Range { start, .. })), NonTerminal(Symbol::CommaSeparatedImportPathChildren((_, paths))), Terminal(Token::WaveBracketClose(Range { end, .. }))] = list {
            Symbol::ImportPathChild((*start..*end, ImportPath::List(mem::take(paths))))
        } else { unreachable!() })
    <CommaSeparatedImportPathChildren>::=(<ImportPathChild>)
        (|list| if let [NonTerminal(Symbol::ImportPathChild((position, path)))] = list {
            Symbol::CommaSeparatedImportPathChildren((position.clone(), vec![mem::take(path)]))
        } else { unreachable!() })
    <CommaSeparatedImportPathChildren>::=(<CommaSeparatedImportPathChildren> [Comma] <ImportPathChild>)
        (|list| if let [NonTerminal(Symbol::CommaSeparatedImportPathChildren((Range { start, .. }, list))), _, NonTerminal(Symbol::ImportPathChild((Range { end, .. }, path)))] = list {
            Symbol::CommaSeparatedImportPathChildren((*start..*end, {
                list.push(mem::take(path));
                mem::take(list)
            }))
        } else { unreachable!() })
    <GenericsTypeArguments>::=([Identifier])
        (|list| if let [Terminal(Token::Identifier((position, name)))] = list {
            Symbol::GenericsTypeArguments((position.clone(), vec![name.clone()]))
        } else { unreachable!() })
    <GenericsTypeArguments>::=(<GenericsTypeArguments> [Comma] [Identifier])
        (|list| if let [NonTerminal(Symbol::GenericsTypeArguments((Range { start, .. }, list))), _, Terminal(Token::Identifier((Range { end, .. }, name)))] = list {
            Symbol::GenericsTypeArguments((*start..*end, {
                list.push(name.clone());
                mem::take(list)
            }))
        } else { unreachable!() })
}
