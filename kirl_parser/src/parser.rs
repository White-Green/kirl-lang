use parser::{LR1Parser, Rule, Syntax};
use parser::enum_index;
use parser::enum_index_derive::*;
use parser::Symbol::{NonTerminal, Terminal};

use crate::tokenizer::Token;
use crate::CharacterPosition;
use std::ops::Range;

#[derive(Debug, EnumIndex)]
pub enum Symbol {
    ValidKirlCode((Range<CharacterPosition>)),
    Type((Range<CharacterPosition>)),
    FullPath((Range<CharacterPosition>)),
    CommaSeparatedTypes((Range<CharacterPosition>)),
    OrSeparatedTypes((Range<CharacterPosition>)),
    StructDefinitionItems((Range<CharacterPosition>)),
    ConstructStruct((Range<CharacterPosition>)),
    StructName((Range<CharacterPosition>)),
    StructDefinition((Range<CharacterPosition>)),
    ConstructStructItems((Range<CharacterPosition>)),
    LetBinding((Range<CharacterPosition>)),
    Statement((Range<CharacterPosition>)),
    StatementList((Range<CharacterPosition>)),
    Block((Range<CharacterPosition>)),
    Expression8((Range<CharacterPosition>)),
    Expression7((Range<CharacterPosition>)),
    Expression6((Range<CharacterPosition>)),
    Expression5((Range<CharacterPosition>)),
    Expression4((Range<CharacterPosition>)),
    Expression3((Range<CharacterPosition>)),
    Expression2((Range<CharacterPosition>)),
    Expression1((Range<CharacterPosition>)),
    Expression0((Range<CharacterPosition>)),
    Expression((Range<CharacterPosition>)),
    ClosureExpression((Range<CharacterPosition>)),
    IfExpression((Range<CharacterPosition>)),
    MatchExpression((Range<CharacterPosition>)),
    MatchPatterns((Range<CharacterPosition>)),
    ForStatement((Range<CharacterPosition>)),
    WhileStatement((Range<CharacterPosition>)),
    Pattern((Range<CharacterPosition>)),
    PatternStructItems((Range<CharacterPosition>)),
    ClosureArguments((Range<CharacterPosition>)),
    CommaSeparatedExpressions((Range<CharacterPosition>)),
    FunctionDefinition((Range<CharacterPosition>)),
    FunctionArguments((Range<CharacterPosition>)),
    ImportStatement((Range<CharacterPosition>)),
    ImportPath((Range<CharacterPosition>)),
    ImportPathChild((Range<CharacterPosition>)),
    CommaSeparatedImportPathChildren((Range<CharacterPosition>)),
    GenericsTypeArguments((Range<CharacterPosition>)),
}

pub fn get_parser() -> LR1Parser<Symbol, Token> {
    let (parser, warning) = LR1Parser::new(Syntax::builder()
        .rule(Rule::new(Symbol::ValidKirlCode(Default::default()), &[],
                        |list| if let [] = list {
                            Symbol::ValidKirlCode(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ValidKirlCode(Default::default()), &[NonTerminal(Symbol::ValidKirlCode(Default::default())), NonTerminal(Symbol::Statement(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ValidKirlCode(_)), NonTerminal(Symbol::Statement(_))] = list {
                            Symbol::ValidKirlCode(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ValidKirlCode(Default::default()), &[NonTerminal(Symbol::ValidKirlCode(Default::default())), NonTerminal(Symbol::FunctionDefinition(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ValidKirlCode(_)), NonTerminal(Symbol::FunctionDefinition(_))] = list {
                            Symbol::ValidKirlCode(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ValidKirlCode(Default::default()), &[NonTerminal(Symbol::ValidKirlCode(Default::default())), NonTerminal(Symbol::StructDefinition(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ValidKirlCode(_)), NonTerminal(Symbol::StructDefinition(_))] = list {
                            Symbol::ValidKirlCode(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::Not(Default::default()))],
                        |list| if let [Terminal(Token::Not(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::GreaterThan(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::CommaSeparatedTypes(_)), Terminal(Token::GreaterThan(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[NonTerminal(Symbol::FullPath(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FullPath(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[NonTerminal(Symbol::FullPath(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::GreaterThan(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FullPath(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::CommaSeparatedTypes(_)), Terminal(Token::GreaterThan(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::SquareBracketOpen(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::SquareBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::SquareBracketOpen(_)), NonTerminal(Symbol::Type(_)), Terminal(Token::SquareBracketClose(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::Type(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::Type(_)), Terminal(Token::Comma(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::CommaSeparatedTypes(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::Sharp(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::StructDefinitionItems(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Sharp(_)), Terminal(Token::WaveBracketOpen(_)), NonTerminal(Symbol::StructDefinitionItems(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::Type(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::CommaSeparatedTypes(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Type(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::OrSeparatedTypes(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::OrSeparatedTypes(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Type(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FullPath(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::Identifier(_))] = list {
                            Symbol::FullPath(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FullPath(Default::default()), &[NonTerminal(Symbol::FullPath(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FullPath(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::Identifier(_))] = list {
                            Symbol::FullPath(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::CommaSeparatedTypes(Default::default()), &[NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Comma(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Type(_)), Terminal(Token::Comma(_)), NonTerminal(Symbol::Type(_))] = list {
                            Symbol::CommaSeparatedTypes(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::CommaSeparatedTypes(Default::default()), &[NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::Comma(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::CommaSeparatedTypes(_)), Terminal(Token::Comma(_)), NonTerminal(Symbol::Type(_))] = list {
                            Symbol::CommaSeparatedTypes(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::OrSeparatedTypes(Default::default()), &[NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Type(_)), Terminal(Token::Or(_)), NonTerminal(Symbol::Type(_))] = list {
                            Symbol::OrSeparatedTypes(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::OrSeparatedTypes(Default::default()), &[NonTerminal(Symbol::OrSeparatedTypes(Default::default())), Terminal(Token::Or(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::OrSeparatedTypes(_)), Terminal(Token::Or(_)), NonTerminal(Symbol::Type(_))] = list {
                            Symbol::OrSeparatedTypes(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructDefinitionItems(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_)), Terminal(Token::Colon(_)), NonTerminal(Symbol::Type(_))] = list {
                            Symbol::StructDefinitionItems(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructDefinitionItems(Default::default()), &[NonTerminal(Symbol::StructDefinitionItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructDefinitionItems(_)), Terminal(Token::Comma(_)), Terminal(Token::Identifier(_)), Terminal(Token::Colon(_)), NonTerminal(Symbol::Type(_))] = list {
                            Symbol::StructDefinitionItems(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStruct(Default::default()), &[NonTerminal(Symbol::StructName(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::ConstructStructItems(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructName(_)), Terminal(Token::WaveBracketOpen(_)), NonTerminal(Symbol::ConstructStructItems(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::ConstructStruct(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStruct(Default::default()), &[NonTerminal(Symbol::StructName(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::ConstructStructItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructName(_)), Terminal(Token::WaveBracketOpen(_)), NonTerminal(Symbol::ConstructStructItems(_)), Terminal(Token::Comma(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::ConstructStruct(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStruct(Default::default()), &[NonTerminal(Symbol::StructName(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructName(_)), Terminal(Token::WaveBracketOpen(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::ConstructStruct(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructName(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_))] = list {
                            Symbol::StructName(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructName(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::GreaterThan(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::CommaSeparatedTypes(_)), Terminal(Token::GreaterThan(_))] = list {
                            Symbol::StructName(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructName(Default::default()), &[NonTerminal(Symbol::FullPath(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FullPath(_))] = list {
                            Symbol::StructName(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructName(Default::default()), &[NonTerminal(Symbol::FullPath(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::CommaSeparatedTypes(Default::default())), Terminal(Token::GreaterThan(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FullPath(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::CommaSeparatedTypes(_)), Terminal(Token::GreaterThan(_))] = list {
                            Symbol::StructName(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructName(Default::default()), &[Terminal(Token::Sharp(Default::default()))],
                        |list| if let [Terminal(Token::Sharp(_))] = list {
                            Symbol::StructName(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructDefinition(Default::default()), &[Terminal(Token::Struct(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Struct(_)), Terminal(Token::Identifier(_)), Terminal(Token::WaveBracketOpen(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::StructDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructDefinition(Default::default()), &[Terminal(Token::Struct(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Struct(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::WaveBracketOpen(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::StructDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructDefinition(Default::default()), &[Terminal(Token::Struct(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::StructDefinitionItems(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Struct(_)), Terminal(Token::Identifier(_)), Terminal(Token::WaveBracketOpen(_)), NonTerminal(Symbol::StructDefinitionItems(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::StructDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StructDefinition(Default::default()), &[Terminal(Token::Struct(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::StructDefinitionItems(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Struct(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::WaveBracketOpen(_)), NonTerminal(Symbol::StructDefinitionItems(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::StructDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStructItems(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_)), Terminal(Token::Colon(_)), NonTerminal(Symbol::Expression(_))] = list {
                            Symbol::ConstructStructItems(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStructItems(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_))] = list {
                            Symbol::ConstructStructItems(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStructItems(Default::default()), &[NonTerminal(Symbol::ConstructStructItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ConstructStructItems(_)), Terminal(Token::Comma(_)), Terminal(Token::Identifier(_)), Terminal(Token::Colon(_)), NonTerminal(Symbol::Expression(_))] = list {
                            Symbol::ConstructStructItems(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ConstructStructItems(Default::default()), &[NonTerminal(Symbol::ConstructStructItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ConstructStructItems(_)), Terminal(Token::Comma(_)), Terminal(Token::Identifier(_))] = list {
                            Symbol::ConstructStructItems(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::LetBinding(Default::default()), &[Terminal(Token::Let(Default::default())), NonTerminal(Symbol::Pattern(Default::default())), Terminal(Token::Assign(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [Terminal(Token::Let(_)), NonTerminal(Symbol::Pattern(_)), Terminal(Token::Assign(_)), NonTerminal(Symbol::Expression(_))] = list {
                            Symbol::LetBinding(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::LetBinding(Default::default()), &[Terminal(Token::Let(Default::default())), NonTerminal(Symbol::Pattern(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Assign(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [Terminal(Token::Let(_)), NonTerminal(Symbol::Pattern(_)), Terminal(Token::Colon(_)), NonTerminal(Symbol::Type(_)), Terminal(Token::Assign(_)), NonTerminal(Symbol::Expression(_))] = list {
                            Symbol::LetBinding(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[NonTerminal(Symbol::ImportStatement(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ImportStatement(_))] = list {
                            Symbol::Statement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::Statement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[NonTerminal(Symbol::LetBinding(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [NonTerminal(Symbol::LetBinding(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::Statement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[Terminal(Token::Return(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Return(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::Statement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[Terminal(Token::Continue(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Continue(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::Statement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[Terminal(Token::Continue(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Continue(_)), Terminal(Token::Identifier(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::Statement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[Terminal(Token::Break(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Break(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::Statement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[Terminal(Token::Break(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Break(_)), Terminal(Token::Identifier(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::Statement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[NonTerminal(Symbol::ForStatement(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ForStatement(_))] = list {
                            Symbol::Statement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Statement(Default::default()), &[NonTerminal(Symbol::WhileStatement(Default::default()))],
                        |list| if let [NonTerminal(Symbol::WhileStatement(_))] = list {
                            Symbol::Statement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StatementList(Default::default()), &[NonTerminal(Symbol::Statement(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Statement(_))] = list {
                            Symbol::StatementList(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::StatementList(Default::default()), &[NonTerminal(Symbol::StatementList(Default::default())), NonTerminal(Symbol::Statement(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StatementList(_)), NonTerminal(Symbol::Statement(_))] = list {
                            Symbol::StatementList(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Block(Default::default()), &[Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::StatementList(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::WaveBracketOpen(_)), NonTerminal(Symbol::StatementList(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::Block(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Block(Default::default()), &[Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::StatementList(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::WaveBracketOpen(_)), NonTerminal(Symbol::StatementList(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::Block(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Block(Default::default()), &[Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::WaveBracketOpen(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::Block(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Block(Default::default()), &[Terminal(Token::WaveBracketOpen(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::WaveBracketOpen(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::Block(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::StringImmediate(Default::default()))],
                        |list| if let [Terminal(Token::StringImmediate(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::IntegerImmediate(Default::default()))],
                        |list| if let [Terminal(Token::IntegerImmediate(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::FloatImmediate(Default::default()))],
                        |list| if let [Terminal(Token::FloatImmediate(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::Dot(Default::default())), NonTerminal(Symbol::FullPath(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8(_)), Terminal(Token::Dot(_)), NonTerminal(Symbol::FullPath(_)), Terminal(Token::RoundBracketOpen(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8(_)), Terminal(Token::RoundBracketOpen(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::Dot(Default::default())), NonTerminal(Symbol::FullPath(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8(_)), Terminal(Token::Dot(_)), NonTerminal(Symbol::FullPath(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::Dot(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8(_)), Terminal(Token::Dot(_)), Terminal(Token::Identifier(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::Dot(Default::default())), NonTerminal(Symbol::FullPath(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedExpressions(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8(_)), Terminal(Token::Dot(_)), NonTerminal(Symbol::FullPath(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::CommaSeparatedExpressions(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedExpressions(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::CommaSeparatedExpressions(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedExpressions(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::CommaSeparatedExpressions(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::SquareBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedExpressions(Default::default())), Terminal(Token::SquareBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::SquareBracketOpen(_)), NonTerminal(Symbol::CommaSeparatedExpressions(_)), Terminal(Token::SquareBracketClose(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Comma(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::SquareBracketOpen(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::SquareBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::SquareBracketOpen(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::SquareBracketClose(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::SquareBracketOpen(Default::default())), Terminal(Token::SquareBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::SquareBracketOpen(_)), Terminal(Token::SquareBracketClose(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::RoundBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::RoundBracketClose(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Block(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression8(Default::default()), &[NonTerminal(Symbol::FullPath(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FullPath(_))] = list {
                            Symbol::Expression8(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression7(Default::default()), &[NonTerminal(Symbol::Expression8(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression8(_))] = list {
                            Symbol::Expression7(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression7(Default::default()), &[Terminal(Token::Sub(Default::default())), NonTerminal(Symbol::Expression7(Default::default()))],
                        |list| if let [Terminal(Token::Sub(_)), NonTerminal(Symbol::Expression7(_))] = list {
                            Symbol::Expression7(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression7(Default::default()), &[Terminal(Token::Not(Default::default())), NonTerminal(Symbol::Expression7(Default::default()))],
                        |list| if let [Terminal(Token::Not(_)), NonTerminal(Symbol::Expression7(_))] = list {
                            Symbol::Expression7(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression6(Default::default()), &[NonTerminal(Symbol::Expression7(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression7(_))] = list {
                            Symbol::Expression6(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression6(Default::default()), &[NonTerminal(Symbol::Expression6(Default::default())), Terminal(Token::Mul(Default::default())), NonTerminal(Symbol::Expression7(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression6(_)), Terminal(Token::Mul(_)), NonTerminal(Symbol::Expression7(_))] = list {
                            Symbol::Expression6(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression6(Default::default()), &[NonTerminal(Symbol::Expression6(Default::default())), Terminal(Token::Div(Default::default())), NonTerminal(Symbol::Expression7(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression6(_)), Terminal(Token::Div(_)), NonTerminal(Symbol::Expression7(_))] = list {
                            Symbol::Expression6(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression6(Default::default()), &[NonTerminal(Symbol::Expression6(Default::default())), Terminal(Token::Rem(Default::default())), NonTerminal(Symbol::Expression7(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression6(_)), Terminal(Token::Rem(_)), NonTerminal(Symbol::Expression7(_))] = list {
                            Symbol::Expression6(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression5(Default::default()), &[NonTerminal(Symbol::Expression6(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression6(_))] = list {
                            Symbol::Expression5(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression5(Default::default()), &[NonTerminal(Symbol::Expression5(Default::default())), Terminal(Token::Add(Default::default())), NonTerminal(Symbol::Expression6(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression5(_)), Terminal(Token::Add(_)), NonTerminal(Symbol::Expression6(_))] = list {
                            Symbol::Expression5(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression5(Default::default()), &[NonTerminal(Symbol::Expression5(Default::default())), Terminal(Token::Sub(Default::default())), NonTerminal(Symbol::Expression6(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression5(_)), Terminal(Token::Sub(_)), NonTerminal(Symbol::Expression6(_))] = list {
                            Symbol::Expression5(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression4(Default::default()), &[NonTerminal(Symbol::Expression5(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression5(_))] = list {
                            Symbol::Expression4(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression4(Default::default()), &[NonTerminal(Symbol::Expression4(Default::default())), Terminal(Token::GreaterThan(Default::default())), NonTerminal(Symbol::Expression5(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression4(_)), Terminal(Token::GreaterThan(_)), NonTerminal(Symbol::Expression5(_))] = list {
                            Symbol::Expression4(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression4(Default::default()), &[NonTerminal(Symbol::Expression4(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::Expression5(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression4(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::Expression5(_))] = list {
                            Symbol::Expression4(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression4(Default::default()), &[NonTerminal(Symbol::Expression4(Default::default())), Terminal(Token::GreaterOrEqual(Default::default())), NonTerminal(Symbol::Expression5(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression4(_)), Terminal(Token::GreaterOrEqual(_)), NonTerminal(Symbol::Expression5(_))] = list {
                            Symbol::Expression4(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression4(Default::default()), &[NonTerminal(Symbol::Expression4(Default::default())), Terminal(Token::LessOrEqual(Default::default())), NonTerminal(Symbol::Expression5(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression4(_)), Terminal(Token::LessOrEqual(_)), NonTerminal(Symbol::Expression5(_))] = list {
                            Symbol::Expression4(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression3(Default::default()), &[NonTerminal(Symbol::Expression4(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression4(_))] = list {
                            Symbol::Expression3(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression3(Default::default()), &[NonTerminal(Symbol::Expression3(Default::default())), Terminal(Token::Equals(Default::default())), NonTerminal(Symbol::Expression4(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression3(_)), Terminal(Token::Equals(_)), NonTerminal(Symbol::Expression4(_))] = list {
                            Symbol::Expression3(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression3(Default::default()), &[NonTerminal(Symbol::Expression3(Default::default())), Terminal(Token::NotEquals(Default::default())), NonTerminal(Symbol::Expression4(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression3(_)), Terminal(Token::NotEquals(_)), NonTerminal(Symbol::Expression4(_))] = list {
                            Symbol::Expression3(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression2(Default::default()), &[NonTerminal(Symbol::Expression3(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression3(_))] = list {
                            Symbol::Expression2(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression2(Default::default()), &[NonTerminal(Symbol::Expression2(Default::default())), Terminal(Token::And(Default::default())), NonTerminal(Symbol::Expression3(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression2(_)), Terminal(Token::And(_)), NonTerminal(Symbol::Expression3(_))] = list {
                            Symbol::Expression2(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression1(Default::default()), &[NonTerminal(Symbol::Expression2(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression2(_))] = list {
                            Symbol::Expression1(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression1(Default::default()), &[NonTerminal(Symbol::Expression1(Default::default())), Terminal(Token::Xor(Default::default())), NonTerminal(Symbol::Expression2(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression1(_)), Terminal(Token::Xor(_)), NonTerminal(Symbol::Expression2(_))] = list {
                            Symbol::Expression1(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression0(Default::default()), &[NonTerminal(Symbol::Expression1(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression1(_))] = list {
                            Symbol::Expression0(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression0(Default::default()), &[NonTerminal(Symbol::Expression0(Default::default())), Terminal(Token::Or(Default::default())), NonTerminal(Symbol::Expression1(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression0(_)), Terminal(Token::Or(_)), NonTerminal(Symbol::Expression1(_))] = list {
                            Symbol::Expression0(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression(Default::default()), &[NonTerminal(Symbol::Expression0(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression0(_))] = list {
                            Symbol::Expression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression(Default::default()), &[NonTerminal(Symbol::ClosureExpression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ClosureExpression(_))] = list {
                            Symbol::Expression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression(Default::default()), &[NonTerminal(Symbol::IfExpression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::IfExpression(_))] = list {
                            Symbol::Expression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Expression(Default::default()), &[NonTerminal(Symbol::MatchExpression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::MatchExpression(_))] = list {
                            Symbol::Expression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ClosureExpression(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::ClosureArguments(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Colon(_)), NonTerminal(Symbol::ClosureArguments(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_))] = list {
                            Symbol::ClosureExpression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ClosureExpression(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Colon(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Colon(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_))] = list {
                            Symbol::ClosureExpression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::IfExpression(Default::default()), &[Terminal(Token::If(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::If(_)), NonTerminal(Symbol::Expression(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::IfExpression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::IfExpression(Default::default()), &[Terminal(Token::If(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default())), Terminal(Token::Else(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::If(_)), NonTerminal(Symbol::Expression(_)), NonTerminal(Symbol::Block(_)), Terminal(Token::Else(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::IfExpression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::IfExpression(Default::default()), &[Terminal(Token::If(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default())), Terminal(Token::Else(Default::default())), NonTerminal(Symbol::IfExpression(Default::default()))],
                        |list| if let [Terminal(Token::If(_)), NonTerminal(Symbol::Expression(_)), NonTerminal(Symbol::Block(_)), Terminal(Token::Else(_)), NonTerminal(Symbol::IfExpression(_))] = list {
                            Symbol::IfExpression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::IfExpression(Default::default()), &[Terminal(Token::If(Default::default())), NonTerminal(Symbol::LetBinding(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::If(_)), NonTerminal(Symbol::LetBinding(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::IfExpression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::IfExpression(Default::default()), &[Terminal(Token::If(Default::default())), NonTerminal(Symbol::LetBinding(Default::default())), NonTerminal(Symbol::Block(Default::default())), Terminal(Token::Else(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::If(_)), NonTerminal(Symbol::LetBinding(_)), NonTerminal(Symbol::Block(_)), Terminal(Token::Else(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::IfExpression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::IfExpression(Default::default()), &[Terminal(Token::If(Default::default())), NonTerminal(Symbol::LetBinding(Default::default())), NonTerminal(Symbol::Block(Default::default())), Terminal(Token::Else(Default::default())), NonTerminal(Symbol::IfExpression(Default::default()))],
                        |list| if let [Terminal(Token::If(_)), NonTerminal(Symbol::LetBinding(_)), NonTerminal(Symbol::Block(_)), Terminal(Token::Else(_)), NonTerminal(Symbol::IfExpression(_))] = list {
                            Symbol::IfExpression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::MatchExpression(Default::default()), &[Terminal(Token::Match(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::MatchPatterns(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Match(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::WaveBracketOpen(_)), NonTerminal(Symbol::MatchPatterns(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::MatchExpression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::MatchExpression(Default::default()), &[Terminal(Token::Match(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::MatchPatterns(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::Match(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::WaveBracketOpen(_)), NonTerminal(Symbol::MatchPatterns(_)), Terminal(Token::Comma(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::MatchExpression(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::MatchPatterns(Default::default()), &[NonTerminal(Symbol::Pattern(Default::default())), Terminal(Token::MatchArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Pattern(_)), Terminal(Token::MatchArrow(_)), NonTerminal(Symbol::Expression(_))] = list {
                            Symbol::MatchPatterns(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::MatchPatterns(Default::default()), &[NonTerminal(Symbol::MatchPatterns(Default::default())), Terminal(Token::Comma(Default::default())), NonTerminal(Symbol::Pattern(Default::default())), Terminal(Token::MatchArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::MatchPatterns(_)), Terminal(Token::Comma(_)), NonTerminal(Symbol::Pattern(_)), Terminal(Token::MatchArrow(_)), NonTerminal(Symbol::Expression(_))] = list {
                            Symbol::MatchPatterns(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ForStatement(Default::default()), &[Terminal(Token::For(Default::default())), NonTerminal(Symbol::Pattern(Default::default())), Terminal(Token::In(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::For(_)), NonTerminal(Symbol::Pattern(_)), Terminal(Token::In(_)), NonTerminal(Symbol::Expression(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::ForStatement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ForStatement(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), Terminal(Token::For(Default::default())), NonTerminal(Symbol::Pattern(Default::default())), Terminal(Token::In(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_)), Terminal(Token::Colon(_)), Terminal(Token::For(_)), NonTerminal(Symbol::Pattern(_)), Terminal(Token::In(_)), NonTerminal(Symbol::Expression(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::ForStatement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::WhileStatement(Default::default()), &[Terminal(Token::While(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::While(_)), NonTerminal(Symbol::Expression(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::WhileStatement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::WhileStatement(Default::default()), &[Terminal(Token::While(Default::default())), NonTerminal(Symbol::LetBinding(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::While(_)), NonTerminal(Symbol::LetBinding(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::WhileStatement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::WhileStatement(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), Terminal(Token::While(Default::default())), NonTerminal(Symbol::Expression(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_)), Terminal(Token::Colon(_)), Terminal(Token::While(_)), NonTerminal(Symbol::Expression(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::WhileStatement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::WhileStatement(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), Terminal(Token::While(Default::default())), NonTerminal(Symbol::LetBinding(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_)), Terminal(Token::Colon(_)), Terminal(Token::While(_)), NonTerminal(Symbol::LetBinding(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::WhileStatement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Pattern(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_))] = list {
                            Symbol::Pattern(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Pattern(Default::default()), &[NonTerminal(Symbol::StructName(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructName(_)), Terminal(Token::WaveBracketOpen(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::Pattern(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Pattern(Default::default()), &[NonTerminal(Symbol::StructName(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::PatternStructItems(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructName(_)), Terminal(Token::WaveBracketOpen(_)), NonTerminal(Symbol::PatternStructItems(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::Pattern(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::Pattern(Default::default()), &[NonTerminal(Symbol::StructName(Default::default())), Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::PatternStructItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [NonTerminal(Symbol::StructName(_)), Terminal(Token::WaveBracketOpen(_)), NonTerminal(Symbol::PatternStructItems(_)), Terminal(Token::Comma(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::Pattern(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::PatternStructItems(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_))] = list {
                            Symbol::PatternStructItems(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::PatternStructItems(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Pattern(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_)), Terminal(Token::Colon(_)), NonTerminal(Symbol::Pattern(_))] = list {
                            Symbol::PatternStructItems(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::PatternStructItems(Default::default()), &[NonTerminal(Symbol::PatternStructItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [NonTerminal(Symbol::PatternStructItems(_)), Terminal(Token::Comma(_)), Terminal(Token::Identifier(_))] = list {
                            Symbol::PatternStructItems(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::PatternStructItems(Default::default()), &[NonTerminal(Symbol::PatternStructItems(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Pattern(Default::default()))],
                        |list| if let [NonTerminal(Symbol::PatternStructItems(_)), Terminal(Token::Comma(_)), Terminal(Token::Identifier(_)), Terminal(Token::Colon(_)), NonTerminal(Symbol::Pattern(_))] = list {
                            Symbol::PatternStructItems(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ClosureArguments(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_))] = list {
                            Symbol::ClosureArguments(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ClosureArguments(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_)), Terminal(Token::Colon(_)), NonTerminal(Symbol::Type(_))] = list {
                            Symbol::ClosureArguments(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ClosureArguments(Default::default()), &[NonTerminal(Symbol::ClosureArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ClosureArguments(_)), Terminal(Token::Comma(_)), Terminal(Token::Identifier(_))] = list {
                            Symbol::ClosureArguments(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ClosureArguments(Default::default()), &[NonTerminal(Symbol::ClosureArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ClosureArguments(_)), Terminal(Token::Comma(_)), Terminal(Token::Identifier(_)), Terminal(Token::Colon(_)), NonTerminal(Symbol::Type(_))] = list {
                            Symbol::ClosureArguments(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::CommaSeparatedExpressions(Default::default()), &[NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Comma(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::Expression(_)), Terminal(Token::Comma(_)), NonTerminal(Symbol::Expression(_))] = list {
                            Symbol::CommaSeparatedExpressions(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::CommaSeparatedExpressions(Default::default()), &[NonTerminal(Symbol::CommaSeparatedExpressions(Default::default())), Terminal(Token::Comma(Default::default())), NonTerminal(Symbol::Expression(Default::default()))],
                        |list| if let [NonTerminal(Symbol::CommaSeparatedExpressions(_)), Terminal(Token::Comma(_)), NonTerminal(Symbol::Expression(_))] = list {
                            Symbol::CommaSeparatedExpressions(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::RoundBracketClose(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::RoundBracketClose(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::Comma(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::Comma(_)), Terminal(Token::RoundBracketClose(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::Comma(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::Comma(_)), Terminal(Token::RoundBracketClose(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::RoundBracketOpen(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::RoundBracketOpen(_)), Terminal(Token::RoundBracketClose(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::RoundBracketOpen(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), NonTerminal(Symbol::Block(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::RoundBracketOpen(_)), Terminal(Token::RoundBracketClose(_)), NonTerminal(Symbol::Block(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::Comma(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::Comma(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::Comma(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::RoundBracketOpen(_)), NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::Comma(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::RoundBracketOpen(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::RoundBracketOpen(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Type(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::RoundBracketOpen(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Type(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionDefinition(Default::default()), &[Terminal(Token::Fn(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), Terminal(Token::LessThan(Default::default())), NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::GreaterThan(Default::default())), Terminal(Token::RoundBracketOpen(Default::default())), Terminal(Token::RoundBracketClose(Default::default())), Terminal(Token::Or(Default::default())), Terminal(Token::FunctionArrow(Default::default())), NonTerminal(Symbol::Expression(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Fn(_)), Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), Terminal(Token::LessThan(_)), NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::GreaterThan(_)), Terminal(Token::RoundBracketOpen(_)), Terminal(Token::RoundBracketClose(_)), Terminal(Token::Or(_)), Terminal(Token::FunctionArrow(_)), NonTerminal(Symbol::Expression(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::FunctionDefinition(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionArguments(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_)), Terminal(Token::Colon(_)), NonTerminal(Symbol::Type(_))] = list {
                            Symbol::FunctionArguments(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::FunctionArguments(Default::default()), &[NonTerminal(Symbol::FunctionArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default())), Terminal(Token::Colon(Default::default())), NonTerminal(Symbol::Type(Default::default()))],
                        |list| if let [NonTerminal(Symbol::FunctionArguments(_)), Terminal(Token::Comma(_)), Terminal(Token::Identifier(_)), Terminal(Token::Colon(_)), NonTerminal(Symbol::Type(_))] = list {
                            Symbol::FunctionArguments(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ImportStatement(Default::default()), &[Terminal(Token::Import(Default::default())), NonTerminal(Symbol::ImportPath(Default::default())), Terminal(Token::Semicolon(Default::default()))],
                        |list| if let [Terminal(Token::Import(_)), NonTerminal(Symbol::ImportPath(_)), Terminal(Token::Semicolon(_))] = list {
                            Symbol::ImportStatement(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ImportPath(Default::default()), &[NonTerminal(Symbol::ImportPathChild(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ImportPathChild(_))] = list {
                            Symbol::ImportPath(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ImportPath(Default::default()), &[Terminal(Token::StringImmediate(Default::default())), Terminal(Token::DoubleColon(Default::default())), NonTerminal(Symbol::ImportPathChild(Default::default()))],
                        |list| if let [Terminal(Token::StringImmediate(_)), Terminal(Token::DoubleColon(_)), NonTerminal(Symbol::ImportPathChild(_))] = list {
                            Symbol::ImportPath(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ImportPathChild(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_))] = list {
                            Symbol::ImportPathChild(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ImportPathChild(Default::default()), &[Terminal(Token::Identifier(Default::default())), Terminal(Token::DoubleColon(Default::default())), NonTerminal(Symbol::ImportPathChild(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_)), Terminal(Token::DoubleColon(_)), NonTerminal(Symbol::ImportPathChild(_))] = list {
                            Symbol::ImportPathChild(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::ImportPathChild(Default::default()), &[Terminal(Token::WaveBracketOpen(Default::default())), NonTerminal(Symbol::CommaSeparatedImportPathChildren(Default::default())), Terminal(Token::WaveBracketClose(Default::default()))],
                        |list| if let [Terminal(Token::WaveBracketOpen(_)), NonTerminal(Symbol::CommaSeparatedImportPathChildren(_)), Terminal(Token::WaveBracketClose(_))] = list {
                            Symbol::ImportPathChild(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::CommaSeparatedImportPathChildren(Default::default()), &[NonTerminal(Symbol::ImportPathChild(Default::default()))],
                        |list| if let [NonTerminal(Symbol::ImportPathChild(_))] = list {
                            Symbol::CommaSeparatedImportPathChildren(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::CommaSeparatedImportPathChildren(Default::default()), &[NonTerminal(Symbol::CommaSeparatedImportPathChildren(Default::default())), Terminal(Token::Comma(Default::default())), NonTerminal(Symbol::ImportPathChild(Default::default()))],
                        |list| if let [NonTerminal(Symbol::CommaSeparatedImportPathChildren(_)), Terminal(Token::Comma(_)), NonTerminal(Symbol::ImportPathChild(_))] = list {
                            Symbol::CommaSeparatedImportPathChildren(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::GenericsTypeArguments(Default::default()), &[Terminal(Token::Identifier(Default::default()))],
                        |list| if let [Terminal(Token::Identifier(_))] = list {
                            Symbol::GenericsTypeArguments(todo!())
                        } else { unreachable!() }))
        .rule(Rule::new(Symbol::GenericsTypeArguments(Default::default()), &[NonTerminal(Symbol::GenericsTypeArguments(Default::default())), Terminal(Token::Comma(Default::default())), Terminal(Token::Identifier(Default::default()))],
                        |list| if let [NonTerminal(Symbol::GenericsTypeArguments(_)), Terminal(Token::Comma(_)), Terminal(Token::Identifier(_))] = list {
                            Symbol::GenericsTypeArguments(todo!())
                        } else { unreachable!() }))
        .build(Symbol::ValidKirlCode(Default::default())));
    println!("{}", warning.len());
    println!("{}", warning.into_iter().map(|w| format!("{:?}", w)).collect::<Vec<_>>().join("\n"));
    parser
}

#[cfg(test)]
mod tests {
    use crate::parser::get_parser;

    #[test]
    fn test_parse() {
        let parser = get_parser();
    }
}
