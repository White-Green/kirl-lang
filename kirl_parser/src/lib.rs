use arrayvec::ArrayVec;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Range;

use once_cell::unsync::OnceCell;
use parser::{Parse, ParseError};
use tokenizer::Tokenize;

use crate::kirl_parser::{get_parser, KirlTopLevelStatement, Parser, Symbol};
use crate::kirl_tokenizer::{get_tokenizer, TokenizeError, Tokenizer};

pub mod kirl_parser;
pub mod kirl_tokenizer;

#[derive(Clone, Default, Copy, Debug, PartialEq)]
pub struct CharacterPosition {
    pub line: usize,
    pub column: usize,
}

impl CharacterPosition {
    pub fn zero() -> Self {
        Self::default()
    }

    pub fn new(line: usize, column: usize) -> Self {
        CharacterPosition { line, column }
    }

    pub fn next(self) -> Self {
        let CharacterPosition { line, column } = self;
        CharacterPosition { line, column: column + 1 }
    }

    pub fn next_line(self) -> Self {
        let CharacterPosition { line, .. } = self;
        CharacterPosition { line: line + 1, column: 0 }
    }
}

impl Display for CharacterPosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let CharacterPosition { line, column } = self;
        write!(f, "Line:{},position:{}", line + 1, column)
    }
}

#[derive(Debug)]
pub enum KirlParseError {
    TokenizeError(TokenizeError),
    ParseError(ParseError<ParseErrorDetail>),
}

impl Display for KirlParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KirlParseError::TokenizeError(e) => Display::fmt(e, f),
            KirlParseError::ParseError(e) => Display::fmt(e, f),
        }
    }
}

impl Error for KirlParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            KirlParseError::TokenizeError(e) => Some(e),
            KirlParseError::ParseError(e) => Some(e),
        }
    }
}

#[derive(Debug)]
pub enum ParseErrorDetail {
    SyntaxErrorAt(Range<CharacterPosition>),
}

impl Display for ParseErrorDetail {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorDetail::SyntaxErrorAt(Range { start, end }) => {
                write!(f, "Syntax Error at {} - {}", start, end)
            }
        }
    }
}

impl Error for ParseErrorDetail {}

pub struct KirlParser {
    tokenizer: Tokenizer,
    parser: Parser,
}

impl Default for KirlParser {
    fn default() -> Self {
        Self::new()
    }
}

impl KirlParser {
    pub fn new() -> Self {
        KirlParser { tokenizer: get_tokenizer(), parser: get_parser() }
    }

    pub fn parse(&self, input: &str) -> Result<Vec<KirlTopLevelStatement>, KirlParseError> {
        let cell = OnceCell::new();
        let parse_result = input
            .chars()
            .scan(CharacterPosition::zero(), |position, c| {
                let current_position = *position;
                *position = match c {
                    '\n' => position.next_line(),
                    _ => position.next(),
                };
                Some((current_position, c))
            })
            .tokenize_with(&self.tokenizer, |(_, c)| *c)
            .flat_map(|token| match token {
                Ok(token) => token,
                Err(e) => {
                    let _ = cell.set(e);
                    ArrayVec::new()
                }
            })
            .parse(&self.parser);
        cell.into_inner().map(KirlParseError::TokenizeError).map_or(Ok(()), Err).and_then(|_| match parse_result {
            Ok(Symbol::ValidKirlCode((_, code))) => Ok(code),
            Ok(_) => unreachable!("parserの仕様として、ValidKirlCodeに還元できないならErrになるのであんりーちゃぶる"),
            Err(e) => Err(KirlParseError::ParseError(e)),
        })
    }
}
