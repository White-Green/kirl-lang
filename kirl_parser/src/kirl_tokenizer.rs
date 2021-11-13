use arrayvec::ArrayVec;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Range;
use std::str::FromStr;

use kirl_common::dec::{Decimal128, ParseDecimalError};
use parser::enum_index;
use parser::enum_index_derive::*;
use tokenizer::DFATokenizer;
use tokenizer_generator::tokenizer;

use crate::CharacterPosition;

#[derive(Debug, PartialEq, EnumIndex)]
pub enum Token {
    /// import
    Import(Range<CharacterPosition>),
    /// fn
    Fn(Range<CharacterPosition>),
    /// struct
    Struct(Range<CharacterPosition>),
    /// let
    Let(Range<CharacterPosition>),
    /// var
    Var(Range<CharacterPosition>),
    /// if
    If(Range<CharacterPosition>),
    /// else
    Else(Range<CharacterPosition>),
    /// match
    Match(Range<CharacterPosition>),
    /// for
    For(Range<CharacterPosition>),
    /// in
    In(Range<CharacterPosition>),
    /// while
    While(Range<CharacterPosition>),
    /// return
    Return(Range<CharacterPosition>),
    /// break
    Break(Range<CharacterPosition>),
    /// continue
    Continue(Range<CharacterPosition>),
    /// タプルのn番目にアクセスするみたいなの
    TupleIndex((Range<CharacterPosition>, usize)),
    /// 変数とかの識別子
    Identifier((Range<CharacterPosition>, String)),
    /// 文字列即値
    StringImmediate((Range<CharacterPosition>, String)),
    /// 数値即値
    NumberImmediate((Range<CharacterPosition>, Decimal128)),
    /// !
    Not(Range<CharacterPosition>),
    /// .
    Dot(Range<CharacterPosition>),
    /// ,
    Comma(Range<CharacterPosition>),
    /// #
    Sharp(Range<CharacterPosition>),
    /// ::
    DoubleColon(Range<CharacterPosition>),
    /// :
    Colon(Range<CharacterPosition>),
    /// ;
    Semicolon(Range<CharacterPosition>),
    /// >
    GreaterThan(Range<CharacterPosition>),
    /// <
    LessThan(Range<CharacterPosition>),
    /// >=
    GreaterOrEqual(Range<CharacterPosition>),
    /// <=
    LessOrEqual(Range<CharacterPosition>),
    /// =
    Assign(Range<CharacterPosition>),
    /// ==
    Equals(Range<CharacterPosition>),
    /// !=
    NotEquals(Range<CharacterPosition>),
    /// +
    Add(Range<CharacterPosition>),
    /// -
    Sub(Range<CharacterPosition>),
    /// *
    Mul(Range<CharacterPosition>),
    /// /
    Div(Range<CharacterPosition>),
    /// %
    Rem(Range<CharacterPosition>),
    /// &
    And(Range<CharacterPosition>),
    /// |
    Or(Range<CharacterPosition>),
    /// ^
    Xor(Range<CharacterPosition>),
    /// (
    RoundBracketOpen(Range<CharacterPosition>),
    /// )
    RoundBracketClose(Range<CharacterPosition>),
    /// [
    SquareBracketOpen(Range<CharacterPosition>),
    /// ]
    SquareBracketClose(Range<CharacterPosition>),
    /// {
    WaveBracketOpen(Range<CharacterPosition>),
    /// }
    WaveBracketClose(Range<CharacterPosition>),
    /// ->
    FunctionArrow(Range<CharacterPosition>),
    /// |->
    MapsTo(Range<CharacterPosition>),
    /// =>
    MatchArrow(Range<CharacterPosition>),
}

impl Token {
    pub fn get_position(&self) -> &Range<CharacterPosition> {
        match self {
            Token::Import(range) => range,
            Token::Fn(range) => range,
            Token::Struct(range) => range,
            Token::Let(range) => range,
            Token::Var(range) => range,
            Token::If(range) => range,
            Token::Else(range) => range,
            Token::Match(range) => range,
            Token::For(range) => range,
            Token::In(range) => range,
            Token::While(range) => range,
            Token::Return(range) => range,
            Token::Break(range) => range,
            Token::Continue(range) => range,
            Token::TupleIndex((range, _)) => range,
            Token::Identifier((range, _)) => range,
            Token::StringImmediate((range, _)) => range,
            Token::NumberImmediate((range, _)) => range,
            Token::Not(range) => range,
            Token::Dot(range) => range,
            Token::Comma(range) => range,
            Token::Sharp(range) => range,
            Token::DoubleColon(range) => range,
            Token::Colon(range) => range,
            Token::Semicolon(range) => range,
            Token::GreaterThan(range) => range,
            Token::LessThan(range) => range,
            Token::GreaterOrEqual(range) => range,
            Token::LessOrEqual(range) => range,
            Token::Assign(range) => range,
            Token::Equals(range) => range,
            Token::NotEquals(range) => range,
            Token::Add(range) => range,
            Token::Sub(range) => range,
            Token::Mul(range) => range,
            Token::Div(range) => range,
            Token::Rem(range) => range,
            Token::And(range) => range,
            Token::Or(range) => range,
            Token::Xor(range) => range,
            Token::RoundBracketOpen(range) => range,
            Token::RoundBracketClose(range) => range,
            Token::SquareBracketOpen(range) => range,
            Token::SquareBracketClose(range) => range,
            Token::WaveBracketOpen(range) => range,
            Token::WaveBracketClose(range) => range,
            Token::FunctionArrow(range) => range,
            Token::MapsTo(range) => range,
            Token::MatchArrow(range) => range,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenizeError {
    IntegerParseError { raw: String, position: Range<CharacterPosition> },
    DecimalParseError { raw: String, position: Range<CharacterPosition>, error: ParseDecimalError },
    StringParseError { raw: String, position: Range<CharacterPosition> },
    UnknownCharacter { character: char, position: CharacterPosition },
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl Error for TokenizeError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            TokenizeError::DecimalParseError { error, .. } => Some(error),
            _ => None,
        }
    }
}

macro_rules! array {
    ($($e:expr),*) => { [$($e),*].into_iter().collect() }
}

pub type Tokenizer = DFATokenizer<Result<ArrayVec<Token, 2>, TokenizeError>, (CharacterPosition, char)>;

tokenizer! {
    pub fn get_tokenizer() -> DFATokenizer {
        character (CharacterPosition, char);
        token Result<ArrayVec<Token, 2>, TokenizeError>;
        "//.*": |_, _| Ok(array![]);
        "/\\*(\\*[^/]|[^\\*])*\\*/": |_, _| Ok(array![]);
        "\\s": |_, _| Ok(array![]);
        "import": |_, v| Ok(array![Token::Import(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "fn": |_, v| Ok(array![Token::Fn(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "struct": |_, v| Ok(array![Token::Struct(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "let": |_, v| Ok(array![Token::Let(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "var": |_, v| Ok(array![Token::Var(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "if": |_, v| Ok(array![Token::If(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "else": |_, v| Ok(array![Token::Else(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "match": |_, v| Ok(array![Token::Match(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "for": |_, v| Ok(array![Token::For(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "in": |_, v| Ok(array![Token::In(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "while": |_, v| Ok(array![Token::While(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "return": |_, v| Ok(array![Token::Return(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "break": |_, v| Ok(array![Token::Break(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "continue": |_, v| Ok(array![Token::Continue(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\.[0-9][1-9]*(th|st|nd|rd)": |s, v| Ok(array![Token::Dot(v.first().unwrap().0..v[1].0), Token::TupleIndex((v[1].0..v.last().unwrap().0.next(), parse_tuple_index(&s[1..])))]);
        "[0-9][1-9]*(th|st|nd|rd)": |s, v| Ok(array![Token::TupleIndex((v.first().unwrap().0..v.last().unwrap().0.next(), parse_tuple_index(s)))]);
        "[a-zA-Z_][a-zA-Z0-9_]*": |s, v| Ok(array![Token::Identifier((v.first().unwrap().0..v.last().unwrap().0.next(), s.to_string()))]);
        "\"(\\\\(\n|\r|\r\n|r|n|t|x[0-9a-fA-F]{2}|u\\{[0-9a-fA-F]{1,6}\\}|\\\\|\")|[^\\\\\"])*\"": |s, v| {
                parse_string_literal(s)
                    .ok_or_else(|| TokenizeError::StringParseError {
                        raw: s.to_string(),
                        position: v.first().unwrap().0..v.last().unwrap().0.next(),
                    })
                    .map(|s| array![Token::StringImmediate((v.first().unwrap().0..v.last().unwrap().0.next(), s))])
            };
        "0b[01_]*[01]|0o[0-7_]*[0-7]|0d[0-9_]*[0-9]|0x[0-9a-fA-F_]*[0-9a-fA-F]|[0-9]|[0-9][0-9_]*[0-9]": |s, v| Ok(array![Token::NumberImmediate((v.first().unwrap().0..v.last().unwrap().0.next(), parse_integer(s)))]);
        "([0-9][0-9_]*[0-9]|[0-9]|([0-9][0-9_]*)?\\.[0-9_]*[0-9])([eE][\\+\\-]?[0-9_]*[0-9])?": |s, v| {
                Decimal128::from_str(&s.replace("_", ""))
                    .map(|f| array![Token::NumberImmediate((v.first().unwrap().0..v.last().unwrap().0.next(), f))])
                    .map_err(
                        |e| TokenizeError::DecimalParseError {
                            raw: s.to_string(),
                            position: v.first().unwrap().0..v.last().unwrap().0.next(),
                            error: e,
                        })
        };
        "!": |_, v| Ok(array![Token::Not(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\.": |_, v| Ok(array![Token::Dot(v.first().unwrap().0..v.last().unwrap().0.next())]);
        ",": |_, v| Ok(array![Token::Comma(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "#": |_, v| Ok(array![Token::Sharp(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "::": |_, v| Ok(array![Token::DoubleColon(v.first().unwrap().0..v.last().unwrap().0.next())]);
        ":": |_, v| Ok(array![Token::Colon(v.first().unwrap().0..v.last().unwrap().0.next())]);
        ";": |_, v| Ok(array![Token::Semicolon(v.first().unwrap().0..v.last().unwrap().0.next())]);
        ">": |_, v| Ok(array![Token::GreaterThan(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "<": |_, v| Ok(array![Token::LessThan(v.first().unwrap().0..v.last().unwrap().0.next())]);
        ">=": |_, v| Ok(array![Token::GreaterOrEqual(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "<=": |_, v| Ok(array![Token::LessOrEqual(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "=": |_, v| Ok(array![Token::Assign(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "==": |_, v| Ok(array![Token::Equals(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "!=": |_, v| Ok(array![Token::NotEquals(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\+": |_, v| Ok(array![Token::Add(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\-": |_, v| Ok(array![Token::Sub(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\*": |_, v| Ok(array![Token::Mul(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "/": |_, v| Ok(array![Token::Div(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "%": |_, v| Ok(array![Token::Rem(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "&": |_, v| Ok(array![Token::And(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\|": |_, v| Ok(array![Token::Or(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\^": |_, v| Ok(array![Token::Xor(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\(": |_, v| Ok(array![Token::RoundBracketOpen(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\)": |_, v| Ok(array![Token::RoundBracketClose(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\[": |_, v| Ok(array![Token::SquareBracketOpen(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\]": |_, v| Ok(array![Token::SquareBracketClose(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\{": |_, v| Ok(array![Token::WaveBracketOpen(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\}": |_, v| Ok(array![Token::WaveBracketClose(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "->": |_, v| Ok(array![Token::FunctionArrow(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "\\|->": |_, v| Ok(array![Token::MapsTo(v.first().unwrap().0..v.last().unwrap().0.next())]);
        "=>": |_, v| Ok(array![Token::MatchArrow(v.first().unwrap().0..v.last().unwrap().0.next())]);
        ".|\n": |_, v| Err(TokenizeError::UnknownCharacter { character: v.first().unwrap().1, position: v.first().unwrap().0 });
    }
}

fn parse_tuple_index(s: &str) -> usize {
    usize::from_str(&s[..s.len() - 2]).expect("正規表現でチェックしてるのであんりーちゃぶる(オーバーフローする場合があるが)")
}

fn parse_integer(s: &str) -> Decimal128 {
    let s = s.replace("_", "");
    let (s, radix) = if let Some(s) = s.strip_prefix("0b") {
        (s, 2)
    } else if let Some(s) = s.strip_prefix("0o") {
        (s, 8)
    } else if let Some(s) = s.strip_prefix("0d") {
        (s, 10)
    } else if let Some(s) = s.strip_prefix("0x") {
        (s, 16)
    } else {
        (s.as_str(), 10)
    };
    let dec_radix = Decimal128::from(radix);
    s.chars().fold(Decimal128::ZERO, |acc, c| acc * dec_radix + Decimal128::from(c.to_digit(radix as u32).expect("正規表現でチェックしてるのであんりーちゃぶる")))
}

fn parse_string_literal(s: &str) -> Option<String> {
    let mut iter = s[1..s.len() - 1].chars().peekable();
    let mut result = String::with_capacity(s.len());
    while let Some(c) = iter.next() {
        if c != '\\' {
            result.push(c);
            continue;
        }
        match iter.next() {
            Some('\n') => {}
            Some('\r') => {
                iter.next_if(|&c| c == '\n');
            }
            Some('r') => result.push('\r'),
            Some('n') => result.push('\n'),
            Some('t') => result.push('\t'),
            Some('\\') => result.push('\\'),
            Some('\"') => result.push('\"'),
            Some('x') => {
                let c = iter.by_ref().take(2).fold(0, |acc, c| acc << 4 | c.to_digit(16).expect("事前にとーくないざの正規表現で確認してるのであんりーちゃぶる"));
                result.push(char::from_u32(c).expect("16進2桁なのであんりーちゃぶる"));
            }
            Some('u') => {
                let c = iter.by_ref().skip(1).take_while(|c| *c != '}').fold(0, |acc, c| acc << 4 | c.to_digit(16).expect("事前にとーくないざの正規表現で確認してるのであんりーちゃぶる"));
                result.push(char::from_u32(c)?);
            }
            _ => unreachable!("事前にとーくないざの正規表現で確認してるのであんりーちゃぶる"),
        }
    }
    result.shrink_to_fit();
    Some(result)
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use kirl_common::dec::Decimal128;
    use tokenizer::Tokenize;

    use crate::kirl_tokenizer::{get_tokenizer, CharacterPosition, Token};

    #[test]
    fn test_tokenize() {
        let tokenizer = get_tokenizer();
        let tokenize = |s: &str| {
            s.chars()
                .scan(CharacterPosition::zero(), |position, c| {
                    let current_position = *position;
                    *position = match c {
                        '\n' => position.next_line(),
                        _ => position.next(),
                    };
                    Some((current_position, c))
                })
                .tokenize_with(&tokenizer, |(_, c)| *c)
                .collect::<Vec<_>>()
        };
        const TEXT: &str = "\
import fn
struct let
if else
match for
in while
return ::
: ;
> <
>= <=
= ==
+ -
* /
% &
| ^
( ).0th
[ ].3rd
{ }. 5th
-> =>
. #
, !=
! break
continue // Comment test
/* Comment
test2 */
";
        println!("{:?}", TEXT);
        let vec = tokenize(TEXT);
        println!("{:?}", vec);
        let new = CharacterPosition::new;
        assert_eq!(
            vec,
            vec![
                Ok(array![Token::Import(new(0, 0)..new(0, 6))]),
                Ok(array![]),
                Ok(array![Token::Fn(new(0, 7)..new(0, 9))]),
                Ok(array![]),
                Ok(array![Token::Struct(new(1, 0)..new(1, 6))]),
                Ok(array![]),
                Ok(array![Token::Let(new(1, 7)..new(1, 10))]),
                Ok(array![]),
                Ok(array![Token::If(new(2, 0)..new(2, 2))]),
                Ok(array![]),
                Ok(array![Token::Else(new(2, 3)..new(2, 7))]),
                Ok(array![]),
                Ok(array![Token::Match(new(3, 0)..new(3, 5))]),
                Ok(array![]),
                Ok(array![Token::For(new(3, 6)..new(3, 9))]),
                Ok(array![]),
                Ok(array![Token::In(new(4, 0)..new(4, 2))]),
                Ok(array![]),
                Ok(array![Token::While(new(4, 3)..new(4, 8))]),
                Ok(array![]),
                Ok(array![Token::Return(new(5, 0)..new(5, 6))]),
                Ok(array![]),
                Ok(array![Token::DoubleColon(new(5, 7)..new(5, 9))]),
                Ok(array![]),
                Ok(array![Token::Colon(new(6, 0)..new(6, 1))]),
                Ok(array![]),
                Ok(array![Token::Semicolon(new(6, 2)..new(6, 3))]),
                Ok(array![]),
                Ok(array![Token::GreaterThan(new(7, 0)..new(7, 1))]),
                Ok(array![]),
                Ok(array![Token::LessThan(new(7, 2)..new(7, 3))]),
                Ok(array![]),
                Ok(array![Token::GreaterOrEqual(new(8, 0)..new(8, 2))]),
                Ok(array![]),
                Ok(array![Token::LessOrEqual(new(8, 3)..new(8, 5))]),
                Ok(array![]),
                Ok(array![Token::Assign(new(9, 0)..new(9, 1))]),
                Ok(array![]),
                Ok(array![Token::Equals(new(9, 2)..new(9, 4))]),
                Ok(array![]),
                Ok(array![Token::Add(new(10, 0)..new(10, 1))]),
                Ok(array![]),
                Ok(array![Token::Sub(new(10, 2)..new(10, 3))]),
                Ok(array![]),
                Ok(array![Token::Mul(new(11, 0)..new(11, 1))]),
                Ok(array![]),
                Ok(array![Token::Div(new(11, 2)..new(11, 3))]),
                Ok(array![]),
                Ok(array![Token::Rem(new(12, 0)..new(12, 1))]),
                Ok(array![]),
                Ok(array![Token::And(new(12, 2)..new(12, 3))]),
                Ok(array![]),
                Ok(array![Token::Or(new(13, 0)..new(13, 1))]),
                Ok(array![]),
                Ok(array![Token::Xor(new(13, 2)..new(13, 3))]),
                Ok(array![]),
                Ok(array![Token::RoundBracketOpen(new(14, 0)..new(14, 1))]),
                Ok(array![]),
                Ok(array![Token::RoundBracketClose(new(14, 2)..new(14, 3))]),
                Ok(array![Token::Dot(new(14, 3)..new(14, 4)), Token::TupleIndex((new(14, 4)..new(14, 7), 0))]),
                Ok(array![]),
                Ok(array![Token::SquareBracketOpen(new(15, 0)..new(15, 1))]),
                Ok(array![]),
                Ok(array![Token::SquareBracketClose(new(15, 2)..new(15, 3))]),
                Ok(array![Token::Dot(new(15, 3)..new(15, 4)), Token::TupleIndex((new(15, 4)..new(15, 7), 3))]),
                Ok(array![]),
                Ok(array![Token::WaveBracketOpen(new(16, 0)..new(16, 1))]),
                Ok(array![]),
                Ok(array![Token::WaveBracketClose(new(16, 2)..new(16, 3))]),
                Ok(array![Token::Dot(new(16, 3)..new(16, 4))]),
                Ok(array![]),
                Ok(array![Token::TupleIndex((new(16, 5)..new(16, 8), 5))]),
                Ok(array![]),
                Ok(array![Token::FunctionArrow(new(17, 0)..new(17, 2))]),
                Ok(array![]),
                Ok(array![Token::MatchArrow(new(17, 3)..new(17, 5))]),
                Ok(array![]),
                Ok(array![Token::Dot(new(18, 0)..new(18, 1))]),
                Ok(array![]),
                Ok(array![Token::Sharp(new(18, 2)..new(18, 3))]),
                Ok(array![]),
                Ok(array![Token::Comma(new(19, 0)..new(19, 1))]),
                Ok(array![]),
                Ok(array![Token::NotEquals(new(19, 2)..new(19, 4))]),
                Ok(array![]),
                Ok(array![Token::Not(new(20, 0)..new(20, 1))]),
                Ok(array![]),
                Ok(array![Token::Break(new(20, 2)..new(20, 7))]),
                Ok(array![]),
                Ok(array![Token::Continue(new(21, 0)..new(21, 8))]),
                Ok(array![]),
                Ok(array![]),
                Ok(array![]),
                Ok(array![]),
                Ok(array![]),
            ]
        );
        assert_eq!(tokenize("a"), vec![Ok(array![Token::Identifier((new(0, 0)..new(0, 1), "a".to_string()))])]);
        assert_eq!(tokenize("abc_123"), vec![Ok(array![Token::Identifier((new(0, 0)..new(0, 7), "abc_123".to_string()))])]);
        assert_eq!(tokenize("_abc_123"), vec![Ok(array![Token::Identifier((new(0, 0)..new(0, 8), "_abc_123".to_string()))])]);
        assert_ne!(tokenize("0_abc_123"), vec![Ok(array![Token::Identifier((new(0, 0)..new(0, 9), "0_abc_123".to_string()))])]);

        assert_eq!(tokenize("\"\n\""), vec![Ok(array![Token::StringImmediate((new(0, 0)..new(1, 1), "\n".to_string()))])]);
        assert_eq!(tokenize("\"\\\n\""), vec![Ok(array![Token::StringImmediate((new(0, 0)..new(1, 1), "".to_string()))])]);
        assert_eq!(tokenize("\"\\\r\""), vec![Ok(array![Token::StringImmediate((new(0, 0)..new(0, 4), "".to_string()))])]);
        assert_eq!(tokenize("\"\\\r\n\""), vec![Ok(array![Token::StringImmediate((new(0, 0)..new(1, 1), "".to_string()))])]);
        assert_eq!(tokenize(r##""abcd1234_*`{}-^=~|<>?_,./""##), vec![Ok(array![Token::StringImmediate((new(0, 0)..new(0, 27), "abcd1234_*`{}-^=~|<>?_,./".to_string()))])]);
        assert_eq!(tokenize(r#""\\ \" \n \t \x41 \u{beef}""#), vec![Ok(array![Token::StringImmediate((new(0, 0)..new(0, 27), "\\ \" \n \t \x41 \u{beef}".to_string()))])]);

        assert_eq!(tokenize("0"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 1), Decimal128::from(0)))])]);
        assert_eq!(tokenize("0b0"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 3), Decimal128::from(0)))])]);
        assert_eq!(tokenize("0o0"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 3), Decimal128::from(0)))])]);
        assert_eq!(tokenize("0d0"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 3), Decimal128::from(0)))])]);
        assert_eq!(tokenize("0x0"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 3), Decimal128::from(0)))])]);
        assert_ne!(tokenize("_0"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 2), Decimal128::from(0)))])]);
        assert_eq!(tokenize("12"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 2), Decimal128::from(12)))])]);
        assert_eq!(tokenize("0b10"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 4), Decimal128::from(0b10)))])]);
        assert_eq!(tokenize("0o12"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 4), Decimal128::from(0o12)))])]);
        assert_eq!(tokenize("0d12"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 4), Decimal128::from(12)))])]);
        assert_eq!(tokenize("0x12"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 4), Decimal128::from(0x12)))])]);
        assert_eq!(tokenize("123"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 3), Decimal128::from(123)))])]);
        assert_eq!(tokenize("0b101"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 5), Decimal128::from(0b101)))])]);
        assert_eq!(tokenize("0o123"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 5), Decimal128::from(0o123)))])]);
        assert_eq!(tokenize("0d123"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 5), Decimal128::from(123)))])]);
        assert_eq!(tokenize("0x123"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 5), Decimal128::from(0x123)))])]);

        assert_eq!(tokenize(".0"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 2), Decimal128::from(0)))])]);
        assert_eq!(tokenize(".1e1"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 4), Decimal128::from_str(".1e1").unwrap()))])]);
        assert_eq!(tokenize(".1e+1"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 5), Decimal128::from_str(".1e+1").unwrap()))])]);
        assert_eq!(tokenize(".1e-1"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 5), Decimal128::from_str(".1e-1").unwrap()))])]);
        assert_eq!(tokenize("1E2"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 3), Decimal128::from_str("1E2").unwrap()))])]);
        assert_eq!(tokenize("1E400"), vec![Ok(array![Token::NumberImmediate((new(0, 0)..new(0, 5), Decimal128::from_str("1E400").unwrap()))])]);
    }
}
