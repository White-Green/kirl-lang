use std::error::Error;
use std::fmt::{Display, Formatter};
use std::num::ParseFloatError;
use std::ops::Range;
use std::str::FromStr;

use parser::enum_index;
use parser::enum_index_derive::*;
use tokenizer::*;

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
    /// 変数とかの識別子
    Identifier((Range<CharacterPosition>, String)),
    /// 文字列即値
    StringImmediate((Range<CharacterPosition>, String)),
    /// 整数即値
    IntegerImmediate((Range<CharacterPosition>, i64)),
    /// 浮動小数点数即値
    FloatImmediate((Range<CharacterPosition>, f64)),
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
    /// =>
    MatchArrow(Range<CharacterPosition>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenizeError {
    IntegerParseError { raw: String, position: Range<CharacterPosition> },
    FloatParseError { raw: String, position: Range<CharacterPosition>, error: ParseFloatError },
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
            TokenizeError::FloatParseError { error, .. } => Some(error),
            _ => None,
        }
    }
}

pub fn get_tokenizer() -> DFATokenizer<Result<Option<Token>, TokenizeError>, (CharacterPosition, char)> {
    let (tokenizer, _warning) = DFATokenizer::<Result<Option<Token>, TokenizeError>, (CharacterPosition, char)>::builder()
        .pattern("//.*", |_, _| Ok(None))
        .pattern("/\\*(\\*[^/]|[^\\*])*\\*/", |_, _| Ok(None))
        .pattern("\\s", |_, _| Ok(None))
        .pattern("import", |_, v| Ok(Some(Token::Import(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("fn", |_, v| Ok(Some(Token::Fn(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("struct", |_, v| Ok(Some(Token::Struct(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("let", |_, v| Ok(Some(Token::Let(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("var", |_, v| Ok(Some(Token::Var(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("if", |_, v| Ok(Some(Token::If(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("else", |_, v| Ok(Some(Token::Else(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("match", |_, v| Ok(Some(Token::Match(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("for", |_, v| Ok(Some(Token::For(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("in", |_, v| Ok(Some(Token::In(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("while", |_, v| Ok(Some(Token::While(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("return", |_, v| Ok(Some(Token::Return(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("break", |_, v| Ok(Some(Token::Break(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("continue", |_, v| Ok(Some(Token::Continue(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("[a-zA-Z_][a-zA-Z0-9_]*", |s, v| Ok(Some(Token::Identifier((v.first().unwrap().0..v.last().unwrap().0.next(), s.to_string())))))
        .pattern("\"(\\\\(n|t|x[0-9a-fA-F]{2}|u\\{[0-9a-fA-F]{1,6}\\}|\\\\|\")|[^\\\\\"\n])*\"", |s, v| {
            parse_string_literal(s)
                .ok_or_else(|| TokenizeError::StringParseError {
                    raw: s.to_string(),
                    position: v.first().unwrap().0..v.last().unwrap().0.next(),
                })
                .map(|s| Some(Token::StringImmediate((v.first().unwrap().0..v.last().unwrap().0.next(), s))))
        })
        .pattern("0b[01_]*[01]|0o[0-7_]*[0-7]|0d[0-9_]*[0-9]|0x[0-9a-fA-F_]*[0-9a-fA-F]|[0-9]|[0-9][0-9_]*[0-9]", |s, v| {
            parse_integer(s)
                .ok_or_else(|| TokenizeError::IntegerParseError {
                    raw: s.to_string(),
                    position: v.first().unwrap().0..v.last().unwrap().0.next(),
                })
                .map(|i| Some(Token::IntegerImmediate((v.first().unwrap().0..v.last().unwrap().0.next(), i))))
        })
        .pattern("([0-9][0-9_]*[0-9]|[0-9]|([0-9][0-9_]*)?\\.[0-9_]*[0-9])([eE][\\+\\-]?[0-9_]*[0-9])?", |s, v| {
            f64::from_str(&s.replace("_", "")).map(|f| Some(Token::FloatImmediate((v.first().unwrap().0..v.last().unwrap().0.next(), f)))).map_err(|e| TokenizeError::FloatParseError {
                raw: s.to_string(),
                position: v.first().unwrap().0..v.last().unwrap().0.next(),
                error: e,
            })
        })
        .pattern("!", |_, v| Ok(Some(Token::Not(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("\\.", |_, v| Ok(Some(Token::Dot(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern(",", |_, v| Ok(Some(Token::Comma(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("#", |_, v| Ok(Some(Token::Sharp(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("::", |_, v| Ok(Some(Token::DoubleColon(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern(":", |_, v| Ok(Some(Token::Colon(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern(";", |_, v| Ok(Some(Token::Semicolon(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern(">", |_, v| Ok(Some(Token::GreaterThan(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("<", |_, v| Ok(Some(Token::LessThan(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern(">=", |_, v| Ok(Some(Token::GreaterOrEqual(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("<=", |_, v| Ok(Some(Token::LessOrEqual(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("=", |_, v| Ok(Some(Token::Assign(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("==", |_, v| Ok(Some(Token::Equals(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("!=", |_, v| Ok(Some(Token::NotEquals(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("\\+", |_, v| Ok(Some(Token::Add(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("\\-", |_, v| Ok(Some(Token::Sub(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("\\*", |_, v| Ok(Some(Token::Mul(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("/", |_, v| Ok(Some(Token::Div(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("%", |_, v| Ok(Some(Token::Rem(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("&", |_, v| Ok(Some(Token::And(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("\\|", |_, v| Ok(Some(Token::Or(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("\\^", |_, v| Ok(Some(Token::Xor(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("\\(", |_, v| Ok(Some(Token::RoundBracketOpen(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("\\)", |_, v| Ok(Some(Token::RoundBracketClose(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("\\[", |_, v| Ok(Some(Token::SquareBracketOpen(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("\\]", |_, v| Ok(Some(Token::SquareBracketClose(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("\\{", |_, v| Ok(Some(Token::WaveBracketOpen(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("\\}", |_, v| Ok(Some(Token::WaveBracketClose(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("->", |_, v| Ok(Some(Token::FunctionArrow(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern("=>", |_, v| Ok(Some(Token::MatchArrow(v.first().unwrap().0..v.last().unwrap().0.next()))))
        .pattern(".|\n", |_, v| Err(TokenizeError::UnknownCharacter { character: v.first().unwrap().1, position: v.first().unwrap().0 }))
        .build()
        .unwrap();
    tokenizer
}

fn parse_integer(s: &str) -> Option<i64> {
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
    s.chars().fold(Some(0), |acc, c| acc?.checked_mul(radix)?.checked_add(c.to_digit(radix as u32).expect("正規表現でチェックしてるのであんりーちゃぶる") as i64))
}

fn parse_string_literal(s: &str) -> Option<String> {
    let mut iter = s[1..s.len() - 1].chars();
    let mut result = String::with_capacity(s.len());
    while let Some(c) = iter.next() {
        if c != '\\' {
            result.push(c);
            continue;
        }
        match iter.next() {
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

    use tokenizer::Tokenize;

    use crate::kirl_tokenizer::{get_tokenizer, CharacterPosition, Token, TokenizeError};

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
( )
[ ]
{ }
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
                Ok(Some(Token::Import(new(0, 0)..new(0, 6)))),
                Ok(None),
                Ok(Some(Token::Fn(new(0, 7)..new(0, 9)))),
                Ok(None),
                Ok(Some(Token::Struct(new(1, 0)..new(1, 6)))),
                Ok(None),
                Ok(Some(Token::Let(new(1, 7)..new(1, 10)))),
                Ok(None),
                Ok(Some(Token::If(new(2, 0)..new(2, 2)))),
                Ok(None),
                Ok(Some(Token::Else(new(2, 3)..new(2, 7)))),
                Ok(None),
                Ok(Some(Token::Match(new(3, 0)..new(3, 5)))),
                Ok(None),
                Ok(Some(Token::For(new(3, 6)..new(3, 9)))),
                Ok(None),
                Ok(Some(Token::In(new(4, 0)..new(4, 2)))),
                Ok(None),
                Ok(Some(Token::While(new(4, 3)..new(4, 8)))),
                Ok(None),
                Ok(Some(Token::Return(new(5, 0)..new(5, 6)))),
                Ok(None),
                Ok(Some(Token::DoubleColon(new(5, 7)..new(5, 9)))),
                Ok(None),
                Ok(Some(Token::Colon(new(6, 0)..new(6, 1)))),
                Ok(None),
                Ok(Some(Token::Semicolon(new(6, 2)..new(6, 3)))),
                Ok(None),
                Ok(Some(Token::GreaterThan(new(7, 0)..new(7, 1)))),
                Ok(None),
                Ok(Some(Token::LessThan(new(7, 2)..new(7, 3)))),
                Ok(None),
                Ok(Some(Token::GreaterOrEqual(new(8, 0)..new(8, 2)))),
                Ok(None),
                Ok(Some(Token::LessOrEqual(new(8, 3)..new(8, 5)))),
                Ok(None),
                Ok(Some(Token::Assign(new(9, 0)..new(9, 1)))),
                Ok(None),
                Ok(Some(Token::Equals(new(9, 2)..new(9, 4)))),
                Ok(None),
                Ok(Some(Token::Add(new(10, 0)..new(10, 1)))),
                Ok(None),
                Ok(Some(Token::Sub(new(10, 2)..new(10, 3)))),
                Ok(None),
                Ok(Some(Token::Mul(new(11, 0)..new(11, 1)))),
                Ok(None),
                Ok(Some(Token::Div(new(11, 2)..new(11, 3)))),
                Ok(None),
                Ok(Some(Token::Rem(new(12, 0)..new(12, 1)))),
                Ok(None),
                Ok(Some(Token::And(new(12, 2)..new(12, 3)))),
                Ok(None),
                Ok(Some(Token::Or(new(13, 0)..new(13, 1)))),
                Ok(None),
                Ok(Some(Token::Xor(new(13, 2)..new(13, 3)))),
                Ok(None),
                Ok(Some(Token::RoundBracketOpen(new(14, 0)..new(14, 1)))),
                Ok(None),
                Ok(Some(Token::RoundBracketClose(new(14, 2)..new(14, 3)))),
                Ok(None),
                Ok(Some(Token::SquareBracketOpen(new(15, 0)..new(15, 1)))),
                Ok(None),
                Ok(Some(Token::SquareBracketClose(new(15, 2)..new(15, 3)))),
                Ok(None),
                Ok(Some(Token::WaveBracketOpen(new(16, 0)..new(16, 1)))),
                Ok(None),
                Ok(Some(Token::WaveBracketClose(new(16, 2)..new(16, 3)))),
                Ok(None),
                Ok(Some(Token::FunctionArrow(new(17, 0)..new(17, 2)))),
                Ok(None),
                Ok(Some(Token::MatchArrow(new(17, 3)..new(17, 5)))),
                Ok(None),
                Ok(Some(Token::Dot(new(18, 0)..new(18, 1)))),
                Ok(None),
                Ok(Some(Token::Sharp(new(18, 2)..new(18, 3)))),
                Ok(None),
                Ok(Some(Token::Comma(new(19, 0)..new(19, 1)))),
                Ok(None),
                Ok(Some(Token::NotEquals(new(19, 2)..new(19, 4)))),
                Ok(None),
                Ok(Some(Token::Not(new(20, 0)..new(20, 1)))),
                Ok(None),
                Ok(Some(Token::Break(new(20, 2)..new(20, 7)))),
                Ok(None),
                Ok(Some(Token::Continue(new(21, 0)..new(21, 8)))),
                Ok(None),
                Ok(None),
                Ok(None),
                Ok(None),
                Ok(None)
            ]
        );
        assert_eq!(tokenize("a"), vec![Ok(Some(Token::Identifier((new(0, 0)..new(0, 1), "a".to_string()))))]);
        assert_eq!(tokenize("abc_123"), vec![Ok(Some(Token::Identifier((new(0, 0)..new(0, 7), "abc_123".to_string()))))]);
        assert_eq!(tokenize("_abc_123"), vec![Ok(Some(Token::Identifier((new(0, 0)..new(0, 8), "_abc_123".to_string()))))]);
        assert_ne!(tokenize("0_abc_123"), vec![Ok(Some(Token::Identifier((new(0, 0)..new(0, 9), "0_abc_123".to_string()))))]);

        assert_eq!(tokenize(r##""abcd1234_*`{}-^=~|<>?_,./""##), vec![Ok(Some(Token::StringImmediate((new(0, 0)..new(0, 27), "abcd1234_*`{}-^=~|<>?_,./".to_string()))))]);
        assert_eq!(tokenize(r#""\\ \" \n \t \x41 \u{beef}""#), vec![Ok(Some(Token::StringImmediate((new(0, 0)..new(0, 27), "\\ \" \n \t \x41 \u{beef}".to_string()))))]);

        assert_eq!(tokenize("0"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 1), 0))))]);
        assert_eq!(tokenize("0b0"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 3), 0))))]);
        assert_eq!(tokenize("0o0"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 3), 0))))]);
        assert_eq!(tokenize("0d0"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 3), 0))))]);
        assert_eq!(tokenize("0x0"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 3), 0))))]);
        assert_ne!(tokenize("_0"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 2), 0))))]);
        assert_eq!(tokenize("12"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 2), 12))))]);
        assert_eq!(tokenize("0b10"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 4), 0b10))))]);
        assert_eq!(tokenize("0o12"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 4), 0o12))))]);
        assert_eq!(tokenize("0d12"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 4), 12))))]);
        assert_eq!(tokenize("0x12"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 4), 0x12))))]);
        assert_eq!(tokenize("123"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 3), 123))))]);
        assert_eq!(tokenize("0b101"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 5), 0b101))))]);
        assert_eq!(tokenize("0o123"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 5), 0o123))))]);
        assert_eq!(tokenize("0d123"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 5), 123))))]);
        assert_eq!(tokenize("0x123"), vec![Ok(Some(Token::IntegerImmediate((new(0, 0)..new(0, 5), 0x123))))]);
        assert_eq!(tokenize("9223372036854775808"), vec![Err(TokenizeError::IntegerParseError { raw: "9223372036854775808".to_string(), position: new(0, 0)..new(0, 19) })]);
        assert_eq!(
            tokenize("0b1000_0000__0000_0000___0000_0000__0000_0000____0000_0000__0000_0000___0000_0000__0000_0000"),
            vec![Err(TokenizeError::IntegerParseError {
                raw: "0b1000_0000__0000_0000___0000_0000__0000_0000____0000_0000__0000_0000___0000_0000__0000_0000".to_string(),
                position: new(0, 0)..new(0, 92)
            })]
        );
        assert_eq!(
            tokenize("0o1000000000000000000000"),
            vec![Err(TokenizeError::IntegerParseError {
                raw: "0o1000000000000000000000".to_string(),
                position: new(0, 0)..new(0, 24),
            })]
        );
        assert_eq!(
            tokenize("0d9223372036854775808"),
            vec![Err(TokenizeError::IntegerParseError {
                raw: "0d9223372036854775808".to_string(),
                position: new(0, 0)..new(0, 21),
            })]
        );
        assert_eq!(
            tokenize("0x8000_0000_0000_0000"),
            vec![Err(TokenizeError::IntegerParseError {
                raw: "0x8000_0000_0000_0000".to_string(),
                position: new(0, 0)..new(0, 21),
            })]
        );

        assert_eq!(tokenize(".0"), vec![Ok(Some(Token::FloatImmediate((new(0, 0)..new(0, 2), 0.))))]);
        assert_eq!(tokenize(".1e1"), vec![Ok(Some(Token::FloatImmediate((new(0, 0)..new(0, 4), f64::from_str(".1e1").unwrap()))))]);
        assert_eq!(tokenize(".1e+1"), vec![Ok(Some(Token::FloatImmediate((new(0, 0)..new(0, 5), f64::from_str(".1e+1").unwrap()))))]);
        assert_eq!(tokenize(".1e-1"), vec![Ok(Some(Token::FloatImmediate((new(0, 0)..new(0, 5), f64::from_str(".1e-1").unwrap()))))]);
        assert_eq!(tokenize("1E2"), vec![Ok(Some(Token::FloatImmediate((new(0, 0)..new(0, 3), f64::from_str("1E2").unwrap()))))]);
        assert_eq!(tokenize("1E400"), vec![Ok(Some(Token::FloatImmediate((new(0, 0)..new(0, 5), f64::from_str("1E400").unwrap()))))]);
    }
}
