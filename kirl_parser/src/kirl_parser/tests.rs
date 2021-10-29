use parser::Parse;
use tokenizer::Tokenize;

use crate::kirl_parser::{get_parser, AnonymousStructType, Block, Condition, ConstructStruct, Expression, ExpressionItem, Function, If, KirlTopLevelStatement, LetBinding, NamedType, Path, Pattern, Statement, StatementItem, StructName, Symbol, Type};
use crate::kirl_tokenizer::get_tokenizer;
use crate::{CharacterPosition, KirlParser};
use dec::Decimal128;

#[test]
fn test_parse() {
    KirlParser::new();
    let tokenizer = get_tokenizer();
    let parser = get_parser();
    const CODE: &str = r#"
import std::println;

fn my_function(value_a: int)-> #{value_a: int, value_b: string} {
    if value_a > 100 {
        #{value_a, value_b: "big"}
    } else {
        var value_c: int = value_a * 2;
        #{value_a: value_c, value_b: "small" }
    }
}

fn my_function(self: #{value_a: int, value_b: (int | string) }){
    if var value_b: int = self.value_b {
        println(value_b);
    } else {
        println("not integer");
    }
}

fn my_function2(#{value_a: a, value_b }: #{value_a: int, value_b: (int, string,) }){
    var (n, s) = value_b;
    var n = (n, s,).0th;
    (a + n).println();
}

fn get_closure(ratio: int)->([int])->int |->
    fn: i: [int] |-> i.sum() * ratio;

fn to_string::<T>(obj: #{number: int, value: T}) -> #{number: string, value: T} {
    var #{number, value} = obj;
    #{number: number.to_string(), value}
}
println("Hello, World!");
var #{value_a, value_b} = my_function(10);
#{value_a, value_b}.my_function();

a:while true {
    if value_a > 10 {
        break a;
    } else {
        continue a;
    }
}

struct MyStruct::<T> {
    value_a: [T],
    value_b: string
}

std::test::new();
std::test::new(a);
std::test::new(a,);
std::test::new(a, b);
std::test::new(a, b,);

std::test::new();
std::test::new::<>();
std::test::new::<Number>();
std::test::new::<Number,>();
std::test::new::<Number, String>();
std::test::new::<Number, String,>();

a.std::test::new();
a.std::test::new(a);
a.std::test::new(a,);
a.std::test::new(a, b);
a.std::test::new(a, b,);

a.std::test::new();
a.std::test::new::<>();
a.std::test::new::<Number>();
a.std::test::new::<Number,>();
a.std::test::new::<Number, String>();
a.std::test::new::<Number, String,>();

a.();
a.(a);
a.(a,);
a.(a, b);
a.(a, b,);

"#;
    let parse = |code: &str| {
        code.chars()
            .scan(CharacterPosition::zero(), |position, c| {
                let current_position = *position;
                *position = match c {
                    '\n' => position.next_line(),
                    _ => position.next(),
                };
                Some((current_position, c))
            })
            .tokenize_with(&tokenizer, |(_, c)| *c)
            .flat_map(|token| token.unwrap())
            .parse(&parser)
    };
    if let Err(err) = parse(CODE) {
        unreachable!("failed to parse {:?} by {:?}", CODE, err);
    }
    const CODE_1: &str = r#"fn my_function(value_a: int)-> #{value_a: int, value_b: string} {
    if value_a > 100 {
        #{value_a, value_b: "big"}
    } else {
        var value_c: int = value_a * 2;
        #{value_a: value_c, value_b: "small" }
    }
}"#;
    assert_eq!(
        parse(CODE_1).unwrap(),
        Symbol::ValidKirlCode((
            CharacterPosition::new(0, 0)..CharacterPosition::new(7, 1),
            vec![KirlTopLevelStatement::FunctionDefinition((
                CharacterPosition::new(0, 0)..CharacterPosition::new(7, 1),
                Function {
                    position: CharacterPosition::new(0, 0)..CharacterPosition::new(7, 1),
                    name: "my_function".to_owned(),
                    generics_arguments: vec![],
                    arguments: vec![(
                        Pattern::Variable("value_a".to_owned()),
                        Type::NamedType(NamedType {
                            position: CharacterPosition::new(0, 24)..CharacterPosition::new(0, 27),
                            path: vec!["int".to_owned()],
                            generics_arguments: vec![],
                        }),
                    ),],
                    return_type: Type::AnonymousStruct(AnonymousStructType {
                        position: CharacterPosition::new(0, 31)..CharacterPosition::new(0, 63),
                        members: vec![
                            (
                                "value_a".to_owned(),
                                Type::NamedType(NamedType {
                                    position: CharacterPosition::new(0, 42)..CharacterPosition::new(0, 45),
                                    path: vec!["int".to_owned()],
                                    generics_arguments: vec![],
                                }),
                            ),
                            (
                                "value_b".to_owned(),
                                Type::NamedType(NamedType {
                                    position: CharacterPosition::new(0, 56)..CharacterPosition::new(0, 62),
                                    path: vec!["string".to_owned()],
                                    generics_arguments: vec![],
                                }),
                            ),
                        ],
                    },),
                    expression: Expression {
                        position: CharacterPosition::new(0, 64)..CharacterPosition::new(7, 1),
                        expression: ExpressionItem::Block(Block {
                            position: CharacterPosition::new(0, 64)..CharacterPosition::new(7, 1),
                            statements: vec![],
                            last_expression: Some(
                                Expression {
                                    position: CharacterPosition::new(1, 4)..CharacterPosition::new(6, 5),
                                    expression: ExpressionItem::If(If {
                                        position: CharacterPosition::new(1, 4)..CharacterPosition::new(6, 5),
                                        condition: Condition::BoolExpression(Expression {
                                            position: CharacterPosition::new(1, 7)..CharacterPosition::new(1, 20),
                                            expression: ExpressionItem::GreaterThan(
                                                Expression {
                                                    position: CharacterPosition::new(1, 7)..CharacterPosition::new(1, 14),
                                                    expression: ExpressionItem::AccessVariable(
                                                        Path {
                                                            position: CharacterPosition::new(1, 7)..CharacterPosition::new(1, 14),
                                                            path: vec!["value_a".to_owned()],
                                                        },
                                                        Vec::new()
                                                    ),
                                                }
                                                .into(),
                                                Expression {
                                                    position: CharacterPosition::new(1, 17)..CharacterPosition::new(1, 20),
                                                    expression: ExpressionItem::NumberImmediate(Decimal128::from(100)),
                                                }
                                                .into(),
                                            ),
                                        })
                                        .into(),
                                        then: Expression {
                                            position: CharacterPosition::new(1, 21)..CharacterPosition::new(3, 5),
                                            expression: ExpressionItem::Block(Block {
                                                position: CharacterPosition::new(1, 21)..CharacterPosition::new(3, 5),
                                                statements: vec![],
                                                last_expression: Some(
                                                    Expression {
                                                        position: CharacterPosition::new(2, 8)..CharacterPosition::new(2, 34),
                                                        expression: ExpressionItem::ConstructStruct(ConstructStruct {
                                                            name: StructName::Anonymous,
                                                            items: vec![
                                                                (
                                                                    "value_a".to_owned(),
                                                                    Expression {
                                                                        position: CharacterPosition::new(2, 10)..CharacterPosition::new(2, 17),
                                                                        expression: ExpressionItem::AccessVariable(
                                                                            Path {
                                                                                position: CharacterPosition::new(2, 10)..CharacterPosition::new(2, 17),
                                                                                path: vec!["value_a".to_owned()],
                                                                            },
                                                                            Vec::new()
                                                                        ),
                                                                    },
                                                                ),
                                                                (
                                                                    "value_b".to_owned(),
                                                                    Expression {
                                                                        position: CharacterPosition::new(2, 28)..CharacterPosition::new(2, 33),
                                                                        expression: ExpressionItem::StringImmediate("big".to_owned()),
                                                                    },
                                                                ),
                                                            ],
                                                        }),
                                                    }
                                                    .into()
                                                ),
                                            }),
                                        }
                                        .into(),
                                        other: Some(
                                            Expression {
                                                position: CharacterPosition::new(3, 11)..CharacterPosition::new(6, 5),
                                                expression: ExpressionItem::Block(Block {
                                                    position: CharacterPosition::new(3, 11)..CharacterPosition::new(6, 5),
                                                    statements: vec![Statement {
                                                        position: CharacterPosition::new(4, 8)..CharacterPosition::new(4, 39),
                                                        statement: StatementItem::LetBinding(LetBinding {
                                                            position: CharacterPosition::new(4, 8)..CharacterPosition::new(4, 38),
                                                            pattern: Pattern::Variable("value_c".to_owned()),
                                                            type_hint: Some(Type::NamedType(NamedType {
                                                                position: CharacterPosition::new(4, 21)..CharacterPosition::new(4, 24),
                                                                path: vec!["int".to_owned()],
                                                                generics_arguments: vec![],
                                                            })),
                                                            expression: Box::new(Expression {
                                                                position: CharacterPosition::new(4, 27)..CharacterPosition::new(4, 38),
                                                                expression: ExpressionItem::Mul(
                                                                    Expression {
                                                                        position: CharacterPosition::new(4, 27)..CharacterPosition::new(4, 34),
                                                                        expression: ExpressionItem::AccessVariable(
                                                                            Path {
                                                                                position: CharacterPosition::new(4, 27)..CharacterPosition::new(4, 34),
                                                                                path: vec!["value_a".to_owned()],
                                                                            },
                                                                            Vec::new()
                                                                        ),
                                                                    }
                                                                    .into(),
                                                                    Expression {
                                                                        position: CharacterPosition::new(4, 37)..CharacterPosition::new(4, 38),
                                                                        expression: ExpressionItem::NumberImmediate(Decimal128::from(2)),
                                                                    }
                                                                    .into(),
                                                                ),
                                                            }),
                                                        }),
                                                    },],
                                                    last_expression: Some(
                                                        Expression {
                                                            position: CharacterPosition::new(5, 8)..CharacterPosition::new(5, 46),
                                                            expression: ExpressionItem::ConstructStruct(ConstructStruct {
                                                                name: StructName::Anonymous,
                                                                items: vec![
                                                                    (
                                                                        "value_a".to_owned(),
                                                                        Expression {
                                                                            position: CharacterPosition::new(5, 19)..CharacterPosition::new(5, 26),
                                                                            expression: ExpressionItem::AccessVariable(
                                                                                Path {
                                                                                    position: CharacterPosition::new(5, 19)..CharacterPosition::new(5, 26),
                                                                                    path: vec!["value_c".to_owned()],
                                                                                },
                                                                                Vec::new()
                                                                            ),
                                                                        },
                                                                    ),
                                                                    (
                                                                        "value_b".to_owned(),
                                                                        Expression {
                                                                            position: CharacterPosition::new(5, 37)..CharacterPosition::new(5, 44),
                                                                            expression: ExpressionItem::StringImmediate("small".to_owned()),
                                                                        },
                                                                    ),
                                                                ],
                                                            }),
                                                        }
                                                        .into(),
                                                    ),
                                                }),
                                            }
                                            .into()
                                        ),
                                    }),
                                }
                                .into()
                            ),
                        }),
                    },
                },
            )),],
        ))
    );
}
