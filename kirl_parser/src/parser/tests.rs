use parser::{LR1Parser, Parse};
use tokenizer::Tokenize;

use crate::CharacterPosition;
use crate::parser::get_syntax;
use crate::tokenizer::get_tokenizer;

#[test]
fn test_parse() {
    let tokenizer = get_tokenizer();
    let (parser, warning) = LR1Parser::new(get_syntax());
    assert_eq!(warning.len(), 0, "{}", warning.into_iter().map(|w| format!("{:?}", w)).collect::<Vec<_>>().join("\n"));
    const CODE: &str = r#"
import std::println;

fn my_function(value_a: int)-> #{value_a: int, value_b: string} {
    if value_a > 100 {
        #{value_a, value_b: "big"}
    } else {
        let value_c: int = value_a * 2;
        #{value_a: value_c, value_b: "small" }
    }
}

fn my_function(self: #{value_a: int, value_b: (int | string) }){
    if let value_b: int = self.value_b {
        println(value_b);
    } else {
        println("not integer");
    }
}

fn get_closure(ratio: int)->([int])->int |->
    (i: [int])->{
        i.sum() * ratio
    };

fn to_string::<T>(obj: #{number: int, value: T}) -> #{number: string, value: T} {
    let #{number, value} = obj;
    #{number: number.to_string(), value}
}
println("Hello, World!");
let #{value_a, value_b} = my_function(10);
#{value_a, value_b}.my_function();

struct MyStruct::<T> {
    value_a: [T],
    value_b: string,
}

"#;
    println!("{:?}", CODE.chars()
        .scan(CharacterPosition::zero(), |position, c| {
            let current_position = *position;
            *position = match c {
                '\n' => position.next_line(),
                _ => position.next()
            };
            Some((current_position, c))
        })
        .tokenize_with(&tokenizer, |(_, c)| *c).collect::<Vec<_>>());
    let result = CODE.chars()
        .scan(CharacterPosition::zero(), |position, c| {
            let current_position = *position;
            *position = match c {
                '\n' => position.next_line(),
                _ => position.next()
            };
            Some((current_position, c))
        })
        .tokenize_with(&tokenizer, |(_, c)| *c)
        .filter_map(|token| token.unwrap())
        .inspect(|token| println!("{:?}", token))
        .parse(&parser);
    println!("{:?}", result);
}
