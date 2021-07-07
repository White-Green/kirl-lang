use std::io::Read;

use regex::Regex;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    let input = Regex::new("[ \t]").unwrap().replace_all(&input, "");
    let input = Regex::new("\r?\n").unwrap().split(&input).filter(|s| !s.is_empty()).scan(String::new(), |name, current| {
        if current.starts_with("|") {
            Some((name.clone(), current[1..].to_string()))
        } else {
            let mut split = current.split("::=");
            *name = split.next().unwrap().to_string();
            Some((name.clone(), split.next().unwrap().to_string()))
        }
    }).collect::<Vec<_>>();
    let regex = Regex::new("^(<.+?>|\\[.+?]|\".+?\")").unwrap();
    let bnf = input.into_iter()
        .map(|(k, v)| {
            let mut i = 0;
            let mut symbols = Vec::new();
            while let Some(m) = regex.find(&v[i..]) {
                i += m.end();
                symbols.push(to_symbol(m.as_str()));
            }
            assert_eq!(v[i..].len(), 0, "{}", &v[i..]);
            (k[1..k.len() - 1].to_string(), symbols)
        }).collect::<Vec<_>>();
    let mut symbols = bnf.iter().map(|(k, _)| format!("{}(()),", k)).collect::<Vec<_>>();
    symbols.dedup();
    println!("{}", symbols.join("\n"));
    println!("{}", bnf.into_iter().map(|(k, v)|
        format!(".rule(Rule::new(Symbol::{}(Default::default()), &[{}],\n\
                        |list| if let [{}] = list {{\n\
                            Symbol::{0}(todo!())\n\
                        }} else {{ unreachable!() }}))\n",
                k,
                v.iter().map(|s| match s {
                    Symbol::Terminal(t) => format!("Terminal(Token::{}(Default::default()))", t),
                    Symbol::NonTerminal(t) => format!("NonTerminal(Symbol::{}(Default::default()))", t),
                }).reduce(|a, b| format!("{}, {}", a, b)).unwrap_or_default(),
                v.iter().map(|s| match s {
                    Symbol::Terminal(t) => format!("Terminal(Token::{}(_))", t),
                    Symbol::NonTerminal(t) => format!("NonTerminal(Symbol::{}(_))", t),
                }).reduce(|a, b| format!("{}, {}", a, b)).unwrap_or_default(),
        )
    ).reduce(|mut a, b| {
        a.push_str(&b);
        a
    }).unwrap());
}

#[derive(Debug)]
enum Symbol {
    Terminal(String),
    NonTerminal(String),
}

fn to_symbol(s: &str) -> Symbol {
    match s.chars().next().unwrap() {
        '<' => Symbol::NonTerminal(s[1..s.len() - 1].to_string()),
        '[' => Symbol::Terminal(s[1..s.len() - 1].to_string()),
        '\"' => {
            match &s[1..s.len() - 1] {
                "import" => Symbol::Terminal("Import".to_string()),
                "fn" => Symbol::Terminal("Fn".to_string()),
                "struct" => Symbol::Terminal("Struct".to_string()),
                "let" => Symbol::Terminal("Let".to_string()),
                "if" => Symbol::Terminal("If".to_string()),
                "else" => Symbol::Terminal("Else".to_string()),
                "match" => Symbol::Terminal("Match".to_string()),
                "for" => Symbol::Terminal("For".to_string()),
                "in" => Symbol::Terminal("In".to_string()),
                "while" => Symbol::Terminal("While".to_string()),
                "return" => Symbol::Terminal("Return".to_string()),
                "break" => Symbol::Terminal("Break".to_string()),
                "continue" => Symbol::Terminal("Continue".to_string()),
                "!" => Symbol::Terminal("Not".to_string()),
                "." => Symbol::Terminal("Dot".to_string()),
                "," => Symbol::Terminal("Comma".to_string()),
                "#" => Symbol::Terminal("Sharp".to_string()),
                "::" => Symbol::Terminal("DoubleColon".to_string()),
                ":" => Symbol::Terminal("Colon".to_string()),
                ";" => Symbol::Terminal("Semicolon".to_string()),
                ">" => Symbol::Terminal("GreaterThan".to_string()),
                "<" => Symbol::Terminal("LessThan".to_string()),
                ">=" => Symbol::Terminal("GreaterOrEqual".to_string()),
                "<=" => Symbol::Terminal("LessOrEqual".to_string()),
                "=" => Symbol::Terminal("Assign".to_string()),
                "==" => Symbol::Terminal("Equals".to_string()),
                "!=" => Symbol::Terminal("NotEquals".to_string()),
                "+" => Symbol::Terminal("Add".to_string()),
                "-" => Symbol::Terminal("Sub".to_string()),
                "*" => Symbol::Terminal("Mul".to_string()),
                "/" => Symbol::Terminal("Div".to_string()),
                "%" => Symbol::Terminal("Rem".to_string()),
                "&" => Symbol::Terminal("And".to_string()),
                "|" => Symbol::Terminal("Or".to_string()),
                "^" => Symbol::Terminal("Xor".to_string()),
                "(" => Symbol::Terminal("RoundBracketOpen".to_string()),
                ")" => Symbol::Terminal("RoundBracketClose".to_string()),
                "[" => Symbol::Terminal("SquareBracketOpen".to_string()),
                "]" => Symbol::Terminal("SquareBracketClose".to_string()),
                "{" => Symbol::Terminal("WaveBracketOpen".to_string()),
                "}" => Symbol::Terminal("WaveBracketClose".to_string()),
                "->" => Symbol::Terminal("FunctionArrow".to_string()),
                "=>" => Symbol::Terminal("MatchArrow".to_string()),
                c => unreachable!("{:?}", c)
            }
        }
        _ => unreachable!()
    }
}
