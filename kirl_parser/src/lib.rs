mod parser;
mod tokenizer;

#[derive(Clone, Default, Copy, Debug, PartialEq)]
pub struct CharacterPosition {
    line: usize,
    column: usize,
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
