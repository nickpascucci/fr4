//! Board Description Language
use crate::model::{Component, Layered, Point, Shape};
use rustyline::{error::ReadlineError, Editor};
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::{Arc, RwLock};
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum Error {
    #[error("Unrecognized word '{0}'")]
    UnrecognizedWord(String),
    #[error("Word is already defined: '{0}'")]
    AlreadyDefined(String),
    #[error("Type error: expected '{0}' but got '{1:?}'")]
    TypeError(String, StackItem),
    #[error("Stack size changed during compilation (was {0}, now {1})")]
    StackSizeChanged(usize, usize),
    #[error("Stack underflow")]
    StackUnderflow,
    #[error("Aborted")]
    Aborted,
    #[error("Invalid state: {0}")]
    InvalidState(String),
    #[error("Input/output error")]
    IoError(String),
    #[error("User exit")]
    UserExit,
    #[error("The word {0} is not implemented yet!")]
    #[allow(dead_code)]
    NotImplemented(String),
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum StackItem {
    Number(u64),
    Boolean(bool),
    String(String),

    Block(Vec<String>),

    Point(Point),
    Shape(Shape),
    Layered(Layered),

    Component(Component),

    Address(Address),
}

impl StackItem {
    fn as_type(&self) -> &'static str {
        match self {
            StackItem::Number(_) => "num",
            StackItem::Boolean(_) => "bool",
            StackItem::String(_) => "str",
            StackItem::Block(_) => "block",
            StackItem::Point(_) => "point",
            StackItem::Shape(_) => "shape",
            StackItem::Layered(_) => "layered",
            StackItem::Component(_) => "component",
            StackItem::Address(_) => "addr",
        }
    }
}

impl std::fmt::Display for StackItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackItem::Number(x) => write!(f, "{}: {}", x, self.as_type()),
            StackItem::Boolean(b) => write!(f, "{}: {}", b, self.as_type()),
            StackItem::String(s) => write!(f, "{}: {}", s, self.as_type()),
            StackItem::Block(b) => write!(f, "[{}]: {}", b.join(" "), self.as_type()),
            StackItem::Point(p) => write!(f, "{}: {}", p, self.as_type()),
            StackItem::Shape(s) => write!(f, "{}: {}", s, self.as_type()),
            StackItem::Layered(l) => write!(f, "{}: {}", l, self.as_type()),
            StackItem::Component(c) => write!(f, "{}: {}", c, self.as_type()),
            StackItem::Address(a) => write!(f, "{}: {}", a, self.as_type()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Address {
    Model,
}

impl std::fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Address::Model => write!(f, "<model>"),
        }
    }
}

// TODO Document each word, its stack effect, and its desired behavior.
#[derive(Clone)]
enum Word {
    // Interpreter
    Abort,
    Words,
    Colon,
    SemiColon,
    Interpret,
    Compile,
    Literal,
    // TODO Comma?
    Comment,
    Include,

    // TODO Control flow

    // Data types
    Quote, // Starts and ends a string

    // Shapes
    Point,
    Rect,
    Invert,
    Layer,

    // Components
    Board,
    Pad,
    Mask,
    Silk,
    Group,

    // Stack operations
    Drop,
    Pick,
    Dup,
    Swap,
    Show,
    Display,

    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,

    // Arrays
    Insert,

    // Memory
    Store,
    Load,
    Model, // Push the model address onto the stack

    Defined(String, Vec<String>),
}

macro_rules! with_stack {
    // Base case: matches an untyped singleton, such as "x".
	($ctxt:ident, [ $item:ident ] do $e:expr) => {
		match $ctxt.data_stack.pop() {
            Some($item) => {
                $e
            },
            None => Err(Error::StackUnderflow),
        }
	};
    // Inductive case: matches a list of untyped values, such as [x, y].
    ($ctxt:ident, [ $head:ident, $($tail:ident),+ ] do $e:expr) => {
        with_stack!($ctxt, [ $head ] do with_stack!($ctxt, [ $($tail),+ ] do $e))
	};

    // Base case: matches a typed singleton, such as "StackItem::Number(x)".
	($ctxt:ident, [ $item:pat ] do $e:expr) => {
		match $ctxt.data_stack.pop() {
            Some($item) => {
                $e
            },
            // TODO Fix error message. This doesn't actually print the type.
            // May need to use a proc macro here...
            Some(other) => Err(Error::TypeError("$item".to_string(), other.clone())),
            None => Err(Error::StackUnderflow),
        }
	};
    // Inductive case: matches a list of typed values, such as
    // [StackItem::Number(x), StackItem::Number(y)].
    ($ctxt:ident, [ $head:pat, $($tail:pat),+ ] do $e:expr) => {
        with_stack!($ctxt, [ $head ] do with_stack!($ctxt, [ $($tail),+ ] do $e))
	};
}

/// Call a binary operator on numeric types.
fn call_binop<F>(ctxt: &mut Context, op: F) -> Result<()>
where
    F: Fn(u64, u64) -> u64,
{
    with_stack!(ctxt, [StackItem::Number(x), StackItem::Number(y)] do {
        ctxt.data_stack.push(StackItem::Number(op(x, y)));
        Ok(())
    })
}

impl Word {
    /// Perform the interpretation semantics of the word.
    ///
    /// For predefined words the behavior varies. For user definitions, the word will execute each
    /// of its subwords in turn.
    fn call(&self, ctxt: &mut Context) -> Result<()> {
        use Word::*;

        let err = match self {
            Abort => Err(Error::Aborted),
            Words => {
                for w in ctxt.dictionary.keys() {
                    println!("{}", w);
                }
                Ok(())
            }
            Colon => match ctxt.state {
                State::Interpreting => {
                    ctxt.state = State::WaitingForCompilationIdentifier;
                    Ok(())
                }
                _ => Err(Error::InvalidState(
                    "Already in compilation state".to_string(),
                )),
            },
            SemiColon => Err(Error::InvalidState(format!(
                "Cannot end definition in state {:?}",
                ctxt.state
            ))),
            Interpret => Err(Error::InvalidState(
                "Error: Already in interpretation state".to_string(),
            )),
            Compile => match &ctxt.state {
                State::CompilationPaused {
                    name,
                    subwords,
                    stack_count,
                } => {
                    ctxt.state = State::Compiling {
                        name: name.clone(),
                        subwords: subwords.clone(),
                        stack_count: *stack_count,
                    };
                    Ok(())
                }
                _ => Err(Error::InvalidState(
                    "Already in compilation state".to_string(),
                )),
            },
            Literal => Err(Error::InvalidState(
                "'Literal' seen outside of definition".to_string(),
            )),
            Comment => {
                ctxt.state = State::WaitingForCommentClose(Box::new(ctxt.state.clone()));
                Ok(())
            }
            Include => with_stack!(ctxt, [ StackItem::String(s) ] do match PathBuf::from_str(&s) {
                Ok(pb) => ctxt.interpret_file(&pb),
                Err(_) => unreachable!(),
            }),

            Quote => {
                ctxt.state =
                    State::WaitingForStringClose(Box::new(ctxt.state.clone()), String::new());
                Ok(())
            }

            Point => with_stack!(ctxt, [StackItem::Number(y), StackItem::Number(x)] do {
                ctxt.data_stack
                    .push(StackItem::Point(crate::model::Point { x, y }));
                Ok(())
            }),
            Rect => with_stack!(ctxt, [StackItem::Point(xy2), StackItem::Point(xy1)] do {
                ctxt.data_stack.push(StackItem::Shape(
                    crate::model::Shape::Rectangle { xy1, xy2 },
                ));
                Ok(())
            }),
            Invert => with_stack!(ctxt, [StackItem::Shape(shape)] do {
                ctxt.data_stack
                    .push(StackItem::Shape(Shape::Inverted(Box::new(shape))));
                Ok(())
            }),
            Layer => with_stack!(ctxt, [StackItem::Number(layer),  StackItem::Shape(shape)] do {
                ctxt.data_stack.push(StackItem::Layered(Layered {
                    // TODO Should this be a u64, for consistency?
                    layer: layer as usize,
                    shape,
                }));
                Ok(())
            }),
            Board => with_stack!(ctxt, [StackItem::Layered(layered)] do {
                ctxt.data_stack
                    .push(StackItem::Component(Component::Board(layered)));
                Ok(())
            }),
            Pad => with_stack!(ctxt, [StackItem::Layered(l)] do {
                ctxt.data_stack
                    .push(StackItem::Component(Component::Pad(l)));
                Ok(())
            }),
            Mask => with_stack!(ctxt, [StackItem::Layered(l)] do {
                ctxt.data_stack
                    .push(StackItem::Component(Component::SolderMask(l)));
                Ok(())
            }),
            Silk => with_stack!(ctxt, [StackItem::Layered(l)] do {
                ctxt.data_stack
                    .push(StackItem::Component(Component::SilkScreen(l)));
                Ok(())
            }),
            Group => {
                ctxt.data_stack
                    .push(StackItem::Component(Component::Group(vec![])));
                Ok(())
            }

            Drop => with_stack!(ctxt, [_x] do Ok(())),
            Dup => with_stack!(ctxt, [x] do {
                ctxt.data_stack.push(x.clone());
                ctxt.data_stack.push(x);
                Ok(())
            }),

            Swap => with_stack!(ctxt, [y, x] do {
                ctxt.data_stack.push(y);
                ctxt.data_stack.push(x);
                Ok(())
            }),

            Pick => with_stack!(ctxt, [StackItem::Number(i)] do {
                if (i as usize) >= ctxt.data_stack.len() {
                    Err(Error::StackUnderflow)
                } else {
                    let idx = ctxt.data_stack.len() - 1 - i as usize;
                    let x = ctxt
                        .data_stack
                        .get(idx)
                        .cloned()
                        .unwrap_or(StackItem::Number(0));
                    ctxt.data_stack.push(x);
                    Ok(())
                }
            }),

            Show => {
                ctxt.print_data_stack();
                Ok(())
            }
            Display => with_stack!(ctxt, [x] do {
                println!("{:?}", x);
                Ok(())
            }),

            Add => call_binop(ctxt, std::ops::Add::add),
            Sub => call_binop(ctxt, std::ops::Sub::sub),
            Mul => call_binop(ctxt, std::ops::Mul::mul),
            Div => call_binop(ctxt, std::ops::Div::div),

            Insert => with_stack!(ctxt, [
                StackItem::Component(Component::Group(mut v)),
                StackItem::Component(cmpt)
            ] do {
                v.push(cmpt);
                ctxt.data_stack
                    .push(StackItem::Component(Component::Group(v)));
                Ok(())
            }),

            Store => with_stack!(ctxt, [StackItem::Address(addr)] do match addr {
                Address::Model => with_stack!(ctxt, [StackItem::Component(cmpt)] do {
                    let mut model_ref = ctxt.model.write().unwrap();
                    *model_ref = cmpt;
                    Ok(())
                })
            }),
            Load => with_stack!(ctxt, [StackItem::Address(addr)] do match addr {
                Address::Model => {
                    let model_ref = ctxt.model.read().unwrap();
                    ctxt.data_stack
                        .push(StackItem::Component(model_ref.clone()));
                    Ok(())
                }
            }),
            Model => {
                ctxt.data_stack.push(StackItem::Address(Address::Model));
                Ok(())
            }

            Defined(_, subwords) => {
                for sw in subwords.iter() {
                    ctxt.interpret_word(sw)?;
                }
                Ok(())
            }
        };
        match err {
            Err(_) => {
                ctxt.data_stack.clear();
                ctxt.state = State::Interpreting;
                err
            }
            x => x,
        }
    }

    /// Perform the compilation-time behavior of a word.
    ///
    /// Most words simply append themselves to the current definition during compilation time. A few
    /// exceptions exist; these are known in the Forth lexicon as "immediate" words. FR4's dialect
    /// does not allow the user to define their own immediate words, as they are hard coded here.
    fn compile(&self, ctxt: &mut Context) -> Result<()> {
        use State::*;
        use Word::*;

        let res = match self {
            Abort => self.call(ctxt),
            Colon => Err(Error::InvalidState(
                "Already in compilation state".to_string(),
            )),
            SemiColon => {
                if let State::Compiling {
                    ref name,
                    ref mut subwords,
                    ref stack_count,
                } = ctxt.state
                {
                    if ctxt.dictionary.contains_key(name) {
                        Err(Error::AlreadyDefined(name.clone()))
                    } else if *stack_count != ctxt.data_stack.len() {
                        Err(Error::StackSizeChanged(*stack_count, ctxt.data_stack.len()))
                    } else {
                        ctxt.dictionary
                            .insert(name.clone(), Word::Defined(name.clone(), subwords.clone()));
                        ctxt.state = State::Interpreting;
                        Ok(())
                    }
                } else {
                    Err(Error::InvalidState(format!(
                        "Saw end of definition in state {:?}",
                        ctxt.state
                    )))
                }
            }
            Interpret => match &ctxt.state {
                Compiling {
                    name,
                    subwords,
                    stack_count,
                } => {
                    ctxt.state = CompilationPaused {
                        name: name.clone(),
                        subwords: subwords.clone(),
                        stack_count: *stack_count,
                    };
                    Ok(())
                }
                _ => Err(Error::InvalidState(format!(
                    "Unexpected state {:?}",
                    ctxt.state
                ))),
            },
            Compile => Err(Error::InvalidState(
                "Already in compilation state".to_string(),
            )),
            Literal => match ctxt.state {
                State::Compiling {
                    ref mut subwords, ..
                } => with_stack!(ctxt, [StackItem::Number(x)] do {
                    subwords.push(x.to_string());
                    Ok(())
                }),
                _ => Err(Error::InvalidState(format!(
                    "Unexpected state {:?}",
                    ctxt.state
                ))),
            },
            Comment => self.call(ctxt),
            Quote => self.call(ctxt),
            _ => {
                if let Compiling {
                    ref mut subwords, ..
                } = ctxt.state
                {
                    subwords.push(self.name().to_string());
                    Ok(())
                } else {
                    Err(Error::InvalidState(
                        "Called compile() while not in compilation state!".to_string(),
                    ))
                }
            }
        };
        match res {
            Err(_) => {
                // TODO This is kind of hacky and should be cleaned up.
                let _ = Abort.call(ctxt);
                res
            }
            Ok(_) => res,
        }
    }

    fn name(&self) -> &str {
        use Word::*;
        // Note: All names should be normalized to lower case.
        match self {
            Abort => "abort",
            Words => "words",
            Colon => ":",
            SemiColon => ";",
            Interpret => "[",
            Compile => "]",
            Literal => "literal",
            Comment => "(",
            Include => "include",

            Quote => "\"",
            Point => "point",
            Rect => "rect",
            Invert => "invert",
            Layer => "layer",
            Board => "board",
            Pad => "pad",
            Mask => "mask",
            Silk => "silk",
            Group => "group",

            Drop => "drop",
            Pick => "pick",
            Dup => "dup",
            Swap => "swap",
            Show => ".s",
            Display => ".",

            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",

            Insert => "insert",

            Store => "!",
            Load => "@",
            Model => "model",

            Defined(n, _) => n,
        }
    }
}

#[derive(Clone, Debug)]
pub enum State {
    Interpreting,
    WaitingForCommentClose(Box<Self>),
    WaitingForCompilationIdentifier,
    WaitingForStringClose(Box<Self>, String),
    Compiling {
        name: String,
        subwords: Vec<String>,
        stack_count: usize,
    },
    CompilationPaused {
        name: String,
        subwords: Vec<String>,
        stack_count: usize,
    },
}

pub struct Context {
    data_stack: Vec<StackItem>,
    dictionary: HashMap<String, Word>,
    state: State,
    model: Arc<RwLock<Component>>,
}

impl Context {
    pub fn new(model: Arc<RwLock<Component>>) -> Self {
        use Word::*;
        let mut ctxt = Self {
            data_stack: Vec::new(),
            dictionary: HashMap::new(),
            state: State::Interpreting,
            model,
        };

        let mut install = |w: Word| ctxt.dictionary.insert(w.name().to_string(), w);

        install(Abort);
        install(Words);
        install(Colon);
        install(SemiColon);
        install(Interpret);
        install(Compile);
        install(Literal);
        install(Comment);
        install(Include);

        install(Quote);
        install(Point);
        install(Rect);
        install(Invert);
        install(Board);
        install(Layer);
        install(Pad);
        install(Mask);
        install(Silk);
        install(Group);

        install(Drop);
        install(Pick);
        install(Dup);
        install(Swap);
        install(Show);
        install(Display);

        install(Add);
        install(Sub);
        install(Mul);
        install(Div);

        install(Insert);

        install(Store);
        install(Load);
        install(Model);

        ctxt
    }

    pub fn interpret(&mut self) -> Result<()> {
        // `()` can be used when no completer is required
        // TODO Add completion support
        let mut rl = Editor::<()>::new();
        if rl.load_history("history.txt").is_err() {
            println!("No previous history.");
        }
        loop {
            let prompt = match self.state {
                State::Interpreting => "i> ",
                State::WaitingForCommentClose(_) => "(> ",
                State::WaitingForCompilationIdentifier => "?> ",
                State::WaitingForStringClose(_, _) => "\"> ",
                State::Compiling { .. } => "c> ",
                State::CompilationPaused { .. } => "[> ",
            };
            let readline = rl.readline(prompt);
            match readline {
                Ok(line) => {
                    rl.add_history_entry(line.as_str());
                    match self.interpret_line(line.as_str()) {
                        Ok(_) => {}
                        Err(Error::UserExit) => {
                            break;
                        }
                        Err(err) => {
                            println!("Error: {}", err);
                        }
                    };
                }
                Err(ReadlineError::Interrupted) => {
                    break;
                }
                Err(ReadlineError::Eof) => {
                    break;
                }
                Err(err) => {
                    println!("Readline Error: {}", err);
                }
            }
        }
        rl.save_history("history.txt").unwrap();
        Ok(())
    }

    pub fn interpret_file(&mut self, filename: &PathBuf) -> Result<()> {
        let file = File::open(filename).map_err(|e| Error::IoError(e.to_string()))?;
        let buf_reader = BufReader::new(file);
        for l in buf_reader.lines() {
            match l {
                Ok(line) => {
                    self.interpret_line(&line)?;
                }
                Err(e) => {
                    return Err(Error::IoError(e.to_string()));
                }
            }
        }
        Ok(())
    }

    fn interpret_line(&mut self, line: &str) -> Result<()> {
        // TODO Don't lower if the context is reading a string.
        for word in line.split_whitespace().map(|s| s.to_lowercase()) {
            // TODO Refactor this to return the next state, rather than an err flag
            let res = match word.as_str() {
                "bye" => return Err(Error::UserExit),
                s => match &self.state {
                    State::Interpreting | State::CompilationPaused { .. } => self.interpret_word(s),
                    State::Compiling { .. } => self.compile_word(s),
                    State::WaitingForCompilationIdentifier => {
                        self.state = State::Compiling {
                            name: s.to_string(),
                            subwords: Vec::new(),
                            stack_count: self.data_stack.len(),
                        };
                        Ok(())
                    }
                    State::WaitingForCommentClose(prev) => {
                        if s.ends_with(")") {
                            self.state = *prev.clone();
                        }
                        Ok(())
                    }
                    State::WaitingForStringClose(prev, s2) => {
                        if s == Word::Quote.name() {
                            self.data_stack.push(StackItem::String(s2.clone()));
                            self.state = *prev.clone();
                        } else {
                            let mut s2 = s2.clone();
                            if !s2.is_empty() {
                                s2.push(' ');
                            }
                            s2.push_str(s);
                            self.state = State::WaitingForStringClose(prev.clone(), s2);
                        }
                        Ok(())
                    }
                },
            };
            if res.is_err() {
                self.state = State::Interpreting;
                return res;
            }
        }
        println!("ok");
        self.print_data_stack();
        Ok(())
    }

    fn compile_word(&mut self, word: &str) -> Result<()> {
        match self.dictionary.get(word) {
            Some(xt) => {
                let xt2 = &xt.clone();
                self.compile(xt2)
            }
            None => match u64::from_str(word) {
                Ok(_) => match self.state {
                    State::Compiling {
                        ref mut subwords, ..
                    } => {
                        subwords.push(word.to_string());
                        Ok(())
                    }
                    _ => Err(Error::InvalidState(format!(
                        "Unexpected state {:?}",
                        self.state
                    ))),
                },
                Err(_) => Err(Error::UnrecognizedWord(word.to_string())),
            },
        }
    }

    fn interpret_word(&mut self, word: &str) -> Result<()> {
        match self.dictionary.get(word) {
            Some(xt) => {
                let xt2 = &xt.clone();
                self.call(xt2)
            }
            None => match u64::from_str(word) {
                Ok(x) => {
                    self.data_stack.push(StackItem::Number(x));
                    Ok(())
                }
                Err(_) => Err(Error::UnrecognizedWord(word.to_string())),
            },
        }
    }

    fn print_data_stack(&self) {
        self.data_stack.iter().for_each(|i| print!("{:?} ", i));
        println!()
    }

    fn call(&mut self, xt: &Word) -> Result<()> {
        xt.call(self)
    }

    fn compile(&mut self, xt: &Word) -> Result<()> {
        xt.compile(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new_context() -> Context {
        let model = Arc::new(RwLock::new(Component::Group(vec![])));
        Context::new(model)
    }

    #[test]
    fn swap() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("1 2 swap")?;
        assert_eq!(Some(StackItem::Number(1)), ctxt.data_stack.pop());
        assert_eq!(Some(StackItem::Number(2)), ctxt.data_stack.pop());
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn dup() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("1 dup")?;
        assert_eq!(Some(StackItem::Number(1)), ctxt.data_stack.pop());
        assert_eq!(Some(StackItem::Number(1)), ctxt.data_stack.pop());
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn drop() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("1 2 drop")?;
        assert_eq!(Some(StackItem::Number(1)), ctxt.data_stack.pop());
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn pick() -> Result<()> {
        {
            let mut ctxt = new_context();
            ctxt.interpret_line("1 2 3 1 pick")?;
            assert_eq!(Some(StackItem::Number(2)), ctxt.data_stack.pop());
            assert_eq!(Some(StackItem::Number(3)), ctxt.data_stack.pop());
            assert_eq!(Some(StackItem::Number(2)), ctxt.data_stack.pop());
            assert_eq!(Some(StackItem::Number(1)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line("1 2 3 0 pick")?;
            assert_eq!(Some(StackItem::Number(3)), ctxt.data_stack.pop());
            assert_eq!(Some(StackItem::Number(3)), ctxt.data_stack.pop());
            assert_eq!(Some(StackItem::Number(2)), ctxt.data_stack.pop());
            assert_eq!(Some(StackItem::Number(1)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            assert_eq!(Err(Error::StackUnderflow), ctxt.interpret_line("0 pick"));
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            assert_eq!(
                Err(Error::StackUnderflow),
                ctxt.interpret_line("1 2 3 3 pick")
            );
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line("1 2 3 2 pick")?;
            assert_eq!(Some(StackItem::Number(1)), ctxt.data_stack.pop());
            assert_eq!(Some(StackItem::Number(3)), ctxt.data_stack.pop());
            assert_eq!(Some(StackItem::Number(2)), ctxt.data_stack.pop());
            assert_eq!(Some(StackItem::Number(1)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        Ok(())
    }

    #[test]
    fn add() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("1 2 +")?;
        assert_eq!(Some(StackItem::Number(3)), ctxt.data_stack.pop());
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn sub() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("1 2 -")?;
        assert_eq!(Some(StackItem::Number(1)), ctxt.data_stack.pop());
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn mul() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("2 2 *")?;
        assert_eq!(Some(StackItem::Number(4)), ctxt.data_stack.pop());
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn div() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("2 6 /")?;
        assert_eq!(Some(StackItem::Number(3)), ctxt.data_stack.pop());
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn point() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("2 6 point")?;
        assert_eq!(
            Some(StackItem::Point(Point { x: 2, y: 6 })),
            ctxt.data_stack.pop()
        );
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn rect() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("2 6 point 4 8 point rect")?;
        assert_eq!(
            Some(StackItem::Shape(Shape::Rectangle {
                xy1: Point { x: 2, y: 6 },
                xy2: Point { x: 4, y: 8 },
            })),
            ctxt.data_stack.pop()
        );
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn pad() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("2 6 point 4 8 point rect 1 layer pad")?;
        assert_eq!(
            Some(StackItem::Component(Component::Pad(Layered {
                layer: 1,
                shape: Shape::Rectangle {
                    xy1: Point { x: 2, y: 6 },
                    xy2: Point { x: 4, y: 8 },
                }
            }))),
            ctxt.data_stack.pop()
        );
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn board() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("2 6 point 4 8 point rect 0 layer board")?;
        assert_eq!(
            Some(StackItem::Component(Component::Board(Layered {
                layer: 0,
                shape: Shape::Rectangle {
                    xy1: Point { x: 2, y: 6 },
                    xy2: Point { x: 4, y: 8 },
                }
            }))),
            ctxt.data_stack.pop()
        );
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn group_insert() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line(
            "0 0 point 10 10 point rect 1 layer pad \
             2 6 point 4  8  point rect 0 layer board \
             group insert insert",
        )?;
        assert_eq!(
            Some(StackItem::Component(Component::Group(vec![
                Component::Board(Layered {
                    layer: 0,
                    shape: Shape::Rectangle {
                        xy1: Point { x: 2, y: 6 },
                        xy2: Point { x: 4, y: 8 },
                    }
                }),
                Component::Pad(Layered {
                    layer: 1,
                    shape: Shape::Rectangle {
                        xy1: Point { x: 0, y: 0 },
                        xy2: Point { x: 10, y: 10 },
                    }
                })
            ]))),
            ctxt.data_stack.pop()
        );
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }
}
