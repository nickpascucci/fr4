//! Board Description Language
use crate::model::{Component, Layered, Point, Shape};
use log::debug;
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
    #[error("Arithmetic underflow: {0} - {1} < 0")]
    ArithmeticUnderflow(u64, u64),
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
#[derive(Clone, Debug)]
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

    BlockOpen,
    BlockClose,
    Conditional,
    Loop,

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
    Rot,
    Show,
    Display,

    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,

    // Comparisons
    Eq,
    GreaterThan,

    // Boolean Logic
    True,
    And,
    Or,
    Not,

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
            Some(other) => Err(Error::TypeError(stringify!($item).to_string(), other.clone())),
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

            BlockOpen => {
                ctxt.state = State::WaitingForBlockClose(Box::new(ctxt.state.clone()), Vec::new());
                Ok(())
            }
            BlockClose => match &ctxt.state {
                State::WaitingForBlockClose(prev, subwords) => {
                    ctxt.data_stack.push(StackItem::Block(subwords.clone()));
                    ctxt.state = *prev.clone();
                    Ok(())
                }
                _ => Err(Error::InvalidState(format!(
                    "'{}' seen without matching '{}'",
                    BlockClose.name(),
                    BlockOpen.name()
                ))),
            },
            Conditional => {
                with_stack!(ctxt, [
                    StackItem::Block(right),
                    StackItem::Block(left),
                    StackItem::Boolean(test)
                ] do {
                    let block = if test {
                        left
                    } else {
                        right
                    };
                    for w in block { ctxt.interpret_line(&w)?; }
                    Ok(())
                })
            }
            Loop => {
                with_stack!(ctxt, [
                    StackItem::Block(test),
                    StackItem::Block(body)
                ] do {
                    loop {
                        for w in &test { ctxt.interpret_line(w)?; }

                        if with_stack!(ctxt, [StackItem::Boolean(cont)] do Ok(!cont))? {
                            break;
                        }

                        for w in &body { ctxt.interpret_line(w)?; }
                    }

                    Ok(())
                })
            }

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
            Rot => with_stack!(ctxt, [z, y, x] do {
                ctxt.data_stack.push(y);
                ctxt.data_stack.push(z);
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
                println!("{}", x);
                Ok(())
            }),

            Add => call_binop(ctxt, std::ops::Add::add),
            Sub => with_stack!(ctxt, [StackItem::Number(r), StackItem::Number(l)] do {
                if l >= r {
                    ctxt.data_stack.push(StackItem::Number(l - r));
                    Ok(())
                } else {
                    Err(Error::ArithmeticUnderflow(l, r))
                }
            }),
            Mul => call_binop(ctxt, std::ops::Mul::mul),
            Div => call_binop(ctxt, std::ops::Div::div),

            Eq => with_stack!(ctxt, [a, b] do {
                ctxt.data_stack.push(StackItem::Boolean(a == b));
                Ok(())
            }),
            GreaterThan => with_stack!(ctxt, [StackItem::Number(b), StackItem::Number(a)] do {
                ctxt.data_stack.push(StackItem::Boolean(a > b));
                Ok(())
            }),

            True => {
                ctxt.data_stack.push(StackItem::Boolean(true));
                Ok(())
            }
            And => with_stack!(ctxt, [StackItem::Boolean(a), StackItem::Boolean(b)] do {
                ctxt.data_stack.push(StackItem::Boolean(a && b));
                Ok(())
            }),
            Or => with_stack!(ctxt, [StackItem::Boolean(a), StackItem::Boolean(b)] do {
                ctxt.data_stack.push(StackItem::Boolean(a || b));
                Ok(())
            }),
            Not => with_stack!(ctxt, [StackItem::Boolean(a)] do {
                ctxt.data_stack.push(StackItem::Boolean(!a));
                Ok(())
            }),

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
                    ctxt.interpret_line(sw)?;
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

            BlockOpen => "{",
            BlockClose => "}",
            Conditional => "if-else",
            Loop => "loop",

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
            Rot => "rot",
            Show => ".s",
            Display => ".",

            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",

            Eq => "=",
            GreaterThan => ">",

            True => "true",
            And => "and",
            Or => "or",
            Not => "not",

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

    WaitingForCompilationIdentifier,
    WaitingForCommentClose(Box<Self>),
    WaitingForStringClose(Box<Self>, String),
    WaitingForBlockClose(Box<Self>, Vec<String>),

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

impl State {
    fn as_prompt(&self) -> &str {
        match self {
            State::Interpreting => "i> ",
            State::WaitingForCommentClose(_) => "(> ",
            State::WaitingForCompilationIdentifier => "?> ",
            State::WaitingForStringClose(_, _) => "\"> ",
            State::WaitingForBlockClose(_, _) => "{> ",
            State::Compiling { .. } => "c> ",
            State::CompilationPaused { .. } => "[> ",
        }
    }
}

#[derive(Debug)]
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

        install(BlockOpen);
        install(BlockClose);
        install(Conditional);
        install(Loop);

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
        install(Rot);
        install(Show);
        install(Display);

        install(Add);
        install(Sub);
        install(Mul);
        install(Div);

        install(Eq);
        install(GreaterThan);

        install(True);
        install(And);
        install(Or);
        install(Not);

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
            let readline = rl.readline(self.state.as_prompt());
            match readline {
                Ok(line) => {
                    rl.add_history_entry(line.as_str());
                    match self.interpret_line(line.as_str()) {
                        Ok(_) => {
                            println!("ok");
                            self.print_data_stack();
                        }
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
        for word in line.split_whitespace() {
            debug!("Read '{}' in stack: {:?}", word, &self.data_stack);
            // TODO Refactor this to return the next state, rather than an err flag
            let res = match word.to_lowercase().as_str() {
                "bye" => return Err(Error::UserExit),
                s => match &mut self.state {
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
                        } else if s == Word::Comment.name() {
                            self.state = State::WaitingForCommentClose(Box::new(
                                State::WaitingForCommentClose(prev.clone()),
                            ))
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
                            s2.push_str(word);
                            self.state = State::WaitingForStringClose(prev.clone(), s2);
                        }
                        Ok(())
                    }
                    State::WaitingForBlockClose(prev, ref mut subwords) => {
                        if s == Word::BlockClose.name() {
                            self.data_stack.push(StackItem::Block(subwords.clone()));
                            self.state = *prev.clone();
                        } else {
                            subwords.push(s.to_string());
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
        Ok(())
    }

    fn compile_word(&mut self, word: &str) -> Result<()> {
        match self.dictionary.get(word) {
            Some(xt) => {
                let xt2 = &xt.clone();
                xt2.compile(self)
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
                xt2.call(self)
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
    fn rot() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("1 2 3 rot")?;
        assert_eq!(Some(StackItem::Number(1)), ctxt.data_stack.pop());
        assert_eq!(Some(StackItem::Number(3)), ctxt.data_stack.pop());
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
        ctxt.interpret_line("2 1 -")?;
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

    #[test]
    fn comparisons() -> Result<()> {
        {
            let mut ctxt = new_context();
            ctxt.interpret_line("0 0 =")?;
            assert_eq!(Some(StackItem::Boolean(true)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line("0 1 =")?;
            assert_eq!(Some(StackItem::Boolean(false)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line("\" abc \" \" abc \" =")?;
            assert_eq!(Some(StackItem::Boolean(true)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line("3 2 >")?;
            assert_eq!(Some(StackItem::Boolean(true)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line("2 3 >")?;
            assert_eq!(Some(StackItem::Boolean(false)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        Ok(())
    }

    #[test]
    fn logic() -> Result<()> {
        {
            let mut ctxt = new_context();
            ctxt.interpret_line("true")?;
            assert_eq!(Some(StackItem::Boolean(true)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line("true true and")?;
            assert_eq!(Some(StackItem::Boolean(true)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line("true true or")?;
            assert_eq!(Some(StackItem::Boolean(true)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line("true true not and")?;
            assert_eq!(Some(StackItem::Boolean(false)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line("true true not or")?;
            assert_eq!(Some(StackItem::Boolean(true)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line("true true not and not")?;
            assert_eq!(Some(StackItem::Boolean(true)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        Ok(())
    }

    #[test]
    fn if_else() -> Result<()> {
        {
            let mut ctxt = new_context();
            ctxt.interpret_line("0 0 = { 1 } { 2 } if-else")?;
            assert_eq!(Some(StackItem::Number(1)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line("0 1 = { 1 } { 2 } if-else")?;
            assert_eq!(Some(StackItem::Number(2)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }
        Ok(())
    }

    #[test]
    fn simple_loop() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("1 { 0 } { dup 1 = } loop")?;
        assert_eq!(Some(StackItem::Number(0)), ctxt.data_stack.pop());
        assert_eq!(Some(StackItem::Number(1)), ctxt.data_stack.pop());
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn basic_strings() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("\" abc def  ghIJ 123 \"")?;
        assert_eq!(
            Some(StackItem::String("abc def ghIJ 123".to_string())),
            ctxt.data_stack.pop()
        );
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn basic_word_definitions() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line(": test 3 + ; 4 test")?;
        assert_eq!(Some(StackItem::Number(7)), ctxt.data_stack.pop());
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn compile_time_literal() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line(": test [ 4 3 + ] literal ; test")?;
        assert_eq!(Some(StackItem::Number(7)), ctxt.data_stack.pop());
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    // TODO Add support for using strings in word definitions.
    // #[test]
    fn string_in_word_definition() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line(": test \" foo \" ; test")?;
        assert_eq!(
            Some(StackItem::String("foo".to_string())),
            ctxt.data_stack.pop()
        );
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn basic_block() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line("{ test }")?;
        assert_eq!(
            Some(StackItem::Block(vec!["test".to_string()])),
            ctxt.data_stack.pop()
        );
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn block_in_word_definition() -> Result<()> {
        let mut ctxt = new_context();
        ctxt.interpret_line(": test { 2 3 + } ; test")?;
        assert_eq!(
            Some(StackItem::Block(vec![
                "2".to_string(),
                "3".to_string(),
                "+".to_string()
            ])),
            ctxt.data_stack.pop()
        );
        assert_eq!(None, ctxt.data_stack.pop());
        Ok(())
    }

    #[test]
    fn comments() -> Result<()> {
        {
            let mut ctxt = new_context();
            ctxt.interpret_line("( this is a comment )")?;
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line("(\n this is a multiline comment \n)")?;
            assert_eq!(None, ctxt.data_stack.pop());
        }

        {
            let mut ctxt = new_context();
            ctxt.interpret_line(": test ( this is a  comment ) 2 + ; 3 test")?;
            assert_eq!(Some(StackItem::Number(5)), ctxt.data_stack.pop());
            assert_eq!(None, ctxt.data_stack.pop());
        }

        Ok(())
    }

    #[test]
    fn bye() -> Result<()> {
        let mut ctxt = new_context();
        let res = ctxt.interpret_line("bye");
        assert_eq!(Err(Error::UserExit), res);

        Ok(())
    }

    #[test]
    fn undefined_word() -> Result<()> {
        let mut ctxt = new_context();
        let res = ctxt.interpret_line("foo");
        assert_eq!(Err(Error::UnrecognizedWord("foo".to_string())), res);

        Ok(())
    }

    #[test]
    fn invalid_states() {
        let lines = [
            "]",         // End compilation state while in interpretation state
            "[ [",       // Enter compilation state twice
            ";",         // End definition without starting one
            "[ :",       // Definition in compilation state
            ": foo :",   // Nested definition
            ": foo [ :", // Nested definition where the programmer was trying to be clever
            "literal",   // Literal used during interpretation
            "}",         // Unopened block
        ];
        for line in &lines {
            let mut ctxt = new_context();
            let res = ctxt.interpret_line(line);
            assert!(matches!(res, Err(Error::InvalidState(_))));
        }
    }

    #[test]
    fn changed_stack_size_error() {
        let mut ctxt = new_context();
        let res = ctxt.interpret_line(": foo [ 1 ] ;");
        assert_eq!(res, Err(Error::StackSizeChanged(0, 1)));
    }
}
