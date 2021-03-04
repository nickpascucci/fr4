//! Board Description Language

use crate::model::{Component, Point, Shape};
use rustyline::{error::ReadlineError, Editor};
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::{Arc, RwLock};
use thiserror::Error;

#[derive(Debug, Error)]
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
    IoError(#[from] std::io::Error),
    #[error("User exit")]
    UserExit,
    #[error("The word {0} is not implemented yet!")]
    #[allow(dead_code)]
    NotImplemented(String),
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum StackItem {
    Number(u64),
    String(String),
    Point(Point),
    Shape(Shape),
    Component(Component),
    Address(Address),
}

#[derive(Debug, Clone)]
pub enum Address {
    Model,
}

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
    Comment,
    Include,

    // Data types
    Quote, // Starts and ends a string
    Point,
    Rect,
    Board,
    Pad,

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

    // Memory
    Store,
    Load,
    Model, // Push the model address onto the stack

    Defined(String, Vec<String>),
}

/// Call a binary operator on numeric types.
fn call_binop<F>(ctxt: &mut Context, op: F) -> Result<()>
where
    F: Fn(u64, u64) -> u64,
{
    match ctxt.data_stack.pop() {
        Some(StackItem::Number(y)) => match ctxt.data_stack.pop() {
            Some(StackItem::Number(x)) => {
                ctxt.data_stack.push(StackItem::Number(op(x, y)));
                Ok(())
            }
            Some(x) => Err(Error::TypeError("Number".to_string(), x.clone())),
            None => Err(Error::StackUnderflow),
        },
        Some(y) => Err(Error::TypeError("Number".to_string(), y.clone())),
        None => Err(Error::StackUnderflow),
    }
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
            Include => match ctxt.data_stack.pop() {
                Some(StackItem::String(s)) => match PathBuf::from_str(&s) {
                    Ok(pb) => ctxt.interpret_file(&pb),
                    Err(_) => unreachable!(),
                },
                Some(x) => Err(Error::TypeError("String".to_string(), x.clone())),
                None => Err(Error::StackUnderflow),
            },

            Quote => {
                ctxt.state =
                    State::WaitingForStringClose(Box::new(ctxt.state.clone()), String::new());
                Ok(())
            }
            Point => match ctxt.data_stack.pop() {
                Some(StackItem::Number(y)) => match ctxt.data_stack.pop() {
                    Some(StackItem::Number(x)) => {
                        ctxt.data_stack
                            .push(StackItem::Point(crate::model::Point { x, y }));
                        Ok(())
                    }
                    Some(x) => Err(Error::TypeError("Number".to_string(), x.clone())),
                    None => Err(Error::StackUnderflow),
                },
                Some(y) => Err(Error::TypeError("Number".to_string(), y.clone())),
                None => Err(Error::StackUnderflow),
            },
            Rect => match ctxt.data_stack.pop() {
                Some(StackItem::Number(rot)) => match ctxt.data_stack.pop() {
                    Some(StackItem::Point(xy2)) => match ctxt.data_stack.pop() {
                        Some(StackItem::Point(xy1)) => {
                            ctxt.data_stack.push(StackItem::Shape(
                                crate::model::Shape::Rectangle { xy1, xy2, rot },
                            ));
                            Ok(())
                        }
                        Some(x) => Err(Error::TypeError("Point".to_string(), x.clone())),
                        None => Err(Error::StackUnderflow),
                    },
                    Some(y) => Err(Error::TypeError("Point".to_string(), y.clone())),
                    None => Err(Error::StackUnderflow),
                },
                Some(x) => Err(Error::TypeError("Number".to_string(), x.clone())),
                None => Err(Error::StackUnderflow),
            },
            Board => match ctxt.data_stack.pop() {
                Some(StackItem::Shape(shape)) => {
                    ctxt.data_stack
                        .push(StackItem::Component(Component::Board(shape)));
                    Ok(())
                }
                Some(x) => Err(Error::TypeError("Shape".to_string(), x.clone())),
                None => Err(Error::StackUnderflow),
            },
            Pad => match ctxt.data_stack.pop() {
                Some(StackItem::Shape(shape)) => {
                    ctxt.data_stack
                        .push(StackItem::Component(Component::Pad(shape)));
                    Ok(())
                }
                Some(x) => Err(Error::TypeError("Shape".to_string(), x.clone())),
                None => Err(Error::StackUnderflow),
            },

            Drop => match ctxt.data_stack.pop() {
                Some(_) => Ok(()),
                None => Err(Error::StackUnderflow),
            },

            Dup => match ctxt.data_stack.pop() {
                Some(x) => {
                    ctxt.data_stack.push(x.clone());
                    ctxt.data_stack.push(x);
                    Ok(())
                }
                None => Err(Error::StackUnderflow),
            },
            Swap => match ctxt.data_stack.pop() {
                Some(y) => match ctxt.data_stack.pop() {
                    Some(x) => {
                        ctxt.data_stack.push(y);
                        ctxt.data_stack.push(x);
                        Ok(())
                    }
                    None => Err(Error::StackUnderflow),
                },
                None => Err(Error::StackUnderflow),
            },
            Pick => match ctxt.data_stack.pop() {
                Some(StackItem::Number(i)) => {
                    if (i as usize) > ctxt.data_stack.len() {
                        Err(Error::StackUnderflow)
                    } else {
                        let idx = ctxt.data_stack.len() - i as usize;
                        let x = ctxt
                            .data_stack
                            .get(idx)
                            .cloned()
                            .unwrap_or(StackItem::Number(0));
                        ctxt.data_stack.push(x);
                        Ok(())
                    }
                }
                Some(x) => Err(Error::TypeError("Number".to_string(), x.clone())),
                None => Err(Error::StackUnderflow),
            },
            Show => {
                ctxt.print_data_stack();
                Ok(())
            }
            Display => match ctxt.data_stack.pop() {
                Some(x) => {
                    println!("{:?}", x);
                    Ok(())
                }
                None => Err(Error::StackUnderflow),
            },

            Add => call_binop(ctxt, std::ops::Add::add),
            Sub => call_binop(ctxt, std::ops::Sub::sub),
            Mul => call_binop(ctxt, std::ops::Mul::mul),
            Div => call_binop(ctxt, std::ops::Div::div),

            Store => match ctxt.data_stack.pop() {
                Some(StackItem::Address(addr)) => match addr {
                    Address::Model => match ctxt.data_stack.pop() {
                        Some(StackItem::Component(cmpt)) => {
                            let mut model_ref = ctxt.model.write().unwrap();
                            *model_ref = Box::new(cmpt);
                            Ok(())
                        }
                        Some(x) => Err(Error::TypeError("Component".to_string(), x.clone())),
                        None => Err(Error::StackUnderflow),
                    },
                },
                Some(x) => Err(Error::TypeError("Address".to_string(), x.clone())),
                None => Err(Error::StackUnderflow),
            },
            Load => match ctxt.data_stack.pop() {
                Some(StackItem::Address(addr)) => match addr {
                    Address::Model => {
                        let model_ref = ctxt.model.read().unwrap();
                        ctxt.data_stack
                            .push(StackItem::Component(*model_ref.clone()));
                        Ok(())
                    }
                },
                Some(x) => Err(Error::TypeError("Address".to_string(), x.clone())),
                None => Err(Error::StackUnderflow),
            },
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
                } => match ctxt.data_stack.pop() {
                    Some(StackItem::Number(x)) => {
                        subwords.push(x.to_string());
                        Ok(())
                    }
                    Some(x) => Err(Error::TypeError("Number".to_string(), x)),
                    None => Err(Error::StackUnderflow),
                },
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
            Board => "board",
            Pad => "pad",

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
    model: Arc<RwLock<Box<Component>>>, // TODO Update model
}

impl Context {
    pub fn new(model: Arc<RwLock<Box<Component>>>) -> Self {
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
        install(Board);
        install(Pad);

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

        install(Store);
        install(Load);
        install(Model);

        ctxt
    }

    pub fn interpret(&mut self) -> Result<()> {
        // `()` can be used when no completer is required
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
        let file = File::open(filename)?;
        let buf_reader = BufReader::new(file);
        for l in buf_reader.lines() {
            match l {
                Ok(line) => {
                    self.interpret_line(&line)?;
                }
                Err(e) => {
                    return Err(Error::from(e));
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
