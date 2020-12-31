use crate::model::Component;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::collections::HashMap;
use std::str::FromStr;
use std::sync::{Arc, RwLock};

/// Board Description Language

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

    // Data types
    Quote, // Starts and ends a string

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

    Defined(String, Vec<String>),
}

fn stack_underflow() -> bool {
    println!("Stack underflow");
    true
}

fn type_error(expected: &str, actual: &StackItem) -> bool {
    println!("Type error: expected {}, got {:?}", expected, actual);
    true
}

/// Call a binary operator on numeric types.
fn call_binop<F>(ctxt: &mut Context, op: F) -> bool
where
    F: Fn(u64, u64) -> u64,
{
    match ctxt.data_stack.pop() {
        Some(StackItem::Number(y)) => match ctxt.data_stack.pop() {
            Some(StackItem::Number(x)) => {
                ctxt.data_stack.push(StackItem::Number(op(x, y)));
                false
            }
            Some(x) => type_error("Number", &x),
            None => stack_underflow(),
        },
        Some(y) => type_error("Number", &y),
        None => stack_underflow(),
    }
}

impl Word {
    /// Perform the interpretation semantics of the word.
    ///
    /// For predefined words the behavior varies. For user definitions, the word will execute each
    /// of its subwords in turn.
    fn call(&self, ctxt: &mut Context) -> bool {
        use Word::*;

        let err = match self {
            Abort => {
                ctxt.data_stack.clear();
                ctxt.state = State::Interpreting;
                println!("Aborted");
                false
            }
            Words => {
                for w in ctxt.dictionary.keys() {
                    println!("{}", w);
                }
                false
            }
            Colon => match ctxt.state {
                State::Interpreting => {
                    ctxt.state = State::WaitingForCompilationIdentifier;
                    false
                }
                _ => {
                    println!("Error: Already in compilation state");
                    true
                }
            },
            SemiColon => {
                println!("Error: Cannot end definition in state {:?}", ctxt.state);
                true
            }
            Interpret => {
                println!("Error: Already in interpretation state");
                true
            }
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
                    false
                }
                _ => {
                    println!("Error: Already in compilation state");
                    true
                }
            },
            Literal => {
                println!("'Literal' seen outside of definition");
                true
            }
            Comment => {
                ctxt.state = State::WaitingForCommentClose(Box::new(ctxt.state.clone()));
                false
            }

            Quote => {
                ctxt.state =
                    State::WaitingForStringClose(Box::new(ctxt.state.clone()), String::new());
                false
            }

            Drop => match ctxt.data_stack.pop() {
                Some(_) => false,
                None => stack_underflow(),
            },

            Dup => match ctxt.data_stack.pop() {
                Some(x) => {
                    ctxt.data_stack.push(x.clone());
                    ctxt.data_stack.push(x);
                    false
                }
                None => stack_underflow(),
            },
            Swap => match ctxt.data_stack.pop() {
                Some(y) => match ctxt.data_stack.pop() {
                    Some(x) => {
                        ctxt.data_stack.push(y);
                        ctxt.data_stack.push(x);
                        false
                    }
                    None => stack_underflow(),
                },
                None => stack_underflow(),
            },
            Pick => match ctxt.data_stack.pop() {
                Some(StackItem::Number(i)) => {
                    if (i as usize) > ctxt.data_stack.len() {
                        stack_underflow()
                    } else {
                        let idx = ctxt.data_stack.len() - i as usize;
                        let x = ctxt
                            .data_stack
                            .get(idx)
                            .cloned()
                            .unwrap_or(StackItem::Number(0));
                        ctxt.data_stack.push(x);
                        false
                    }
                }
                Some(x) => type_error("Number", &x),
                None => stack_underflow(),
            },
            Show => {
                ctxt.print_data_stack();
                false
            }
            Display => match ctxt.data_stack.pop() {
                Some(x) => {
                    println!("{:?}", x);
                    false
                }
                None => stack_underflow(),
            },

            Add => call_binop(ctxt, std::ops::Add::add),
            Sub => call_binop(ctxt, std::ops::Sub::sub),
            Mul => call_binop(ctxt, std::ops::Mul::mul),
            Div => call_binop(ctxt, std::ops::Div::div),
            Defined(_, subwords) => {
                for sw in subwords.iter() {
                    if ctxt.interpret_word(sw) {
                        return true;
                    }
                }
                false
            }
        };
        match err {
            true => Abort.call(ctxt),
            false => false,
        }
    }

    /// Perform the compilation-time behavior of a word.
    ///
    /// Most words simply append themselves to the current definition during compilation time. A few
    /// exceptions exist; these are known in the Forth lexicon as "immediate" words. FR4's dialect
    /// does not allow the user to define their own immediate words, as they are hard coded here.
    fn compile(&self, ctxt: &mut Context) -> bool {
        use State::*;
        use Word::*;

        let err = match self {
            Abort => self.call(ctxt),
            Colon => {
                println!("Error: Already in compilation state");
                true
            }
            SemiColon => {
                if let State::Compiling {
                    ref name,
                    ref mut subwords,
                    ref stack_count,
                } = ctxt.state
                {
                    if ctxt.dictionary.contains_key(name) {
                        println!("Error: '{}' already defined", name);
                        true
                    } else if *stack_count != ctxt.data_stack.len() {
                        println!(
                            "Error: Stack size changed during definition (was {}, now {})",
                            stack_count,
                            ctxt.data_stack.len()
                        );
                        true
                    } else {
                        ctxt.dictionary
                            .insert(name.clone(), Word::Defined(name.clone(), subwords.clone()));
                        ctxt.state = State::Interpreting;
                        false
                    }
                } else {
                    println!("Error: Unexpected state {:?}", ctxt.state);
                    true
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
                    false
                }
                _ => {
                    println!("Error: Unexpected state {:?}", ctxt.state);
                    true
                }
            },
            Compile => {
                println!("Error: Already in compilation state");
                true
            }
            Literal => match ctxt.state {
                State::Compiling {
                    ref mut subwords, ..
                } => match ctxt.data_stack.pop() {
                    Some(StackItem::Number(x)) => {
                        subwords.push(x.to_string());
                        false
                    }
                    Some(x) => {
                        println!("Can't create literal from stack item {:?}", x);
                        true
                    }
                    None => stack_underflow(),
                },
                _ => {
                    println!("Error: unexpected state {:?}", ctxt.state);
                    true
                }
            },
            Comment => self.call(ctxt),
            Quote => self.call(ctxt),
            _ => {
                if let Compiling {
                    ref mut subwords, ..
                } = ctxt.state
                {
                    subwords.push(self.name().to_string());
                    false
                } else {
                    println!("Called compile() while not in compilation state!");
                    true
                }
            }
        };
        match err {
            true => Abort.call(ctxt),
            false => false,
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

            Quote => "\"",

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

#[derive(Debug, Clone)]
pub enum StackItem {
    Number(u64),
    String(String),
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

        install(Quote);

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

        ctxt
    }

    pub fn interpret(&mut self) {
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
                    if self.interpret_line(line.as_str()) {
                        break;
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    break;
                }
                Err(ReadlineError::Eof) => {
                    break;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                }
            }
        }
        rl.save_history("history.txt").unwrap();
    }

    fn interpret_line(&mut self, line: &str) -> bool {
        if !line.trim_start().starts_with("\\") {
            for word in line.split_whitespace().map(|s| s.to_lowercase()) {
                // TODO Refactor this to return the next state, rather than an err flag
                let err = match word.as_str() {
                    "bye" => return true,
                    s => match &self.state {
                        State::Interpreting | State::CompilationPaused { .. } => {
                            self.interpret_word(s)
                        }
                        State::Compiling { .. } => self.compile_word(s),
                        State::WaitingForCompilationIdentifier => {
                            self.state = State::Compiling {
                                name: s.to_string(),
                                subwords: Vec::new(),
                                stack_count: self.data_stack.len(),
                            };
                            false
                        }
                        State::WaitingForCommentClose(prev) => {
                            if s.ends_with(")") {
                                self.state = *prev.clone();
                            }
                            false
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
                            false
                        }
                    },
                };
                if err {
                    self.state = State::Interpreting;
                    break;
                }
            }
        }
        println!("ok");
        self.print_data_stack();
        false
    }

    fn compile_word(&mut self, word: &str) -> bool {
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
                        false
                    }
                    _ => {
                        println!("Unexpected state: {:?}", self.state);
                        true
                    }
                },
                Err(_) => {
                    println!("Unrecognized word '{}'", word);
                    true
                }
            },
        }
    }

    fn interpret_word(&mut self, word: &str) -> bool {
        match self.dictionary.get(word) {
            Some(xt) => {
                let xt2 = &xt.clone();
                self.call(xt2)
            }
            None => match u64::from_str(word) {
                Ok(x) => {
                    self.data_stack.push(StackItem::Number(x));
                    false
                }
                Err(_) => {
                    println!("Unrecognized word '{}'", word);
                    true
                }
            },
        }
    }

    fn print_data_stack(&self) {
        self.data_stack.iter().for_each(|i| print!("{:?} ", i));
        println!()
    }

    fn call(&mut self, xt: &Word) -> bool {
        xt.call(self)
    }

    fn compile(&mut self, xt: &Word) -> bool {
        xt.compile(self)
    }
}
