/// Board Description Language
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::collections::HashMap;
use std::str::FromStr;

#[derive(Clone)]
enum Word {
    // Interpreter
    Abort,
    Words,
    Colon,
    Compile,

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

    Custom(String, Vec<String>),
}

fn stack_underflow() -> bool {
    println!("Stack underflow");
    true
}

impl Word {
    fn call(&self, ctxt: &mut Context) -> bool {
        use Word::*;

        let err = match self {
            Abort => {
                ctxt.data_stack.clear();
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

            Drop => match ctxt.data_stack.pop() {
                Some(_) => false,
                None => stack_underflow(),
            },

            Dup => match ctxt.data_stack.pop() {
                Some(x) => {
                    ctxt.data_stack.push(x);
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
                Some(i) => {
                    if (i as usize) > ctxt.data_stack.len() {
                        stack_underflow()
                    } else {
                        let idx = ctxt.data_stack.len() - i as usize;
                        let x = ctxt.data_stack.get(idx).copied().unwrap_or(0);
                        ctxt.data_stack.push(x);
                        false
                    }
                }
                None => stack_underflow(),
            },
            Show => {
                ctxt.print_data_stack();
                false
            }
            Display => match ctxt.data_stack.pop() {
                Some(x) => {
                    println!("{}", x);
                    false
                }
                None => stack_underflow(),
            },

            Add => match ctxt.data_stack.pop() {
                Some(y) => match ctxt.data_stack.pop() {
                    Some(x) => {
                        ctxt.data_stack.push(x + y);
                        false
                    }
                    None => stack_underflow(),
                },
                None => stack_underflow(),
            },
            Sub => match ctxt.data_stack.pop() {
                Some(y) => match ctxt.data_stack.pop() {
                    Some(x) => {
                        ctxt.data_stack.push(x - y);
                        false
                    }
                    None => stack_underflow(),
                },
                None => stack_underflow(),
            },
            Mul => match ctxt.data_stack.pop() {
                Some(y) => match ctxt.data_stack.pop() {
                    Some(x) => {
                        ctxt.data_stack.push(x * y);
                        false
                    }
                    None => stack_underflow(),
                },
                None => stack_underflow(),
            },
            Div => match ctxt.data_stack.pop() {
                Some(y) => match ctxt.data_stack.pop() {
                    Some(x) => {
                        ctxt.data_stack.push(x / y);
                        false
                    }
                    None => stack_underflow(),
                },
                None => stack_underflow(),
            },
            Custom(_, subwords) => {
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

    fn name(&self) -> &str {
        use Word::*;
        // Note: All names should be normalized to lower case.
        match self {
            Abort => "abort",
            Words => "words",
            Colon => ":",
            Compile => "]",

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

            Custom(n, _) => n,
        }
    }
}

pub enum State {
    Interpreting,
    WaitingForCompilationIdentifier,
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
    data_stack: Vec<u64>,
    dictionary: HashMap<String, Word>,
    state: State,
}

impl Context {
    pub fn new() -> Self {
        use Word::*;
        let mut ctxt = Self {
            data_stack: Vec::new(),
            dictionary: HashMap::new(),
            state: State::Interpreting,
        };

        let mut install = |w: Word| ctxt.dictionary.insert(w.name().to_string(), w);

        install(Abort);
        install(Words);
        install(Colon);
        install(Compile);

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
            let readline = rl.readline("> ");
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
        for word in line.split_whitespace().map(|s| s.to_lowercase()) {
            let err = match word.as_str() {
                "bye" => return true,
                s => match self.state {
                    State::Interpreting | State::CompilationPaused { .. } => self.interpret_word(s),
                    State::WaitingForCompilationIdentifier => {
                        self.state = State::Compiling {
                            name: s.to_string(),
                            subwords: Vec::new(),
                            stack_count: self.data_stack.len(),
                        };
                        false
                    }
                    State::Compiling { .. } => self.compile_word(s),
                },
            };
            if err {
                break;
            }
        }
        println!("ok");
        self.print_data_stack();
        false
    }

    fn compile_word(&mut self, word: &str) -> bool {
        match word {
            "[" => {
                match self.state {
                    State::Compiling {
                        ref name,
                        ref subwords,
                        ref stack_count,
                    } => {
                        self.state = State::CompilationPaused {
                            name: name.clone(),
                            subwords: subwords.clone(),
                            stack_count: *stack_count,
                        }
                    }
                    _ => {
                        println!("Error: Encountered '[' outside of definition")
                    }
                }

                false
            }
            "literal" => match self.state {
                State::Compiling { ref mut subwords, .. } => match self.data_stack.pop() {
                    Some(x) => {
                        subwords.push(x.to_string());
                        false
                    }
                    None => stack_underflow(),
                },
                _ => {
                    println!("Error: 'literal' seen outside definition");
                    true
                }
            },
            ";" => match &self.state {
                State::Compiling {
                    name,
                    subwords,
                    stack_count,
                } => {
                    if self.dictionary.contains_key(name) {
                        println!("Error: '{}' already defined", name);
                        true
                    } else if *stack_count != self.data_stack.len() {
                        println!(
                            "Error: Stack size changed during definition (was {}, now {})",
                            stack_count,
                            self.data_stack.len()
                        );
                        true
                    } else {
                        self.dictionary
                            .insert(name.clone(), Word::Custom(name.clone(), subwords.clone()));
                        false
                    }
                }
                _ => {
                    println!("Error: Definition ended in non-compilation state");
                    false
                }
            },
            _ => match self.state {
                State::Compiling { ref mut subwords, .. } => {
                    // TODO Check that words exist
                    // TODO Prevent recursion
                    subwords.push(word.to_string());
                    false
                }
                _ => unreachable!("Expected to be in compilation state"),
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
                    self.data_stack.push(x);
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
        self.data_stack.iter().for_each(|i| print!("{} ", i));
        println!()
    }

    fn call(&mut self, xt: &Word) -> bool {
        xt.call(self)
    }
}
