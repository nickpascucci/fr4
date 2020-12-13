/// Board Description Language
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::collections::HashMap;
use std::str::FromStr;

#[derive(Clone)]
enum Word<'a> {
    Abort,
    Words,

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

    Custom(&'a str, Vec<&'a str>),
}

impl<'a> Word<'a> {
    fn call(&self, ctxt: &mut Context) -> bool {
        use Word::*;
        let stack_underflow = || {
            println!("Stack underflow");
            true
        };

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
                Some(y) => {
                    match ctxt.data_stack.pop() {
                        Some(x) => {
                            ctxt.data_stack.push(y);
                            ctxt.data_stack.push(x);
                            false
                        }
                        None => stack_underflow(),
                    }
                }
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

    fn name(&self) -> &'a str {
        use Word::*;
        // Note: All names should be normalized to lower case.
        match self {
            Abort => "abort",
            Words => "words",

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

pub struct Context<'a> {
    data_stack: Vec<u64>,
    dictionary: HashMap<&'a str, Word<'a>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        use Word::*;
        let mut ctxt = Self {
            data_stack: Vec::new(),
            dictionary: HashMap::new(),
        };

        let mut install = |w: Word<'a>| ctxt.dictionary.insert(w.name(), w);

        install(Abort);
        install(Words);

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
            match word.as_str() {
                "bye" => return true,
                _ => self.interpret_word(&word),
            };
        }
        println!("ok");
        self.print_data_stack();
        false
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
