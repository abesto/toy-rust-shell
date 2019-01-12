extern crate im;
extern crate termion;

use im::{hashmap, HashMap};
use std::io::{stdin, Stdout};
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::RawTerminal;

mod parser;

pub struct State {
    terminal: RawTerminal<Stdout>,
    variables: HashMap<String, String>,
    command_line: String,
    cursor_index: usize,
    last_exit_code: u8,
}

impl State {
    fn new(terminal: RawTerminal<Stdout>) -> State {
        State {
            terminal,
            variables: hashmap! {},
            command_line: String::new(),
            cursor_index: 0,
            last_exit_code: 0,
        }
    }

    fn set_variable(self, name: String, value: String) -> State {
        State {
            variables: self.variables.update(name, value),
            ..self
        }
    }

    fn reset_command_line(&mut self) {
        self.command_line = String::new();
        self.cursor_index = 0;
    }
}

mod utils {
    pub fn quote(string: &String) -> String {
        if !(string.contains(' ') || string.contains('\'')) {
            return string.to_owned();
        }
        let mut escaped = string.replace("/", "\\/");
        escaped.push('\'');
        escaped.insert(0, '\'');
        return escaped;
    }
}

mod term {
    use crate::State;
    use std::io::{stdout, Stdout, Write};
    use termion::raw::{IntoRawMode, RawTerminal};

    pub fn setup_terminal() -> RawTerminal<Stdout> {
        let mut terminal = stdout().into_raw_mode().unwrap();
        write!(terminal, "{}", termion::cursor::Show).unwrap();
        return terminal;
    }

    pub fn update_prompt(state: &mut State) {
        let default_ps1 = &String::new();
        let ps1 = state.variables.get("PS1").unwrap_or(default_ps1);
        write!(
            state.terminal,
            "{}{}{}{}{}",
            termion::cursor::Hide,
            termion::clear::CurrentLine,
            "\r",
            ps1,
            state.command_line,
        )
        .unwrap();
        if state.cursor_index < state.command_line.len() {
            write!(
                state.terminal,
                "{}",
                termion::cursor::Left((state.command_line.len() - state.cursor_index) as u16)
            )
            .unwrap();
        }
        write!(state.terminal, "{}", termion::cursor::Show).unwrap();
        state.terminal.flush().unwrap();
    }
}

mod builtins {
    use crate::utils;
    pub use crate::State;

    pub fn set(state: &State) {
        for (name, value) in state.variables.iter() {
            println!("{}={}", name, utils::quote(value));
        }
    }
}

fn execute(state: &mut State) {}

fn command_prompt(state: &mut State) -> bool {
    state.reset_command_line();
    term::update_prompt(state);

    for c in stdin().keys() {
        match c.unwrap() {
            Key::Ctrl('d') => return false,
            Key::Char('\n') => {
                execute(state);
                return true;
            }
            Key::Char(c) => {
                state.command_line.insert(state.cursor_index, c);
                state.cursor_index += 1;
            }
            Key::Left => {
                if state.cursor_index > 0 {
                    state.cursor_index -= 1;
                }
            }
            Key::Right => {
                if state.cursor_index < state.command_line.len() {
                    state.cursor_index += 1;
                }
            }
            _ => continue,
        };
        term::update_prompt(state);
    }

    return true;
}

fn main() {
    let mut state =
        State::new(term::setup_terminal()).set_variable(String::from("PS1"), String::from("$ "));
    loop {
        if !(command_prompt(&mut state)) {
            break;
        }
    }
}
