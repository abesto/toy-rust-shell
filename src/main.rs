extern crate im;
extern crate nix;
extern crate termion;
#[macro_use] extern crate itertools;

use im::{hashmap, HashMap};
use std::io::{stdin, Stdout};
use termion::event::Key;
use termion::input::TermRead;


mod parser;

pub struct State {
    variables: HashMap<String, String>,
    command_line: String,
    cursor_index: usize,
    last_exit_code: u8,
}

impl State {
    fn new() -> State {
        State {
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

    pub fn update_commandline(state: &mut State, terminal: &mut RawTerminal<Stdout>) {
        let default_ps1 = &String::new();
        let ps1 = state.variables.get("PS1").unwrap_or(default_ps1);
        write!(
            terminal,
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
                terminal,
                "{}",
                termion::cursor::Left((state.command_line.len() - state.cursor_index) as u16)
            )
            .unwrap();
        }
        write!(terminal, "{}", termion::cursor::Show).unwrap();
        terminal.flush().unwrap();
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

mod execute {
    use crate::parser;
    use crate::parser::grammar::*;
    use std::process::Command as PCommand;

    pub fn execute(state: &mut super::State) {
        let input = &state.command_line;
        let p = parser::parse(&input);
        program(p);
    }

    fn program(p: Program) {
        for cp in p {
            match cp {
                CompleteCommand::WithSep(l, op) => {}
                CompleteCommand::WithoutSep(l) => list(l),
            }
        }
    }

    fn list(l: List) {
        match l {
            List::Single(x) => andor(x),
            List::Multi(l, op, x) => {}
        }
    }

    fn andor(ao: AndOr) {
        match ao {
            AndOr::Single(p) => pipeline(p),
            _ => {}
        }
    }

    fn pipeline(p: Pipeline) {
        for c in p.pipe_sequence {
            command(c)
        }
    }

    fn command(c: Command) {
        match c {
            Command::SimpleCommand(d) => simple_command(d),
            _ => {}
        }
    }

    fn simple_command(c: SimpleCommandData) {
        match c {
            SimpleCommandData::Name(s) => {
                let _status = PCommand::new(s).status();
            }
            SimpleCommandData::NameSuffix(name, suffix) => {
                // TODO io redirects
                let _status = PCommand::new(name).args(suffix.words).status();
            }
            _ => {}
        }
    }
}

enum CommandPromptResult {
    Exit,
    Execute,
}

fn command_prompt(state: &mut State) -> CommandPromptResult {
    let mut terminal = term::setup_terminal();
    state.reset_command_line();
    term::update_commandline(state, &mut terminal);

    for c in stdin().keys() {
        match c.unwrap() {
            Key::Ctrl('d') => return CommandPromptResult::Exit,
            Key::Char('\n') => {
                print!("\r\n");
                return CommandPromptResult::Execute;
            }
            Key::Backspace => {
                if state.cursor_index > 0 {
                    state.cursor_index -= 1;
                    state.command_line.remove(state.cursor_index);
                }
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
        term::update_commandline(state, &mut terminal);
    }

    CommandPromptResult::Exit
}

fn main() {
    let mut state = State::new().set_variable(String::from("PS1"), String::from("$ "));
    loop {
        match command_prompt(&mut state) {
            CommandPromptResult::Exit => break,
            CommandPromptResult::Execute => execute::execute(&mut state),
        }
    }
}
