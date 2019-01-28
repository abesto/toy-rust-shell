mod split_words {
    /*
     * http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_03
     * Shell Command Language - 2.3: Token Recognition
     * POSIX.1-2017
     */

    use std::vec;

    const OPERATOR_START: &str = "<>$&;";
    const BLANK: &str = " \t";

    #[derive(Debug)]
    struct State<'a> {
        input: &'a String,
        words: Vec<&'a str>,
        current_word_start: usize,
        iterator: &'a mut std::iter::Peekable<std::str::CharIndices<'a>>,
    }

    impl<'a> State<'a> {
        fn current_word(&mut self) -> &'a str {
            return &self.input[self.current_word_start..self.current_position()];
        }

        fn current_position(&mut self) -> usize {
            return self.iterator.peek().map_or(self.input.len(), |x| x.0);
        }

        fn current_character(&mut self) -> Option<char> {
            return self.iterator.peek().map(|x| x.1);
        }

        fn is_eof(&mut self) -> bool {
            return self.iterator.peek().is_none();
        }

        fn current_character_in(&mut self, class: &str) -> bool {
            self.current_character()
                .map_or(false, |c| class.contains(c))
        }

        fn current_character_is(&mut self, expected: char) -> bool {
            self.current_character().map_or(false, |c| c == expected)
        }

        fn delimit(&mut self) {
            if self.current_position() > self.current_word_start {
                let token = self.current_word();
                self.words.push(token);
                self.current_word_start = self.current_position();
            }
        }

        fn skip(&mut self) {
            self.consume();
            self.current_word_start = self.current_position();
        }

        fn consume(&mut self) {
            self.iterator.next();
        }
    }

    pub fn scan<'a>(input: &'a String) -> Vec<String> {
        let mut state = State {
            input,
            words: vec![],
            current_word_start: 0,
            iterator: &mut input.char_indices().peekable(),
        };

        while !state.is_eof() {
            handle_char(&mut state);
        }
        // Handle EOF
        handle_char(&mut state);

        return state.words.iter().map(|s| String::from(*s)).collect();
    }

    fn extend_operator<'a>(state: &mut State<'a>) -> bool {
        match state.current_character() {
            None => false,
            Some(c) => match (state.current_word(), c) {
                // TODO add the rest
                ("&", '&') => true,
                ("|", '|') => true,
                (">", '>') => true,
                ("<", '<') => true,
                _ => false,
            },
        }
    }

    fn handle_char<'a>(state: &mut State<'a>) {
        // 1. If the end of input is recognized, the current token (if any) shall be delimited.
        if state.is_eof() {
            //println!("EOF");
            state.delimit();
        }
        // 2. If the previous character was used as part of an operator and the
        // current character is not quoted and can be used with the previous
        // characters to form an operator, it shall be used as part of that
        // (operator) token.
        else if extend_operator(state) {
            //println!("EXTEND OPERATOR");
            state.consume();
        }
        // 3. If the previous character was used as part of an operator and the
        // current character cannot be used with the previous characters to form
        // an operator, the operator containing the previous character shall be
        // delimited.
        // TODO

        // 4. If the current character is <backslash>, single-quote, or
        // double-quote and it is not quoted, it shall affect quoting for
        // subsequent characters up to the end of the quoted text. The rules for
        // quoting are as described in Quoting . During token recognition no
        // substitutions shall be actually performed, and the result token shall
        // contain exactly the characters that appear in the input (except for
        // <newline> joining), unmodified, including any embedded or enclosing
        // quotes or substitution operators, between the <quotation-mark> and
        // the end of the quoted text. The token shall not be delimited by the
        // end of the quoted field.
        // TODO

        // 5. If the current character is an unquoted '$' or '`', the shell
        // shall identify the start of any candidates for parameter expansion
        // (Parameter Expansion), command substitution (Command Substitution),
        // or arithmetic expansion (Arithmetic Expansion) from their
        // introductory unquoted character sequences: '$' or "${", "$(" or '`',
        // and "$((", respectively. The shell shall read sufficient input to
        // determine the end of the unit to be expanded (as explained in the
        // cited sections). While processing the characters, if instances of
        // expansions or quoting are found nested within the substitution, the
        // shell shall recursively process them in the manner specified for the
        // construct that is found. The characters found from the beginning of
        // the substitution to its end, allowing for any recursion necessary to
        // recognize embedded constructs, shall be included unmodified in the
        // result token, including any embedded or enclosing substitution
        // operators or quotes. The token shall not be delimited by the end of
        // the substitution.
        // TODO

        // 6. If the current character is not quoted and can be used as the
        // first character of a new operator, the current token (if any) shall
        // be delimited. The current character shall be used as the beginning of
        // the next (operator) token.
        // TODO check quoting
        else if state.current_character_in(OPERATOR_START) {
            //println!("OPERATOR_START -> delimit consume");
            state.delimit();
            state.consume();
        }
        // 7. If the current character is an unquoted <blank>, any token
        // containing the previous character is delimited and the current
        // character shall be discarded.
        // TODO check quoting
        else if state.current_character_in(BLANK) {
            //println!("BLANK -> delimit skip");
            state.delimit();
            state.skip();
        }
        // 8. If the previous character was part of a word, the current
        // character shall be appended to that word.
        // TODO better check for "part of a word"
        else if state
            .current_word()
            .chars()
            .last()
            .map_or(false, |c| !OPERATOR_START.contains(c))
        {
            //println!("NOT OPERATOR_START -> consume");
            state.consume();
        }
        // 9. If the current character is a '#', it and all subsequent
        // characters up to, but excluding, the next <newline> shall be
        // discarded as a comment. The <newline> that ends the line is not
        // considered part of the comment.
        else if state.current_character_is('#') {
            //println!("COMMENT -> last");
            while !state.is_eof() {
                state.skip();
            }
        }
        // 10. The current character is used as the start of a new word.
        else {
            //println!("NEW WORD -> delimit consume");
            state.delimit();
            state.consume();
        }
    }

    #[cfg(test)]
    mod tests {
        fn assert_tokenizes(input: &str, expected: Vec<&str>) {
            assert_eq!(super::scan(&String::from(input)), expected);
        }

        #[test]
        fn tokenize_empty_string() {
            assert_tokenizes("", vec![]);
        }

        #[test]
        fn tokenize_single_word() {
            assert_tokenizes("set", vec!["set"]);
        }

        #[test]
        fn tokenize_multiple_words() {
            assert_tokenizes(
                "ls /home/  /    /usr/share\t~",
                vec!["ls", "/home/", "/", "/usr/share", "~"],
            );
        }

        #[test]
        fn tokenize_operator_in_word() {
            assert_tokenizes("foo$x", vec!["foo", "$", "x"]);
        }

        #[test]
        fn tokenize_comment() {
            assert_tokenizes("foo bar  # wheee whoo # wheee", vec!["foo", "bar"]);
        }
    }
}

mod lexer {
    /*
     * Implements parts of
     * https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_10
     * Shell Command Language - 2.10: Shell Grammar
     * POSIX.1-2017
     */

    use std::slice;

    #[derive(Debug, PartialEq, Eq)]
    pub enum Token {
        Operator(char),
        AndIf,
        OrIf,
        // ... more multi-character operators
        Word(String),
    }

    struct Lexer<'a> {
        input: &'a Vec<String>,
        iterator: &'a mut std::iter::Peekable<slice::Iter<'a, String>>,
        tokens: Vec<Token>,
    }

    impl<'a> Lexer<'a> {
        // Apply logic, including individual token recognizers, to a lexer, and extract the resulting tokens
        fn apply(input: &'a Vec<String>, logic: fn(&mut Lexer) -> ()) -> Vec<Token> {
            let mut l = Lexer {
                input,
                iterator: &mut input.iter().peekable(),
                tokens: vec![],
            };
            logic(&mut l);
            return l.tokens;
        }

        // Helpers for individual token recognizers
        fn recognize(&mut self, token: Token) -> bool {
            self.tokens.push(token);
            self.iterator.next();
            true
        }

        fn tag(&mut self, expected: &str, token: Token) -> bool {
            if self.iterator.peek().map_or(false, |next| next == &expected) {
                self.recognize(token)
            } else {
                false
            }
        }

        // Individual token recognizers
        fn andif(&mut self) -> bool {
            self.tag("&&", Token::AndIf)
        }

        fn orif(&mut self) -> bool {
            self.tag("||", Token::OrIf)
        }

        fn single_char_operator(&mut self) -> bool {
            match self.iterator.peek() {
                None => false,
                Some(word) if word.len() != 1 => false,
                Some(word) => {
                    let character = word.chars().next().unwrap();
                    if "()&;<>".contains(character) {
                        self.recognize(Token::Operator(character))
                    } else {
                        false
                    }
                }
            }
        }

        fn word(&mut self) -> bool {
            self.iterator
                .next()
                .map(|word| self.tokens.push(Token::Word(word.clone())));
            return true;
        }
    }

    pub fn scan(input: &Vec<String>) -> Vec<Token> {
        Lexer::apply(input, |lexer| {
            while lexer.iterator.peek().is_some() {
                let _ =
                    lexer.andif() || lexer.orif() || lexer.single_char_operator() || lexer.word();
            }
        })
    }

    #[cfg(test)]
    mod tests {
        use super::{Lexer, Token};
        use crate::parser::split_words;

        fn assert_apply(input: &str, expected: Vec<Token>, logic: fn(&mut Lexer) -> ()) {
            let words = &split_words::scan(&input.to_string());
            assert_eq!(Lexer::apply(words, logic), expected);
        }

        fn assert_lex(input: &str, expected: Vec<Token>) {
            let words = &split_words::scan(&input.to_string());
            assert_eq!(super::scan(words), expected);
        }

        fn word(input: &str) -> Token {
            Token::Word(input.to_string())
        }

        #[test]
        fn lex_word() {
            assert_apply("foobar", vec![word("foobar")], |l| {
                l.word();
            });
        }

        #[test]
        fn true_and_false() {
            assert_lex(
                "true && false",
                vec![word("true"), Token::AndIf, word("false")],
            );
        }

        #[test]
        fn single_char_op() {
            assert_lex(
                "ls; sleep 30\t& ls",
                vec![
                    word("ls"),
                    Token::Operator(';'),
                    word("sleep"),
                    word("30"),
                    Token::Operator('&'),
                    word("ls"),
                ],
            );
        }
    }
}

pub mod grammar {
    use super::lexer::Token;
    use itertools::multipeek;
    use itertools::structs::MultiPeek;
    use std::iter;
    use std::slice;

    pub type Program<'a> = Vec<CompleteCommand<'a>>;

    #[derive(Debug, Eq, PartialEq)]
    pub enum CompleteCommand<'a> {
        WithSep(List<'a>, SeparatorOp),
        WithoutSep(List<'a>),
    }

    #[derive(Debug, Eq, PartialEq)]
    pub enum List<'a> {
        Single(AndOr<'a>),
        Multi(&'a List<'a>, SeparatorOp, AndOr<'a>),
    }

    #[derive(Debug, Eq, PartialEq)]
    pub enum SeparatorOp {
        Ampersand,
        Semicolon,
    }

    #[derive(Debug, Eq, PartialEq)]
    pub enum AndOr<'a> {
        Single(Pipeline),
        And(&'a AndOr<'a>, Pipeline),
        Or(&'a AndOr<'a>, Pipeline),
    }

    #[derive(Debug, Eq, PartialEq)]
    pub struct Pipeline {
        pub has_bang: bool,
        pub pipe_sequence: PipeSequence,
    }

    pub type PipeSequence = Vec<Command>;

    #[derive(Debug, Eq, PartialEq)]
    pub enum Command {
        SimpleCommand(SimpleCommandData),
        CompoundCommand(CompoundCommandData),
        CompoundCommandWithRedirects(CompoundCommandWithRedirectsData),
        FunctionDefinition(FunctionDefinitionData),
    }

    #[derive(Debug, Eq, PartialEq)]
    pub enum SimpleCommandData {
        PrefixWordSuffix(CmdPrefix, CmdWord, CmdSuffix),
        PrefixWord(CmdPrefix, CmdWord),
        Prefix(CmdPrefix),
        NameSuffix(CmdName, CmdSuffix),
        Name(CmdName),
    }

    pub type CmdName = String;

    pub type CmdWord = String;

    #[derive(Debug, Eq, PartialEq)]
    pub struct CmdSuffix {
        pub redirects: Vec<IORedirect>,
        pub words: Vec<String>,
    }

    // Placeholders

    #[derive(Debug, Eq, PartialEq)]
    pub enum CmdPrefix {}

    #[derive(Debug, Eq, PartialEq)]
    pub enum CompoundCommandData {}

    #[derive(Debug, Eq, PartialEq)]
    pub enum CompoundCommandWithRedirectsData {}

    #[derive(Debug, Eq, PartialEq)]
    pub enum FunctionDefinitionData {}

    #[derive(Debug, Eq, PartialEq)]
    pub struct IORedirect {}

    pub fn build_parse_tree<'a>(tokens: &Vec<Token>) -> Program<'a> {
        let mut program: Program<'a> = vec![];
        let mut iterator = multipeek(tokens);

        while iterator.peek().is_some() {
            program.push(complete_command(&mut iterator).unwrap());
        }

        program
    }

    fn complete_command<'a>(
        iterator: &mut MultiPeek<slice::Iter<Token>>,
    ) -> Option<CompleteCommand<'a>> {
        match list(iterator) {
            None => None,
            Some(list) => {
                match separator_op(iterator) {
                    None => Some(CompleteCommand::WithoutSep(list)),
                    Some(sep) => Some(CompleteCommand::WithSep(list, sep))
                }
            }
        }
    }

    fn accept<I, T>(iterator: &mut MultiPeek<I>, retval: T) -> Option<T> where I: Iterator {
        for i in 0..iterator.index {
            iterator.next();
        }
        iterator.reset_peek()
        Some(retval)
    }

    fn separator_op(
        iterator: &mut MultiPeek<slice::Iter<Token>>,
    ) -> Option<SeparatorOp> {
        match iterator.peek() {
            Token::Operator(';') => accept(iterator, SeparatorOp::Semicolon),
            Token::Operator('&') => accept(iterator, SeparatorOp::Ampersand),

        }
    }

    fn simple_command(iterator: &mut MultiPeek<slice::Iter<Token>>) -> Option<Command> {
        match cmd_prefix(iterator) {
            Some(_prefix) => None, // TODO
            None => match cmd_name(iterator) {
                None => None,
                Some(name) => match cmd_suffix(iterator) {
                    None => Some(Command::SimpleCommand(SimpleCommandData::Name(name))),
                    Some(suffix) => Some(Command::SimpleCommand(SimpleCommandData::NameSuffix(
                        name, suffix,
                    ))),
                },
            },
        }
    }

    fn cmd_prefix(iterator: &mut MultiPeek<slice::Iter<Token>>) -> Option<CmdPrefix> {
        None
    }

    fn word(iterator: &mut MultiPeek<slice::Iter<Token>>) -> Option<String> {
        match iterator.peek() {
            Some(Token::Word(word)) => {
                iterator.next();
                Some(word.to_string())
            }
            _ => None,
        }
    }

    fn cmd_name(iterator: &mut MultiPeek<slice::Iter<Token>>) -> Option<CmdName> {
        word(iterator)
    }

    fn cmd_suffix(iterator: &mut MultiPeek<slice::Iter<Token>>) -> Option<CmdSuffix> {
        // TODO io_redirect
        let mut suffix = CmdSuffix {
            redirects: vec![],
            words: vec![],
        };

        loop {
            match word(iterator) {
                None => break,
                Some(word) => {
                    suffix.words.push(word);
                }
            }
        }

        if suffix.redirects.is_empty() && suffix.words.is_empty() {
            None
        } else {
            Some(suffix)
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        fn assert_tree(input: &str, expected: Program) {
            assert_eq!(super::super::parse(&input.to_string()), expected);
        }

        #[test]
        fn empty_input() {
            assert_tree("", vec![]);
        }

        #[test]
        fn ls() {
            assert_tree(
                "ls",
                vec![CompleteCommand::WithoutSep(List::Single(AndOr::Single(
                    Pipeline {
                        has_bang: false,
                        pipe_sequence: vec![Command::SimpleCommand(SimpleCommandData::Name(
                            "ls".to_string(),
                        ))],
                    },
                )))],
            )
        }

        #[test]
        fn ls_l() {
            assert_tree(
                "ls -l \t -a    /",
                vec![CompleteCommand::WithoutSep(List::Single(AndOr::Single(
                    Pipeline {
                        has_bang: false,
                        pipe_sequence: vec![Command::SimpleCommand(SimpleCommandData::NameSuffix(
                            "ls".to_string(),
                            CmdSuffix {
                                redirects: vec![],
                                words: vec!["-l".to_string(), "-a".to_string(), "/".to_string()],
                            },
                        ))],
                    },
                )))],
            )
        }

        #[test]
        fn separators() {
            assert_tree(
                "echo foo; uptime& ls",
                vec![CompleteCommand::WithoutSep(
                    List::Multi(
                        &List::Multi(
                            &List::Single(
                                AndOr::Single(
                                    Pipeline {
                                        has_bang: false,
                                        pipe_sequence: vec![Command::SimpleCommand(SimpleCommandData::NameSuffix(
                                            "echo".to_string(),
                                            CmdSuffix {
                                                redirects: vec![],
                                                words: vec!["foo".to_string()],
                                            },
                                        ))],
                                    },
                                )
                            ),
                            SeparatorOp::Semicolon,
                            AndOr::Single(
                                Pipeline {
                                    has_bang: false,
                                    pipe_sequence: vec![Command::SimpleCommand(SimpleCommandData::NameSuffix(
                                        "uptime".to_string(),
                                        CmdSuffix {
                                            redirects: vec![],
                                            words: vec![],
                                        },
                                    ))],
                                },
                            )
                        ),
                        SeparatorOp::Ampersand,
                        AndOr::Single(
                            Pipeline {
                                has_bang: false,
                                pipe_sequence: vec![Command::SimpleCommand(SimpleCommandData::NameSuffix(
                                    "ls".to_string(),
                                    CmdSuffix {
                                        redirects: vec![],
                                        words: vec![],
                                    },
                                ))],
                            },
                        )
                    )
                )],
            )
        }
    }
}

pub fn parse<'a>(input: &String) -> grammar::Program<'a> {
    let words = &split_words::scan(&input);
    let tokens = &lexer::scan(&words);
    grammar::build_parse_tree(tokens)
}
