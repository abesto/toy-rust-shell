// TODO add the rest of the characters that can start operators
const OPERATOR_START: &str = "<>$";

mod token_recognition {
    /*
     * http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_03
     * Shell Command Language - 2.3: Token Recognition
     * POSIX.1-2017
     */

    use super::OPERATOR_START;
    use std::vec;

    const BLANK: &str = " \t";

    #[derive(Debug)]
    struct State<'a> {
        input: &'a String,
        tokens: Vec<&'a str>,
        current_token_start: usize,
        current: Option<(usize, char)>,
        iterator: &'a mut std::str::CharIndices<'a>,
    }

    impl<'a> State<'a> {
        fn current_token(&mut self) -> &'a str {
            return &self.input[self.current_token_start..self.current_position()];
        }

        fn current_position(&mut self) -> usize {
            return self.current.map_or(self.input.len(), |x| x.0);
        }

        fn current_character(&mut self) -> Option<char> {
            return self.current.map(|x| x.1);
        }

        fn is_eof(&mut self) -> bool {
            return self.current.is_none();
        }

        fn current_character_in(&mut self, class: &str) -> bool {
            self.current_character()
                .map_or(false, |c| class.contains(c))
        }

        fn current_character_is(&mut self, expected: char) -> bool {
            self.current_character().map_or(false, |c| c == expected)
        }

        fn delimit(&mut self) {
            if self.current_position() > self.current_token_start {
                let token = self.current_token();
                self.tokens.push(token);
                self.current_token_start = self.current_position();
            }
        }

        fn skip(&mut self) {
            self.consume();
            self.current_token_start = self.current_position();
        }

        fn consume(&mut self) {
            self.current = self.iterator.next();
        }
    }

    pub fn tokenize<'a>(input: &'a String) -> Vec<String> {
        let mut state = State {
            input,
            tokens: vec![],
            current_token_start: 0,
            iterator: &mut input.char_indices(),
            current: None,
        };

        state.current = state.iterator.next();
        while !state.is_eof() {
            handle_char(&mut state);
        }
        // Handle EOF
        handle_char(&mut state);

        return state.tokens.iter().map(|s| String::from(*s)).collect();
    }

    fn handle_char<'a>(state: &mut State<'a>) {
        print!("{:?} ", state.current);
        // 1. If the end of input is recognized, the current token (if any) shall be delimited.
        if state.is_eof() {
            println!("EOF");
            state.delimit();
        }
        // 2. If the previous character was used as part of an operator and the
        // current character is not quoted and can be used with the previous
        // characters to form an operator, it shall be used as part of that
        // (operator) token.
        // TODO

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
            println!("OPERATOR_START -> delimit consume");
            state.delimit();
            state.consume();
        }
        // 7. If the current character is an unquoted <blank>, any token
        // containing the previous character is delimited and the current
        // character shall be discarded.
        // TODO check quoting
        else if state.current_character_in(BLANK) {
            println!("BLANK -> delimit skip");
            state.delimit();
            state.skip();
        }
        // 8. If the previous character was part of a word, the current
        // character shall be appended to that word.
        // TODO better check for "part of a word"
        else if state
            .current_token()
            .chars()
            .last()
            .map_or(false, |c| !OPERATOR_START.contains(c))
        {
            println!("NOT OPERATOR_START -> consume");
            state.consume();
        }
        // 9. If the current character is a '#', it and all subsequent
        // characters up to, but excluding, the next <newline> shall be
        // discarded as a comment. The <newline> that ends the line is not
        // considered part of the comment.
        else if state.current_character_is('#') {
            println!("COMMENT -> last");
            while !state.is_eof() {
                state.skip();
            }
        }
        // 10. The current character is used as the start of a new word.
        else {
            println!("NEW WORD -> delimit consume");
            state.delimit();
            state.consume();
        }
    }

    #[cfg(test)]
    mod tests {
        fn assert_tokenizes(input: &str, expected: Vec<&str>) {
            assert_eq!(super::tokenize(&String::from(input)), expected);
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

mod grammar {
    /*
     * https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_10
     * Shell Command Language - 2.10: Shell Grammar
     * POSIX.1-2017
     */

    // Should probably rewrite this using nom

    use super::OPERATOR_START;

    #[derive(Debug, PartialEq, Eq)]
    pub enum Symbol {
        Operator(char),
        AndIf,
        OrIf,
        // ... more multi-character operators
        Word(String),
    }

    type Rule = Fn(&String) -> Option<Symbol>;

    struct Rules<'a> {
        rules: Vec<&'a Rule>,
    }

    impl<'a> Rules<'a> {
        fn evaluate(&mut self, token: &String) -> Option<Symbol> {
            for rule in &self.rules {
                let result = rule(token);
                if result.is_some() {
                    return result;
                }
            }
            return None;
        }
    }

    pub fn parse(tokens: &Vec<String>) -> Vec<Symbol> {
        let mut rules = Rules {
            rules: vec![
                //// 2.10.1 Shell Grammar Lexical Conventions
                // 1. If the token is an operator, the token identifier for that operator shall result.
                &single_character_operator,
                &multi_character_operator,
                // 2. If the string consists solely of digits and the delimiter
                // character is one of '<' or '>', the token identifier
                // IO_NUMBER shall be returned.
                // TODO
                //// 2.10.2 Shell Grammar Rules
                // 1. [Command Name]
            ],
        };

        return tokens
            .iter()
            .map(|t| {
                rules
                    .evaluate(t)
                    .unwrap_or_else(|| panic!("syntax error near unexpected token `{}'", t))
            })
            .collect();
    }

    fn single_character_operator(token: &String) -> Option<Symbol> {
        if token.len() != 1 {
            return None;
        }
        let character = token.chars().next().unwrap();
        if OPERATOR_START.contains(character) {
            return Some(Symbol::Operator(character));
        }
        return None;
    }

    fn multi_character_operator(token: &String) -> Option<Symbol> {
        match token.as_str() {
            "&&" => Some(Symbol::AndIf),
            "||" => Some(Symbol::OrIf),
            _ => None,
        }
    }

    #[cfg(test)]
    mod tests {
        use super::super::token_recognition;
        use super::Symbol;

        fn assert_parses(input: &str, expected: Vec<Symbol>) {
            let tokens = token_recognition::tokenize(&String::from(input));
            assert_eq!(super::parse(&tokens), expected);
        }

        #[test]
        fn parse_multichar_operators() {
            assert_parses("&&", vec![Symbol::AndIf]);
            // TODO test the rest
        }

        #[test]
        fn parse_singlechar_operators() {
            assert_parses("<", vec![Symbol::Operator('<')]);
            // TODO test the rest
        }
    }
}
