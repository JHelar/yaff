use std::collections::HashMap;
use std::env;
use std::fmt;
use std::fs;
use std::iter::Peekable;
use std::ops::Index;
use std::path::Path;

type PropsMap = HashMap<String, Expr>;
type ComponentsMap = HashMap<String, Component>;
type DataMap = HashMap<String, Data>;

#[derive(Debug)]
enum Expr {
    Value(String),
    PropReference(String),
    DataRaference(String),
    Element(String, Vec<Expr>, PropsMap),
    Collection(Vec<Expr>),
    ElementEnd(String),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Value(value) => write!(f, "{}", value),
            Expr::PropReference(name) => write!(f, "{{{}}}", name),
            Expr::DataRaference(name) => write!(f, "{{{}}}", name),
            Expr::Element(name, children, props) => {
                write!(f, "<{}", name)?;
                for (prop_name, prop_value) in props {
                    write!(f, " {}=", prop_name)?;
                    match prop_value {
                        Expr::Value(_) => write!(f, "\"{}\"", prop_value)?,
                        Expr::PropReference(_) => write!(f, "{}", prop_value)?,
                        Expr::DataRaference(_) => write!(f, "{}", prop_value)?,
                        Expr::Element(_, _, _) => write!(f, "{{{}}}", prop_value)?,
                        Expr::Collection(_) => write!(f, "\"{}\"", prop_value)?,
                        _ => {}
                    }
                }
                if children.len() > 0 {
                    write!(f, ">")?;
                    for child in children {
                        write!(f, "{}", child)?;
                    }
                    write!(f, "</{}>", name)
                } else {
                    write!(f, " />")
                }
            }
            Expr::Collection(elements) => {
                for element in elements {
                    write!(f, "{}", element)?;
                }
                Ok(())
            }
            Expr::ElementEnd(name) => Ok(()),
        }
    }
}

impl Expr {
    fn compile(&self, data: &DataMap, props: &PropsMap, components: &ComponentsMap) -> String {
        match self {
            Expr::Value(value) => value.to_string(),
            Expr::PropReference(name) => match props.get(name) {
                Some(value) => value.compile(data, props, components),
                None => todo!("Error missing {:?} value from props {:?}", self, props),
            },
            Expr::DataRaference(name) => match data.get(name) {
                Some(value) => {
                    "".to_string()
                },
                None => todo!("Error \"{}\" is undefined", name),
            },
            Expr::Element(name, children, expression_props) => match components.get(name) {
                Some(component) => {
                    let mut component_props = HashMap::new();
                    for (prop_name, prop_value) in expression_props {
                        component_props.insert(
                            prop_name.to_string(),
                            Expr::Value(prop_value.compile(data, props, components)),
                        );
                    }

                    let mut component_children = String::new();
                    for child in children {
                        component_children.push_str(&child.compile(data, props, components));
                    }
                    component_props.insert(
                        Keyword::ComponentChildren.value(),
                        Expr::Value(component_children),
                    );

                    component.compile(data, &component_props, components)
                }
                None => {
                    let mut result = format!("<{}", name);

                    for (prop_name, prop_value) in expression_props {
                        result.push_str(&format!(
                            " {}=\"{}\"",
                            prop_name,
                            prop_value.compile(data, props, components)
                        ));
                    }
                    result.push_str(">");

                    for child in children {
                        result.push_str(&child.compile(data, props, components));
                    }

                    result.push_str(&format!("</{}>", name));
                    result
                }
            },
            Expr::Collection(elements) => {
                let mut result = String::new();

                for element in elements {
                    result.push_str(&element.compile(data, props, components));
                }
                result
            }
            Expr::ElementEnd(name) => {
                format!("</{}>", name)
            }
        }
    }

    fn parse_peekable(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Option<Self> {
        if let Some(token) = lexer.next() {
            return match token.kind {
                TokenKind::LessThen => {
                    match lexer.next() {
                        Some(Token {
                            kind: TokenKind::Sym,
                            text: component_name,
                            ..
                        }) => {
                            Parser::trim_space(lexer);

                            let mut props = HashMap::new();
                            let mut children: Vec<Expr> = Vec::new();

                            // Parse props
                            while let Some(prop_token) = lexer.next_if(|t| t.kind == TokenKind::Sym)
                            {
                                if let Some(should_be_equals_token) = lexer.next() {
                                    if should_be_equals_token.kind == TokenKind::Equals {
                                        let prop = Self::parse_peekable(lexer).unwrap();
                                        props.insert(prop_token.text, prop);
                                    } else {
                                        todo!(
                                            "Unexpected token {}, expected an equals",
                                            should_be_equals_token.text
                                        )
                                    }
                                }
                                Parser::trim_space(lexer);
                            }

                            // Parse children
                            if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::GreaterThen) {
                                Parser::trim_space(lexer);
                                let mut run = true;
                                while run {
                                    if let Some(child_token_peeked) = lexer.peek() {
                                        match child_token_peeked.kind {
                                            TokenKind::Sym => {
                                                if let Some(child_token) = lexer.next() {
                                                    children.push(Expr::Value(child_token.text));
                                                } else {
                                                    todo!("Peekable value turned bad!");
                                                }
                                            }
                                            TokenKind::Space => {
                                                if let Some(child_token) = lexer.next() {
                                                    children.push(Expr::Value(child_token.text));
                                                } else {
                                                    todo!("Peekable value turned bad!");
                                                }
                                            }
                                            TokenKind::Quote => {
                                                if let Some(child_token) = lexer.next() {
                                                    children.push(Expr::Value(child_token.text));
                                                } else {
                                                    todo!("Peekable value turned bad!");
                                                }
                                            }
                                            _ => {
                                                let child = Self::parse_peekable(lexer).unwrap();
                                                match child {
                                                    Expr::ElementEnd(end_tag_name) => {
                                                        if end_tag_name == component_name {
                                                            run = false
                                                        } else {
                                                            todo!("Expected end tag with name '{}' got '{}'", component_name, end_tag_name)
                                                        }
                                                    }
                                                    _ => children.push(child),
                                                }
                                            }
                                        }
                                    }
                                }

                                return Some(Expr::Element(
                                    component_name.to_string(),
                                    children,
                                    props,
                                ));
                            }

                            // // Self closing element
                            if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::Slash) {
                                if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::GreaterThen)
                                {
                                    return Some(Expr::Element(
                                        component_name.to_string(),
                                        children,
                                        props,
                                    ));
                                } else {
                                    todo!("Unexpected token, expected a closure '>' tag.")
                                }
                            }

                            todo!("Unexpected token, expected a closure tag.")
                            // Closing tag
                        }
                        Some(Token {
                            kind: TokenKind::Slash,
                            ..
                        }) => {
                            // Could be an end tag continue checking
                            match lexer.next() {
                                Some(Token {
                                    kind: TokenKind::Sym,
                                    text: end_tag_name,
                                    ..
                                }) => match lexer.next() {
                                    Some(Token {
                                        kind: TokenKind::GreaterThen,
                                        ..
                                    }) => Some(Expr::ElementEnd(end_tag_name)),
                                    None => todo!("Expected end tag >, got EOF"),
                                    Some(invalid_token) => todo!(
                                        "Unexpected token {:?}, expected > tag",
                                        invalid_token
                                    ),
                                },
                                None => todo!("Expected end tag symbol, got EOF"),
                                Some(invalid_token) => todo!(
                                    "Unexpected token {:?}, expected end tag name",
                                    invalid_token
                                ),
                            }
                        }
                        Some(next_token) => todo!(
                            "Unexpected token {}, expected component name",
                            next_token.text
                        ),
                        _ => todo!("Expected token got nothing"),
                    }
                }
                TokenKind::Quote => {
                    let mut values: Vec<Expr> = Vec::new();
                    let mut run = true;
                    while run {
                        if let Some(next_token_peeked) = lexer.peek() {
                            match next_token_peeked.kind {
                                TokenKind::Sym => {
                                    if let Some(next_token) = lexer.next() {
                                        values.push(Expr::Value(next_token.text));
                                    } else {
                                        todo!("Peekable value turned bad!");
                                    }
                                }
                                TokenKind::Quote => {
                                    lexer.next();
                                    run = false;
                                }
                                _ => {
                                    let value = Self::parse_peekable(lexer).unwrap();
                                    values.push(value);
                                }
                            }
                        }
                    }

                    Some(Expr::Collection(values))
                }
                TokenKind::OpenCurly => {
                    let mut values: Vec<Expr> = Vec::new();
                    let mut run = true;
                    while run {
                        if let Some(next_token_peeked) = lexer.peek() {
                            match next_token_peeked.kind {
                                TokenKind::Sym => {
                                    if let Some(next_token) = lexer.next() {
                                        values.push(Expr::PropReference(next_token.text));
                                    } else {
                                        todo!("Peekable value turned bad!");
                                    }
                                },
                                TokenKind::Quote => {
                                    let value_expression = Expr::parse_peekable(lexer).unwrap();
                                    values.push(value_expression);
                                },
                                TokenKind::CloseCurly => {
                                    lexer.next();
                                    run = false;
                                },
                                _ => {
                                    let value = Self::parse_peekable(lexer).unwrap();
                                    values.push(value);
                                }
                            }
                        }
                    }

                    Some(Expr::Collection(values))
                }
                TokenKind::Keyword => {
                    let keyword = Keyword::from(&token.text);
                    match keyword {
                        Keyword::ComponentChildren => Some(Expr::PropReference(token.text)),
                        Keyword::DataReference(reference_name) => Some(Expr::DataRaference(reference_name)),
                        _ => todo!("Unexpected keyword {:?}", token)
                    }
                }
                TokenKind::Space => Some(Expr::Value(token.text)),
                _ => todo!("Unexpected keyword {:?}", token),
            };
        };
        None
    }

    fn parse(lexer: impl Iterator<Item = Token>) -> Option<Self> {
        Self::parse_peekable(&mut lexer.peekable())
    }
}

#[derive(Debug)]
struct Component {
    name: String,
    args: Vec<String>,
    body: Expr,
}

#[derive(Debug)]
struct Data {
    name: String,
    value: Expr
}

enum DataValue {
    Literal(Expr),
    Expression(Vec<Data>)
}

impl DataValue {
    fn parse_peekable(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Self {
        // match lexer.peek() {
        //     TokenKind::Quote => {
        //         let value = Expr::parse_peekable(lexer);
        //         DataValue::Literal(value)
        //     }
        // }

        DataValue::Expression(Vec::new())
    }
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "data {} = {}", self.name, self.value)
    }
}

impl Data {
    fn parse_peekable(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Self {
        if let Some(token) = lexer.next() {
            match token.kind {
                TokenKind::Keyword => {
                    if token.text == Keyword::Data {
                        Parser::trim_space(lexer);
                        match lexer.next() {
                            Some(Token{kind: TokenKind::Sym, text: variable_name, .. }) => {
                                Parser::trim_space(lexer);
                                match lexer.next() {
                                    Some(Token{ kind: TokenKind::Equals, .. }) => {
                                        Parser::trim_space(lexer);
                                        let value = Expr::parse_peekable(lexer).unwrap();
                                        Data {
                                            name: variable_name.to_string(),
                                            value
                                        }
                                    },
                                    Some(invalid_token) => todo!("Unexpected token {:?}, expected '='", invalid_token),
                                    _ => todo!("Unexpected EoF")        
                                }
                            },
                            Some(invalid_token) => todo!("Unexpected token {:?}, expected data name", invalid_token),
                            _ => todo!("Unexpected EoF")
                        }
                    } else {
                        todo!("Unexpected keyword {:?} expected keyword '{}'", token.text, Keyword::Data.value());   
                    }
                },
                _ => todo!("Unexpected token {:?} expected keyword '{}'", token.text, Keyword::Data.value()),
            }
        } else {
            todo!("Unexpected EoF")
        }
    }

    // fn compile(&self, data: &DataMap, props: &PropsMap, components: &ComponentsMap) -> String {
    //     match self.value {
    //         Expr::Collection(values) => {
                
    //         },
    //     }
    // }
}


// impl DataValue {
//     fn parse_peekable(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Self {
//         match lexer.peek() {
//             Some(Token{ kind: TokenKind::Quote, .. }) => {
//                 let value = Expr::parse_peekable(lexer).unwrap();
//                 DataValue::Literal(value)
//             },
//             Some(Token{ kind: TokenKind::OpenCurly, .. }) => {
//                 lexer.next();
//                 let mut values: Vec<Data> = Vec::new();
//                 while Some(token) = lexer.next_if(|t| t.kind != TokenKind::CloseCurly) {
//                     let value = Data::parse_peekable(lexer);
//                     values.push(value);
//                 }
//                 lexer.next();
//                 DataValue::Expression(values)
//             }
//             Some(invalid_token) => todo!("Value token {:?} not yet supported!", invalid_token),
//             _ => todo!("Unexpected EoF")
//         }
//     }
// }

// impl fmt::Display for DataValue {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             Self::Literal(value) => write!(f, "\"{}\"", value)
//         }
//     }
// }

// data name = "John"
// Data { name: "Name", value: Value::Literal("John") }

/*
    data name = {
        "John"
    }
*/

//Data { name: "Name", value: Value::Expression([Value::Literal("John")]) }

/*
    data name = {
        data first_name = "John"

        data:first_name
    }
*/

//Data { name: "Name", value: Value::Expression([Value::Variable(Data { name: "first_name", value: Value::Literal("John") }), Value::Reference("first_name")]) }

impl Component {
    fn compile(&self, data: &DataMap, props: &PropsMap, components: &ComponentsMap) -> String {
        for arg in &self.args {
            if !props.contains_key(arg) {
                todo!(
                    "Error compiling component \"{}\", missing prop \"{}\"",
                    self.name,
                    arg
                );
            }
        }

        self.body.compile(data, props, components)
    }

    fn parse_peekable(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Self {
        if let Some(token) = lexer.next() {
            match token.kind {
                TokenKind::Keyword => {
                    if token.text == Keyword::Component {
                        Parser::trim_space(lexer);
                        let mut args: Vec<String> = Vec::new();
                        match lexer.next() {
                            Some(Token {
                                kind: TokenKind::Sym,
                                text: component_name,
                                ..
                            }) => {
                                Parser::trim_space(lexer);
                                match lexer.next() {
                                    Some(Token {
                                        kind: TokenKind::OpenParen,
                                        ..
                                    }) => {
                                        Parser::trim_space(lexer);

                                        while let Some(argument_token) =
                                            lexer.next_if(|t| t.kind != TokenKind::CloseParen)
                                        {
                                            Parser::trim_space(lexer);
                                            match argument_token.kind {
                                                TokenKind::Sym => args.push(argument_token.text.to_string()),
                                                TokenKind::Comma => {},
                                                _ => todo!("Unexpected token {:?}, expected component argument list", argument_token)
                                            }
                                        }

                                        // Remove close paren
                                        lexer.next();
                                        Parser::trim_space(lexer);

                                        match lexer.next() {
                                            Some(Token {
                                                kind: TokenKind::Equals,
                                                ..
                                            }) => {
                                                Parser::trim_space(lexer);

                                                let body = Expr::parse_peekable(lexer).unwrap();
                                                return Component {
                                                    name: component_name,
                                                    args,
                                                    body,
                                                };
                                            }
                                            Some(invalid_token) => {
                                                todo!("Unexpected token {:?}, expected component definition", invalid_token)
                                            }
                                            _ => {
                                                todo!("Unexpected EoF")
                                            }
                                        }
                                    }
                                    Some(Token {
                                        kind: TokenKind::Equals,
                                        ..
                                    }) => {
                                        Parser::trim_space(lexer);

                                        let body = Expr::parse_peekable(lexer).unwrap();
                                        return Component {
                                            name: component_name,
                                            args,
                                            body,
                                        };
                                    }
                                    Some(invalid_token) => {
                                        todo!("Unexpected token {:?}, expected component argument list or component definition", invalid_token)
                                    }
                                    _ => {
                                        todo!("Unexpected EoF")
                                    }
                                }
                            }
                            Some(invalid_token) => todo!(
                                "Unexpected token {:?}, expected a component name",
                                invalid_token
                            ),
                            _ => todo!("Unexpected EoF"),
                        }
                    } else {
                        todo!(
                            "Unexpected keyword '{}' expected keyword 'comp'",
                            token.text
                        )
                    }
                }
                _ => todo!("Unexpected token {:?} expected keyword 'comp'", token.text),
            }
        } else {
            todo!("Unexpected EoF")
        }
    }

    fn parse(lexer: impl Iterator<Item = Token>) -> Self {
        Self::parse_peekable(&mut lexer.peekable())
    }
}

impl fmt::Display for Component {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "comp {}", self.name)?;
        if self.args.len() > 0 {
            write!(f, "(")?;
            for (i, arg) in self.args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg)?;
            }
            write!(f, ")")?;
        }
        write!(f, " = {}", self.body)
    }
}

#[derive(Debug, PartialEq)]
enum TokenKind {
    Sym,
    OpenParen,
    CloseParen,
    Keyword,
    Comma,
    Equals,
    GreaterThen,
    LessThen,
    Slash,
    OpenCurly,
    CloseCurly,
    OpenBracket,
    CloseBracket,
    Quote,
    Space,
}

#[derive(Debug, PartialEq)]
enum Keyword {
    None,
    Component,
    ComponentChildren,
    Data,
    DataReference(String)
}

impl PartialEq<String> for Keyword {
    fn eq(&self, other: &String) -> bool {
        self.value() == *other
    }
}

impl PartialEq<Keyword> for &String {
    fn eq(&self, other: &Keyword) -> bool {
        **self == other.value()
    }
}

impl PartialEq<Keyword> for String {
    fn eq(&self, other: &Keyword) -> bool {
        **self == other.value()
    }
}

impl From<&String> for Keyword {
    fn from(item: &String) -> Self {
        if let Some((keyword, sub_data)) = item.split_once(':') {
            
            if Self::Component == keyword.to_string() {
                if sub_data.is_empty() {
                    return Self::Component;
                }
                return Self::ComponentChildren;
            }
    
            if Self::Data == keyword.to_string() {
                if sub_data.is_empty() {
                    return Self::Data;
                }
                return Self::DataReference(sub_data.to_string());
            }
        } else {
            if Self::Component == *item {
                return Self::Component;
            }
            if Self::Data == *item {
                return Self::Data;
            }
        }
        
        Keyword::None
    }
}

impl Keyword {
    fn value(&self) -> String {
        match self {
            Keyword::Component => "comp".to_string(),
            Keyword::ComponentChildren => "comp:children".to_string(),
            Keyword::Data => "data".to_string(),
            Keyword::DataReference(data_name) => "data:".to_string() + data_name,
            _ => todo!("Keyword none should not be used!"),
        }
    }
}

#[derive(Debug)]
struct Token {
    text: String,
    kind: TokenKind,
    lineNo: i32,
    columnNo: i32,
}

struct Lexer<Chars: Iterator<Item = char>> {
    chars: Peekable<Chars>,
    lineNo: i32,
    columnNo: i32,
}

impl<Chars: Iterator<Item = char>> Lexer<Chars> {
    fn from_iter(chars: Chars) -> Self {
        Self {
            chars: chars.peekable(),
            lineNo: 1,
            columnNo: 1,
        }
    }
}

impl<Chars: Iterator<Item = char>> Iterator for Lexer<Chars> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        while let Some(_) = self.chars.next_if(|x| *x == '\n') {
            self.lineNo += 1;
            self.columnNo = 0;
        }

        if let Some(x) = self.chars.next() {
            let mut text = String::new();
            text.push(x);
            self.columnNo += 1;
            match x {
                '(' => Some(Token {
                    kind: TokenKind::OpenParen,
                    text,
                    lineNo: self.lineNo,
                    columnNo: self.columnNo,
                }),
                ')' => Some(Token {
                    kind: TokenKind::CloseParen,
                    text,
                    lineNo: self.lineNo,
                    columnNo: self.columnNo,
                }),
                '{' => Some(Token {
                    kind: TokenKind::OpenCurly,
                    text,
                    lineNo: self.lineNo,
                    columnNo: self.columnNo,
                }),
                '}' => Some(Token {
                    kind: TokenKind::CloseCurly,
                    text,
                    lineNo: self.lineNo,
                    columnNo: self.columnNo,
                }),
                '[' => Some(Token {
                    kind: TokenKind::OpenBracket,
                    text,
                    lineNo: self.lineNo,
                    columnNo: self.columnNo,
                }),
                ']' => Some(Token {
                    kind: TokenKind::CloseBracket,
                    text,
                    lineNo: self.lineNo,
                    columnNo: self.columnNo,
                }),
                '<' => Some(Token {
                    kind: TokenKind::LessThen,
                    text,
                    lineNo: self.lineNo,
                    columnNo: self.columnNo,
                }),
                '>' => Some(Token {
                    kind: TokenKind::GreaterThen,
                    text,
                    lineNo: self.lineNo,
                    columnNo: self.columnNo,
                }),
                '"' => Some(Token {
                    kind: TokenKind::Quote,
                    text,
                    lineNo: self.lineNo,
                    columnNo: self.columnNo,
                }),
                '/' => Some(Token {
                    kind: TokenKind::Slash,
                    text,
                    lineNo: self.lineNo,
                    columnNo: self.columnNo,
                }),
                '=' => Some(Token {
                    kind: TokenKind::Equals,
                    text,
                    lineNo: self.lineNo,
                    columnNo: self.columnNo,
                }),
                ' ' => {
                    while let Some(x) = self.chars.next_if(|x| x.is_whitespace()) {
                        text.push(x);
                    }

                    let columnNo = self.columnNo;
                    self.columnNo += text.len() as i32;

                    Some(Token {
                        kind: TokenKind::Space,
                        text,
                        lineNo: self.lineNo,
                        columnNo: columnNo,
                    })
                }
                _ => {
                    while let Some(x) = self
                        .chars
                        .next_if(|x| x.is_alphanumeric() || *x == '-' || *x == ':' || *x == '_')
                    {
                        text.push(x);
                    }

                    let columnNo = self.columnNo;
                    self.columnNo += text.len() as i32;

                    match Keyword::from(&text) {
                        Keyword::Component => Some(Token {
                            kind: TokenKind::Keyword,
                            text,
                            lineNo: self.lineNo,
                            columnNo: columnNo,
                        }),
                        Keyword::ComponentChildren => Some(Token {
                            kind: TokenKind::Keyword,
                            text,
                            lineNo: self.lineNo,
                            columnNo: columnNo,
                        }),
                        Keyword::Data => Some(Token {
                            kind: TokenKind::Keyword,
                            text,
                            lineNo: self.lineNo,
                            columnNo: columnNo,
                        }),
                        Keyword::DataReference(_) => Some(Token {
                            kind: TokenKind::Keyword,
                            text,
                            lineNo: self.lineNo,
                            columnNo: columnNo,
                        }),
                        _ => Some(Token {
                            kind: TokenKind::Sym,
                            text,
                            lineNo: self.lineNo,
                            columnNo: columnNo,
                        }),
                    }
                }
            }
        } else {
            None
        }
    }
}

#[derive(Debug)]
enum ParseError {
    UnableToParse,
}

#[derive(Debug)]
struct ParseResult {
    components: ComponentsMap,
    data: DataMap,
    templates: Vec<Expr>,
}

impl fmt::Display for ParseResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (_, data) in &self.data {
            writeln!(f, "{}", data);
        }
        for (_, component) in &self.components {
            writeln!(f, "{}", component);
        }
        for template in &self.templates {
            writeln!(f, "{}", template);
        }
        Ok(())
    }
}

struct Parser {}

impl Parser {
    fn trim_space(lexer: &mut Peekable<impl Iterator<Item = Token>>) {
        while let Some(_) = lexer.next_if(|t| t.kind == TokenKind::Space) {}
    }

    fn parse_peekable(
        lexer: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> Result<ParseResult, ParseError> {
        let mut components: ComponentsMap = HashMap::new();
        let mut templates: Vec<Expr> = Vec::new();
        let mut data: DataMap = HashMap::new();

        while let Some(token) = lexer.peek() {
            match token {
                Token {
                    kind: TokenKind::Keyword,
                    text: keyword_name,
                    ..
                } => {
                    let keyword = Keyword::from(keyword_name);
                    match keyword {
                        Keyword::Component => {
                            let component = Component::parse_peekable(lexer);
                            components.insert(component.name.clone(), component);
                        }
                        Keyword::Data => {
                            let d = Data::parse_peekable(lexer);
                            data.insert(d.name.clone(), d);
                        }
                        _ => todo!("Missing parser for keyword '{}'", keyword_name),
                    }
                }
                _ => {
                    let template = Expr::parse_peekable(lexer).unwrap();
                    templates.push(template);
                }
            }
        }
        Ok(ParseResult {
            components,
            templates,
            data,
        })
    }

    fn parse(lexer: impl Iterator<Item = Token>) -> Result<ParseResult, ParseError> {
        Self::parse_peekable(&mut lexer.peekable())
    }
}

struct Compiler {}

impl Compiler {
    fn compile(ast: ParseResult) -> String {
        let mut result = String::new();

        for template in ast.templates {
            let props: PropsMap = HashMap::new();
            result.push_str(&template.compile(&ast.data, &props, &ast.components))
        }

        result
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    assert_ne!(args.len(), 1, "Missing file path argument");

    let source_file = Path::new(&args[1]);
    let file_name = source_file.file_stem().unwrap();

    let contents = fs::read_to_string(source_file).expect("Unable to open file at path");
    let mut lexer = Lexer::from_iter(contents.chars());
    let ast = Parser::parse(lexer).unwrap();
    let result = Compiler::compile(ast);

    let output_file_name = format!("{}.html", file_name.to_str().unwrap());
    let output_file = Path::new(&output_file_name);
    fs::write(output_file, result);
    println!("Compilation done, {}", output_file.to_str().unwrap());
}
