use error::LineColLocation;
use pest::{
    error::Error,
    error::ErrorVariant,
    error::{self},
    iterators::Pair,
    Parser,
};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "leibniz.pest"]
pub struct LeibnizParser;

#[derive(Debug)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    Modulo,
    Equals,
    GreaterThan,
    LessThan,
    GreaterThanOrEquals,
    LessThanOrEquals,
}

#[derive(Debug, Clone)]
pub enum TypeConstraint {
    Real,
    Imaginary,
    Complex,
    Integer,
    Natural,
    Array,
    Custom(String),
}

type InnerNode = Box<ParserNode>;

// represents a node in the AST which the parser can construct.
// leibniz's grammar is defined in leibniz.pest, from which the parser is generated from.
// this enum acts as a mapping between that grammar and rust code
#[derive(Debug)]
pub enum ParserNode {
    Number(f64, bool),                         // any number, either real or imaginary
    Identifier(String), // any identifier, such as a variable name, function name, etc
    Operation(InnerNode, Operator, InnerNode), // an arithmetic operation with a left and right hand side
    Assignment(Vec<String>, InnerNode), // re-assigning (possibly multiple) variables to a value
    FunctionCall(String, Vec<ParserNode>), // a function call with an array of expressions as arguments
    Conditional(InnerNode, InnerNode, InnerNode), // a conditional with a predicate, true expression and false expression
    FunctionDeclaration(String, Vec<String>, InnerNode),
    TypeDeclaration(String, Vec<(String, TypeConstraint)>),
    VariableDeclaration(String, InnerNode),
    Range(InnerNode, InnerNode, InnerNode, bool), // any range with a lower bound, upper bound and a step plus an indication to what direction it is
    Array(Vec<ParserNode>),                       // an array full of expressions
    Index(InnerNode, InnerNode),
    Loop(String, InnerNode, InnerNode), // a loop construct that works on ranges and a named parameter
    Factorial(InnerNode),               // factorial of an expression
    Access(InnerNode, String),          // accessing an expression's field
    String(String),
    Tree(Vec<ParserNode>), // a tree of nodes
}

impl ParserNode {
    pub fn append_tree(&mut self, new_nodes: ParserNode) {
        if let ParserNode::Tree(nodes) = self {
            if let ParserNode::Tree(mut new_nodes) = new_nodes {
                nodes.append(&mut new_nodes);
            }
        } else {
            panic!("tried to append nodes to a non-tree node");
        }
    }

    pub fn destructure_range(self) -> (ParserNode, ParserNode, ParserNode, bool) {
        match self {
            ParserNode::Range(lower_bound, upper_bound, step, going_down) => {
                (*lower_bound, *upper_bound, *step, going_down)
            }
            _ => panic!("failed to destructure range node. this should never happen"),
        }
    }
}

pub fn parse_leibniz_file(file: &str) -> Result<ParserNode, String> {
    let root = match LeibnizParser::parse(Rule::file, file) {
        Ok(mut pair) => pair.next().unwrap(),
        Err(pair) => {
            return Err(get_error_message(file, pair));
        }
    };

    let mut nodes = Vec::new();

    for pair in root.into_inner() {
        nodes.push(match pair.as_rule() {
            Rule::func_decl => parse_func_decl(pair),
            Rule::type_decl => parse_type_decl(pair),
            Rule::var_decl => parse_var_decl(pair),
            Rule::tree | Rule::expression => parse_tree_or_expression(pair),
            Rule::assignment => parse_assignment(pair),
            Rule::rloop => parse_loop(pair),
            Rule::EOI => break,
            _ => unreachable!(),
        });
    }

    Ok(ParserNode::Tree(nodes))
}

fn parse_value(value: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(value);

    let mut left_val = match pairs[0].as_rule() {
        Rule::number => parse_number(pairs[0].clone()),
        Rule::identifier => parse_identifier(pairs[0].clone()),
        Rule::expression => parse_expression(pairs[0].clone()), // parenthesis
        Rule::func_call => parse_func_call(pairs[0].clone()),
        Rule::array => parse_array(pairs[0].clone()),
        Rule::rloop => parse_loop(pairs[0].clone()),
        Rule::string => parse_string(pairs[0].clone()),
        _ => {
            println!("Unknown parser pair: {:#?}", pairs);
            unreachable!()
        }
    };

    if pairs.len() == 1 {
        return left_val; // no power operator on the right side or index, so return left side only
    }

    let mut dropoff = 1;

    for i in 1..pairs.len() {
        let pair = &pairs[i];

        if pair.as_rule() == Rule::index {
            let index_pairs = pairs_to_vec(pair.clone());
            left_val = ParserNode::Index(
                Box::new(left_val),
                Box::new(parse_expression(index_pairs[1].clone())),
            );
            dropoff += 1;
        } else {
            break;
        }
    }

    if dropoff == pairs.len() {
        return left_val;
    }

    for i in dropoff..pairs.len() {
        let pair = &pairs[i];

        if pair.as_rule() == Rule::fact {
            left_val = ParserNode::Factorial(Box::new(left_val));
            dropoff += 1;
        } else {
            break;
        }
    }

    if dropoff == pairs.len() {
        return left_val;
    }

    let mut value = if pairs[dropoff].as_rule() == Rule::pow {
        let val_at = dropoff + 1;
        dropoff += 2;

        ParserNode::Operation(
            Box::new(left_val),
            Operator::Power,
            Box::new(parse_value(pairs[val_at].clone())),
        )
    } else {
        left_val
    };

    if dropoff >= pairs.len() {
        return value;
    }

    let accesses = &pairs[dropoff..];
    for access in accesses {
        value = ParserNode::Access(Box::new(value), access.as_str().to_string());
    }

    value
}

fn parse_number(number: Pair<Rule>) -> ParserNode {
    let num = number.as_str();

    if num == "i" {
        return ParserNode::Number(1.0, true);
    }

    let imaginary = num.ends_with('i');
    let num_str = if imaginary {
        &num[0..num.len() - 1]
    } else {
        num
    };

    ParserNode::Number(num_str.parse().unwrap(), imaginary)
}

fn parse_identifier(identifier: Pair<Rule>) -> ParserNode {
    ParserNode::Identifier(identifier.as_str().to_owned())
}

fn parse_term(term: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(term);
    let left_term = parse_value(pairs[0].clone());

    if pairs.len() == 1 {
        return left_term;
    }

    let operator = parse_operator(pairs[1].clone());
    let right_term = parse_term(pairs[2].clone());

    ParserNode::Operation(Box::new(left_term), operator, Box::new(right_term))
}

fn parse_array(array: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(array);

    let expressions = pairs
        .into_iter()
        .filter(|pair| pair.as_rule() == Rule::expression)
        .map(parse_expression)
        .collect();

    ParserNode::Array(expressions)
}

fn parse_expression(expression: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(expression);

    if pairs[0].as_rule() == Rule::negation_expression {
        return parse_negation_expression(pairs[0].clone());
    }

    if pairs.len() == 1 {
        return parse_term(pairs[0].clone());
    }

    let mut operation = parse_term(pairs[0].clone());
    let conditional_offset = if pairs.last().unwrap().as_rule() == Rule::conditional {
        1
    } else {
        0
    };

    for i in (0..pairs.len() - 1 - conditional_offset).step_by(2) {
        let operator = parse_operator(pairs[i + 1].clone());
        let right = parse_term(pairs[i + 2].clone());
        operation = ParserNode::Operation(Box::new(operation), operator, Box::new(right));
    }

    if conditional_offset > 0 {
        return parse_conditional(pairs[pairs.len() - 1].clone(), operation);
    }

    operation
}

fn parse_negation_expression(expression: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(expression);

    ParserNode::Operation(
        Box::new(parse_expression(pairs[0].clone())),
        Operator::Multiply,
        Box::new(ParserNode::Number(-1.0, false)),
    )
}

fn parse_assignment(assignment: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(assignment);

    let identifiers = pairs
        .iter()
        .filter(|pair| pair.as_rule() == Rule::identifier)
        .map(|pair| pair.as_str().to_owned())
        .collect();

    let expression = parse_expression(pairs.last().unwrap().clone());

    ParserNode::Assignment(identifiers, Box::new(expression))
}

fn parse_type_decl(declaration: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(declaration);

    let type_name = pairs[0].as_str();

    let properties = pairs
        .iter()
        .filter(|pair| pair.as_rule() == Rule::param)
        .map(|pair| parse_param(pair.clone()))
        .collect();

    ParserNode::TypeDeclaration(type_name.to_owned(), properties)
}

fn parse_param(constraint: Pair<Rule>) -> (String, TypeConstraint) {
    let pairs = pairs_to_vec(constraint);

    let name = pairs[0].as_str().to_owned();

    let constraint = match pairs.len() {
        3 => pairs[2].as_str(),
        _ => "real",
    };

    let constraint = match constraint {
        "real" => TypeConstraint::Real,
        "imaginary" => TypeConstraint::Imaginary,
        "complex" => TypeConstraint::Complex,
        "integer" => TypeConstraint::Integer,
        "natural" => TypeConstraint::Natural,
        "array" => TypeConstraint::Array,
        _ => TypeConstraint::Custom(constraint.to_owned()),
    }
    .to_owned();

    (name, constraint)
}

fn parse_var_decl(declaration: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(declaration);

    ParserNode::VariableDeclaration(
        pairs[0].as_str().to_owned(),
        Box::new(parse_tree_or_expression(pairs[2].clone())),
    )
}

fn parse_param_list(param_list: Pair<Rule>) -> Vec<String> {
    param_list
        .into_inner()
        .filter(|pair| pair.as_rule() == Rule::identifier)
        .map(|pair| pair.as_str().to_owned())
        .collect::<Vec<String>>()
}

fn parse_func_decl(declaration: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(declaration);

    ParserNode::FunctionDeclaration(
        pairs[0].as_str().to_owned(),
        parse_param_list(pairs[1].clone()),
        Box::new(parse_tree_or_expression(pairs[3].clone())),
    )
}

fn parse_func_call(func_call: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(func_call);
    let func_name = pairs[0].as_str();
    let arguments = pairs[1]
        .clone()
        .into_inner()
        .filter(|pair| pair.as_rule() == Rule::expression)
        .map(parse_expression)
        .collect();

    ParserNode::FunctionCall(func_name.to_owned(), arguments)
}

fn parse_tree_or_expression(tree: Pair<Rule>) -> ParserNode {
    let mut nodes = Vec::new();

    if tree.as_rule() == Rule::expression {
        return parse_expression(tree);
    }

    let pairs = pairs_to_vec(tree);

    for pair in pairs
        .into_iter()
        .filter(|pair| pair.as_str() != "{" && pair.as_str() != "}")
    {
        nodes.push(parse_action(pair));
    }

    ParserNode::Tree(nodes)
}

fn parse_string(string: Pair<Rule>) -> ParserNode {
    let string = string.as_str().as_bytes();
    let mut content = String::with_capacity(string.len());

    let mut i = 1;
    while i < string.len() - 1 {
        if string[i] as char == '\\' {
            match string[i + 1] as char {
                '\\' => {
                    i += 1;
                    content.push('\\');
                }
                'n' => {
                    i += 1;
                    content.push('\n');
                }
                't' => {
                    i += 1;
                    content.push('\t');
                }
                'r' => {
                    i += 1;
                    content.push('\r');
                }
                '"' => {
                    i += 1;
                    content.push('"');
                }
                _ => {}
            }
        } else {
            content.push(string[i] as char);
        }

        i += 1;
    }

    ParserNode::String(content)
}

fn parse_conditional<'a>(conditional: Pair<'a, Rule>, predicate: ParserNode) -> ParserNode {
    let pairs = pairs_to_vec(conditional);
    let true_branch = parse_tree_or_expression(pairs[1].clone());
    let false_branch = parse_tree_or_expression(pairs[3].clone());

    ParserNode::Conditional(
        Box::new(predicate),
        Box::new(true_branch),
        Box::new(false_branch),
    )
}

fn parse_loop(rloop: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(rloop);
    let range = parse_range(pairs[1].clone());
    let expression = parse_tree_or_expression(pairs[3].clone());

    ParserNode::Loop(
        pairs[0].as_str().to_owned(),
        Box::new(range),
        Box::new(expression),
    )
}

fn parse_range(range: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(range);
    let lower_bound = parse_expression(pairs[1].clone());
    let upper_bound = parse_expression(pairs[3].clone());
    let going_down = pairs[5].as_rule() == Rule::sub;

    let step = if going_down {
        parse_expression(pairs[6].clone())
    } else {
        parse_expression(pairs[5].clone())
    };

    ParserNode::Range(
        Box::new(lower_bound),
        Box::new(upper_bound),
        Box::new(step),
        going_down,
    )
}

fn parse_action(action: Pair<Rule>) -> ParserNode {
    match action.as_rule() {
        Rule::func_decl => parse_func_decl(action),
        Rule::type_decl => parse_type_decl(action),
        Rule::var_decl => parse_var_decl(action),
        Rule::rloop => parse_loop(action),
        Rule::assignment => parse_assignment(action),
        Rule::tree | Rule::expression => parse_tree_or_expression(action),
        _ => unreachable!(),
    }
}

fn parse_operator(operator: Pair<Rule>) -> Operator {
    match operator.as_rule() {
        Rule::add => Operator::Add,
        Rule::sub => Operator::Subtract,
        Rule::mul => Operator::Multiply,
        Rule::div => Operator::Divide,
        Rule::pow => Operator::Power,
        Rule::abs => Operator::Modulo,
        Rule::eqequals => Operator::Equals,
        Rule::grt => Operator::GreaterThan,
        Rule::lst => Operator::LessThan,
        Rule::gre => Operator::GreaterThanOrEquals,
        Rule::lse => Operator::LessThanOrEquals,
        _ => unreachable!(),
    }
}

fn pairs_to_vec(pair: Pair<Rule>) -> Vec<Pair<Rule>> {
    pair.into_inner().collect::<Vec<Pair<Rule>>>()
}

fn get_error_message(input: &str, error: Error<Rule>) -> String {
    let error_pos = match error.line_col {
        LineColLocation::Pos(pos) => pos,
        LineColLocation::Span(line, col) => (line.0, col.0), // this never gets reached, anyways
    };

    let error_msg = if let ErrorVariant::ParsingError {
        positives,
        negatives: _,
    } = error.variant
    {
        let fp = positives[0];

        if fp == Rule::EOI {
            "unexpected character here"
        } else if fp == Rule::identifier {
            "expected an identifier here"
        } else if positives.iter().any(|rule| *rule == Rule::comma) {
            if positives.iter().any(|rule| *rule == Rule::rparen) {
                "expected , or ) here"
            } else if positives.iter().any(|rule| *rule == Rule::rsquarb) {
                "expected , or ] here"
            } else {
                "expected , here"
            }
        } else if fp == Rule::equals {
            if positives.len() > 1 && positives[1] == Rule::param_list {
                "expected = here, or ( if this is a function"
            } else {
                "expected = here"
            }
        } else if fp == Rule::lsquarb {
            "expected a [ here"
        } else if fp == Rule::sub {
            "expected expression or -expression here for range step"
        } else if fp == Rule::rarrow {
            "expected => here"
        } else if positives
            .iter()
            .any(|rule| *rule == Rule::expression || *rule == Rule::value || *rule == Rule::file)
        {
            if positives.iter().any(|rule| *rule == Rule::rcurlb) {
                "expected } here"
            } else {
                "expected an expression here"
            }
        } else if positives.iter().any(|rule| *rule == Rule::dotdot) {
            "expected a .. here"
        } else if positives.iter().any(|rule| *rule == Rule::rsquarb) {
            "expected ] here"
        } else {
            println!("POSITIVES: {:?}", positives);
            "failed to create error message. this should not happen"
        }
    } else {
        unreachable!()
    };

    let highlight = " ".repeat(error_pos.1 - 1) + "^";
    let line = input.split('\n').collect::<Vec<&str>>()[error_pos.0 - 1];
    format!(
        "({}, {}): {}\n--> {}\n    {}",
        error_pos.0, error_pos.1, error_msg, line, highlight
    )
}
