use error::LineColLocation;
use pest::{
    error::Error,
    error::ErrorVariant,
    error::{self},
    iterators::Pair,
    Parser,
};
use pest_derive::Parser;
use crate::runtime::Value;

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

type InnerNode<'a> = Box<ParserNode<'a>>;

// represents a node in the AST which the parser can construct.
// leibniz's grammar is defined in leibniz.pest, from which the parser is generated from.
// this enum acts as a mapping between that grammar and rust code
#[derive(Debug)]
pub enum ParserNode<'a> {
    Number(f64, bool),   // any number, either real or imaginary
    Identifier(&'a str), // any identifier, such as a variable name, function name, etc
    Operation(InnerNode<'a>, Operator, InnerNode<'a>), // an arithmetic operation with a left and right hand side
    Assignment(Vec<&'a str>, InnerNode<'a>), // re-assigning (possibly multiple) variables to a value
    FunctionCall(&'a str, Vec<ParserNode<'a>>), // a function call with an array of expressions as arguments
    Conditional(InnerNode<'a>, InnerNode<'a>, InnerNode<'a>), // a conditional with a predicate, true expression and false expression
    FunctionDeclaration(&'a str, Vec<&'a str>, InnerNode<'a>),
    VariableDeclaration(&'a str, InnerNode<'a>),
    Range(InnerNode<'a>, InnerNode<'a>, InnerNode<'a>), // any range with a lower bound, upper bound and a step
    Array(Vec<ParserNode<'a>>), // an array full of expressions
    Index(InnerNode<'a>, InnerNode<'a>),
    Loop(&'a str, InnerNode<'a>, InnerNode<'a>), // a loop construct that works on ranges and a named parameter
    Factorial(InnerNode<'a>),                    // factorial of an expression
    Tree(Vec<ParserNode<'a>>),                   // a tree of nodes
}

impl<'a> ParserNode<'a> {
    pub fn append_tree(&mut self, new_nodes: ParserNode<'a>) {
        if let ParserNode::Tree(nodes) = self {
            if let ParserNode::Tree(mut new_nodes) = new_nodes {
                nodes.append(&mut new_nodes);
            }
        } else {
            panic!("tried to append nodes to a non-tree node");
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
            left_val = ParserNode::Index(Box::new(left_val), Box::new(parse_expression(index_pairs[1].clone())));
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

    let right_val = parse_value(pairs[dropoff + 1].clone());
    ParserNode::Operation(Box::new(left_val), Operator::Power, Box::new(right_val))
}

fn parse_number(number: Pair<Rule>) -> ParserNode {
    let num = number.as_str();

    if num == "i" {
        return ParserNode::Number(1.0, true);
    }

    let imaginary = num.ends_with("i");
    let num_str = if imaginary {
        &num[0..num.len() - 1]
    } else {
        num
    };

    ParserNode::Number(num_str.parse().unwrap(), imaginary)
}

fn parse_identifier(identifier: Pair<Rule>) -> ParserNode {
    ParserNode::Identifier(identifier.as_str())
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

    let expressions = pairs.into_iter()
        .filter(|pair| pair.as_rule() == Rule::expression)
        .map(|pair| parse_expression(pair))
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
        .map(|pair| pair.as_str())
        .collect();

    let expression = parse_expression(pairs.last().unwrap().clone());

    ParserNode::Assignment(identifiers, Box::new(expression))
}

fn parse_var_decl(declaration: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(declaration);

    ParserNode::VariableDeclaration(
        pairs[0].as_str(),
        Box::new(parse_tree_or_expression(pairs[2].clone())),
    )
}

fn parse_param_list(param_list: Pair<Rule>) -> Vec<&str> {
    param_list
        .into_inner()
        .filter(|pair| pair.as_rule() == Rule::identifier)
        .map(|pair| pair.as_str())
        .collect::<Vec<&str>>()
}

fn parse_func_decl(declaration: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(declaration);

    ParserNode::FunctionDeclaration(
        pairs[0].as_str(),
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
        .map(|pair| parse_expression(pair))
        .collect();

    ParserNode::FunctionCall(func_name, arguments)
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

fn parse_conditional<'a>(conditional: Pair<'a, Rule>, predicate: ParserNode<'a>) -> ParserNode<'a> {
    let pairs = pairs_to_vec(conditional);
    let true_branch = parse_tree_or_expression(pairs[0].clone());
    let false_branch = parse_tree_or_expression(pairs[2].clone());

    ParserNode::Conditional(
        Box::new(predicate),
        Box::new(true_branch),
        Box::new(false_branch),
    )
}

fn parse_loop(rloop: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(rloop);
    let range = parse_range(pairs[1].clone());
    let expression = parse_tree_or_expression(pairs[2].clone());

    ParserNode::Loop(pairs[0].as_str(), Box::new(range), Box::new(expression))
}

fn parse_range(range: Pair<Rule>) -> ParserNode {
    let pairs = pairs_to_vec(range);
    let lower_bound = parse_expression(pairs[1].clone());
    let upper_bound = parse_expression(pairs[3].clone());
    let step = parse_expression(pairs[5].clone());

    ParserNode::Range(Box::new(lower_bound), Box::new(upper_bound), Box::new(step))
}

fn parse_action(action: Pair<Rule>) -> ParserNode {
    match action.as_rule() {
        Rule::func_decl => parse_func_decl(action),
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
        if positives.iter().any(|rule| rule == &Rule::tree) {
            "expected a } here"
        } else if positives.iter().any(|rule| rule == &Rule::comma) {
            "expected a , here"
        } else if positives.iter().any(|rule| rule == &Rule::rsquarb) {
            "expected a ] here"
        } else if positives.iter().any(|rule| rule == &Rule::bar) {
            "expected a | here"
        } else {
            match positives[0] {
                Rule::value => "expected proper value",
                Rule::identifier => "expected a variable name here",
                Rule::expression => "expected an expression",
                Rule::equals => "expected = here after let declaration",
                Rule::rarrow => "expected => here",
                Rule::lcurlb => "expected tree or expression",
                Rule::range => "expected a range here",
                Rule::EOI => "unknown token",
                _ => "unexpected character",
            }
        }
    } else {
        unreachable!()
    };

    let highlight = " ".repeat(error_pos.1 - 1) + "^";
    let line = input.split("\n").collect::<Vec<&str>>()[error_pos.0 - 1];
    format!(
        "({}, {}): {}\n--> {}\n    {}",
        error_pos.0, error_pos.1, error_msg, line, highlight
    )
}
