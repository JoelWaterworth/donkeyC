use nom::{digit, alpha, alphanumeric, IResult};
use nom;
use std::str;
use std::str::FromStr;
use std::collections::HashMap;

pub type TokenizedContent = HashMap<String, Function>;

#[derive(Clone, Debug)]
pub struct Function {
    args:   Vec<Type>,
    ret:    Type,
    body:   Option<Value>
}

#[derive(Clone, Debug)]
pub enum Global {
    Function,
}

#[derive(Clone, Debug)]
pub enum Value {
    Integral(i64),
    Variable(String, Type),
    FuncCall(String, Vec<Value>, Type),
    OpCall(Operator, Type),
}

#[derive(Clone, Debug)]
pub enum Type {
    I64,
    F64,
    Undef,
    Integral,
    Fractional,
}

#[derive(Clone, Debug)]
enum Operator {
    Mul(Box<(Value, Value)>),
    Div(Box<(Value, Value)>),
    Add(Box<(Value, Value)>),
    Sub(Box<(Value, Value)>),
}

pub fn lexer<'a>(content: &'a String) {
    let x = global(content.as_bytes());
    println!("{:?}", x);
}

named!(global<TokenizedContent>, fold_many0!(function_parser,HashMap::new(), |mut acc: TokenizedContent, (name, func): (String, Function) | {
        acc.insert(name, func);
        acc
    }
));

named!(function_parser<(String, Function)>, do_parse!(
    name: alias >>
    tag!(":") >>
    arg_types: ws!(delimited!(
            tag!("("),
            many0!( do_parse!(
                ty: type_parse >>
                paren_ending >>
                (ty))),
            tag!(")") )) >>
    ws!(tag!("->")) >>
    ret: ws!(type_parse) >>
    entry: alias >>
    args: many0!(alias) >>
    ws!(tag!(":=")) >>
    val: apply!(body, &make_variable(args, arg_types.clone())) >>
    ({
        if name != entry {
            panic!("{} and {} should have the same name.", name, entry)
        }
        (name, Function { args: arg_types, ret, body: Some(val) })
    })
));

named_args!(body<'a>(vars: &'a HashMap<String, Type>)<Value>, do_parse!(
    init: apply!(value_parse, vars) >>
    res: fold_many0!(
        pair!( operators, apply!(value_parse, vars) ),
        init,
        |acc, (op, val): (&[u8], Value)| {
            Value::OpCall(operator(op, acc, val), Type::Undef)
        }) >>
    (res)
));

fn make_variable(names: Vec<String>, types: Vec<Type>) -> HashMap<String, Type> {
    let mut variables = HashMap::new();
    for (k, v) in names.iter().zip(types) {
        variables.insert(k.clone(), v.clone());
    }
    variables
}

named!(paren_ending, ws!(alt!( tag!(",") | tag!("") )));

named_args!(value_parse<'a>(vars: &'a HashMap<String, Type>)<Value>,
    ws!(alt_complete!(
        number |
        apply!(func_call, vars) |
        apply!(variable, vars)
    )
));

named_args!(variable<'a>(vars: &'a HashMap<String, Type>)<Value>, do_parse!(
    var: alias >>
    ({
        let v = vars.get(&var);
        match v {
            Some(v) => Value::Variable( var.clone(), v.clone()),
            None => panic!("{} does not exist", var),
        }
    })
));

named_args!(func_call<'a>(vars: &'a HashMap<String, Type>)<Value>, do_parse!(
    func: alias >>
    args: delimited!(
        tag!("("),
        many0!( do_parse!(
                val: apply!(value_parse, vars) >>
                paren_ending >>
                (val))),
        tag!(")")) >>
    (Value::FuncCall(func, args, Type::Undef))
));

named!(type_parse<Type>, do_parse!(
    typ: ws!(alt!(
            tag!("I64") |
            tag!("F64")
            )) >>
    (match str::from_utf8(typ).unwrap() {
        "I64" => Type::I64,
        "F64" => Type::F64,
        _ => panic!("{:?} is not known", typ),
    })
));

named!(alias<String>,map_res!(
    map_res!(
        ws!(alpha),
        str::from_utf8
    ),
    FromStr::from_str
));

named!(number<Value>, do_parse!(
    num: integral >>
    (Value::Integral(num))
));

named!(integral<i64>,
    map_res!(
      map_res!(
        ws!(digit),
        str::from_utf8
      ),
      FromStr::from_str
));

named!(operators, ws!(alt!(
    tag!("+") |
    tag!("-") |
    tag!("*") |
    tag!("/")
)));

fn operator(x: &[u8], left: Value, right: Value) -> Operator {
    match x[0] as char {
        '+' => Operator::Add(Box::new((left, right))),
        '-' => Operator::Sub(Box::new((left, right))),
        'x' => Operator::Mul(Box::new((left, right))),
        '/' => Operator::Div(Box::new((left, right))),
        _   => panic!("operator {} not found", x[0])
    }
}

fn analysis(content: &mut TokenizedContent) {
    for (k, v) in content.iter_mut() {
        match v.body {
            Some(x) => value_analysis(x),
            None => {}
        }
    }
}