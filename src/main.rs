#[macro_use]
extern crate nom;
extern crate llvm;
extern crate libc;

use nom::{digit, alpha, alphanumeric};

use llvm::*;

use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::str;
use std::str::FromStr;

#[derive(Debug)]
struct AliasAndFunctions<'a>{
    values: HashMap<String, &'a Value>,
    functions: HashMap<String, FunctionIR<'a>>,
}

impl<'a> AliasAndFunctions<'a> {
    pub fn new() -> AliasAndFunctions<'a> {
        AliasAndFunctions { values: HashMap::new(), functions: HashMap::new() }
    }
    pub fn insert_func(&mut self, name: String, val: FunctionIR<'a>) -> Option<FunctionIR<'a>> {
        self.functions.insert(name, val)
    }

    pub fn add_vals(&mut self, mut vals: Vec<(String, &'a Value)>) {
        for (k,v) in vals.drain(0..) {
            self.values.insert(k,v);
        }
    }

    pub fn get_function(&self, name: &String) -> &FunctionIR<'a> {
        let func = self.functions.get(name);
        match func {
            Some(x) => x,
            None => panic!("could not find {}", name ),
        }
    }
}

fn bytes_to_string(b: &[u8]) -> String {
    FromStr::from_str(str::from_utf8(b).unwrap()).unwrap()
}

fn func_type_parser() {
    let ctx = Context::new();
    let module = Module::new("main", &ctx);
    let builder = Builder::new(&ctx);

    let mut file = File::open("exampleCode/add.donk").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let mut map = AliasAndFunctions::new();
    function_type_parser( contents.as_bytes(), &mut map, &module, &builder, &ctx);
    parse_rest( contents.as_bytes(), &builder, &map, &ctx);
    //let func = debug( contents.as_bytes());
    println!("{:?}", map);
    module.verify();
    let ee = JitEngine::new(&module, JitOptions {opt_level: 3}).unwrap();
    ee.with_function(map.get_function(&String::from("foo")).func, |add:extern fn((f64, f64)) -> f64| {
        println!("{} + {} = {}", 1., 2., add((1., 2.)));
    });
}

pub fn get_arg_types<'a>(x: &Vec<(String, &'a Type)>) -> Vec<&'a Type> {
    x.iter().map(|y| {
        y.1.clone()
    }).collect()
}

struct FunctionIR<'a> {
    pub name: String,
    pub func_type: &'a FunctionType,
    pub func: &'a Function,
    pub args: Vec<&'a Type>,
    pub ret_type: &'a Type,
    pub entry: &'a BasicBlock,
}

impl<'a> FunctionIR<'a> {
    pub fn new(name: String,
                args: Vec<&'a Type>,
                ret_type: &'a Type,
                module: &'a CSemiBox<Module>,
                builder: &'a Builder,
                ctx: &'a CBox<Context>) -> Self {
        let func_type = FunctionType::new(ret_type, &args);
        let func = module.add_function(&name, func_type);
        let entry = func.append("entry");
        builder.position_at_end(entry);
        FunctionIR {name: name.clone(), func_type, func, args, ret_type, entry}
    }

    pub fn set_current(&self, builder: &'a Builder) {
        builder.position_at_end(self.entry);
    }

    pub fn arg_len(&self) -> usize {
        self.args.len()
    }

    pub fn get_args_with_name(&self, mut names: Vec<String>) -> Vec<(String, &'a Value)> {
        let mut v:Vec<(String, &'a Value)> = Vec::new();

        for (i, name) in names.drain(0..).enumerate() {
            v.insert(i,(name, &self.func[i]));
        }
        v
    }
}

impl<'a> fmt::Debug for FunctionIR<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.func)
    }
}

named!(args_parser<Vec<String>>, many0!(map_res!(
    map_res!(
        ws!(take_until!(" ")),
        str::from_utf8),
        FromStr::from_str
    )
));

named!(debug<&[u8]>, take_until!("\n"));

named_args!(parse_rest <'a>(builder: &'a CSemiBox<Builder>,
                            map: &'a AliasAndFunctions<'a>,
                            ctx: &'a CBox<Context>)
                            <Vec<bool>>, many0!( do_parse!(
                                func: alt!(
                                    import_parse |
                                    apply!(function_parser, builder, map, ctx)
                                    ) >>
                                ({
                                    func
                                })
                            ))
);

named!(import_parse<bool>, do_parse!(
    tag!("import") >>
    module: alias >>
    (false)
));

named_args!(function_body <'a>(builder: &'a CSemiBox<Builder>,
                              map: &'a AliasAndFunctions<'a>,
                              ctx: &'a CBox<Context>)
                              <HashMap<String, &'a Value>>, do_parse!(
    nametype: alias >>
    take_until!("\n") >>
    name: alias >>
    args: take_until!(":=") >>
    tag!(":=") >>
    ({
        if nametype != name { panic!("{} and {} should be the same", nametype, name)}
        let mut vals = HashMap::new();
        let func = map.get_function(&name);
        let mut fx = func.get_args_with_name(args_parser(args).unwrap().1);
        for (k,v) in fx.drain(0..) {
            vals.insert(k,v);
        }
        func.set_current(builder);
        vals
        })
));

named_args!(entry<'a>(vals: &'a HashMap<String, &'a Value>, map: &'a AliasAndFunctions, builder: &'a CSemiBox<Builder>, ctx: &'a CBox<Context>)<&'a Value>, do_parse!(
    init: apply!(value, vals, map, builder, ctx) >>
    res: fold_many0!(
        pair!(operators, apply!(value, vals, map, builder, ctx)),
        init,
        | acc, (op, val): (&[u8], &'a Value)| apply_operators(op, acc, val, builder)
    ) >>
    ({
        res
    })
));

named_args!(function_parser<'a>(builder: &'a CSemiBox<Builder>,
                                map: &'a AliasAndFunctions<'a>,
                                ctx: &'a CBox<Context>)
                                <bool>, do_parse!(
    vals: apply!(function_body, builder, map, ctx)>>
    body: apply!(entry, &vals, map, builder, ctx) >>
    ({
        builder.build_ret(body);
        true
    })
));

named_args!(foo<'a>(map: &mut AliasAndFunctions<'a>, module: &'a CSemiBox<Module>, builder: &'a CSemiBox<Builder>, ctx: &'a CBox<Context>)<Vec<u8>>,
    do_parse!(
        x: many0!(apply!(function_type_parser, map, module, builder, ctx)) >>
        ({
            let mut v = Vec::new();
            for i in x.clone().iter_mut() {
                v.append(&mut i.to_vec());
            }
            v
        })
));

named_args!(function_type_parser<'a>(map: &mut AliasAndFunctions<'a>, module: &'a CSemiBox<Module>, builder: &'a CSemiBox<Builder>, ctx: &'a CBox<Context>)<Vec<u8>>,
    do_parse!(
        x: many0!(alt!(apply!(function_type, map, module, builder, ctx) | take_until!("\n"))) >>
        ({
            let mut v = Vec::new();
            v
        })
));

named_args!(function_type<'a>(map: &mut AliasAndFunctions<'a>, module: &'a CSemiBox<Module>, builder: &'a CSemiBox<Builder>, ctx: &'a CBox<Context>)<&'a [u8]>, do_parse!(
    name: alias >>
    tag!(":") >>
    args: ws!(apply!(bracketed, ctx)) >>
    ws!(tag!("->")) >>
    ret: ws!(string) >>
    ({
        let f = FunctionIR::new(name.clone(), args, string_to_type(ret, ctx).unwrap(), module, builder, ctx);
        map.insert_func(name, f);
        &[]
    })
));

named_args!(bracketed<'a>(ctx: &'a CBox<Context>)<Vec<&'a Type>>,
    delimited!(
        tag!("("),
        many0!(apply!(type_parse, ctx)),
        tag!(")")
    )
);

named!(string<String>, map_res!(
    map_res!(
        ws!(alphanumeric),
        str::from_utf8
    ),
    FromStr::from_str
));

named!(parenthese_ending, ws!(alt!(tag!(",") | tag!(""))));

named_args!(type_parse<'a>(ctx: &'a CBox<Context>)<&'a Type>, do_parse!(
    typ: string >>
    parenthese_ending >>
    (string_to_type(typ, ctx).unwrap())
));

fn string_to_type<'a>(string: String, ctx: &'a CBox<Context>) -> Option<&'a Type> {
    match string.as_ref() {
        "i64" => Some(Type::get::<i64>(ctx)),
        "u64" => Some(Type::get::<u64>(ctx)),
        "i32" => Some(Type::get::<i32>(ctx)),
        "u32" => Some(Type::get::<u32>(ctx)),
        "f32" => Some(Type::get::<f32>(ctx)),
        "f64" => Some(Type::get::<f64>(ctx)),
        _     => None
    }
}

named!(operators, alt!(
    tag!("+") |
    tag!("-") |
    tag!("*") |
    tag!("/")
));

fn apply_operators<'a>(op: &[u8], left: &'a Value, right: &'a Value, builder: &'a CSemiBox<Builder>) -> &'a Value {
    match op[0] as char {
        '+' => builder.build_add(left, right),
        '-' => builder.build_sub(left, right),
        '*' => builder.build_mul(left, right),
        '/' => builder.build_div(left, right),
        _ => left
    }
}

named_args!(value<'a>(val: &'a HashMap<String, &'a Value>, map: &'a AliasAndFunctions, builder: &'a CSemiBox<Builder>, ctx: &'a CBox<Context>)<&'a Value>, alt!(
    apply!(number_to_val,ctx) | apply!(func_call, val, map, builder, ctx) | apply!(alias_to_val, val)
));

named!(alias<String>,map_res!(
    map_res!(
        ws!(alpha),
        str::from_utf8
    ),
    FromStr::from_str
));

named!(number<f64>,
    map_res!(
      map_res!(
        ws!(digit),
        str::from_utf8
      ),
      FromStr::from_str
));

named_args!(func_call<'a>(vals: &'a HashMap<String, &'a Value>, map: &'a AliasAndFunctions, builder: &'a CSemiBox<Builder>, ctx: &'a CBox<Context>)<&'a Value>, do_parse!(
    name: alias >>
    args: delimited!(
              tag!("("),
              many0!(apply!(arg_parser, vals, map, builder, ctx)),
              tag!(")")
          ) >>
    ({
        println!("{:?}", vals);
        println!("{:?}", args);
        builder.build_call(map.get_function(&name).func, &args)
    })
));

named_args!(arg_parser<'a>(vals: &'a HashMap<String, &'a Value>, map: &'a AliasAndFunctions, builder: &'a CSemiBox<Builder>, ctx: &'a CBox<Context>)<&'a Value>, do_parse!(
    arg: apply!(value, vals, map, builder, ctx) >>
    parenthese_ending >>
    (arg)
));

named_args!(alias_to_val<'a>(values: &'a HashMap<String, &'a Value>)<&'a Value>,  do_parse!(
    val: alias >>
    ({
        match values.get(&val) {
            Some(x) => x,
            None => {
                println!("{:?}", values);
                panic!("could not find {}", val )
            },
        }
    })
));

named_args!(number_to_val<'a>(ctx: &'a CBox<Context>)<&'a Value>, do_parse!(
num: number >>
(num.compile(ctx))
));

fn main() {
    func_type_parser()
}