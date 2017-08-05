#[macro_use]
extern crate nom;
extern crate llvm;

use nom::{digit, alpha};

use llvm::*;

use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::str;
use std::str::FromStr;

fn parser() {
    let ctx = Context::new();
    let module = Module::new("main", &ctx);
    let builder = Builder::new(&ctx);

    let mut file = File::open("exampleCode/main.donk").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let mut map = HashMap::new();
    let func = parse( contents.as_bytes(), &module, &builder, &mut map, &ctx);
    println!("{:?}", func);
}

#[derive(Debug)]
struct FunctionDef<'a> {
    pub name: String,
    pub args: Vec<(String, &'a Type)>,
    pub ret_type: &'a Type,
}

pub fn get_arg_types<'a>(x: &Vec<(String, &'a Type)>) -> Vec<&'a Type> {
    x.iter().map(|y| {
        y.1.clone()
    }).collect()
}

struct FunctionIR<'a> {
    name: String,
    func_type: &'a FunctionType,
    func: &'a Function,
    def: FunctionDef<'a>
}

impl<'a> FunctionIR<'a> {
    pub fn new(name: String,
                args: Vec<(String, &'a Type)>,
                ret_type: &'a Type,
                module: &'a CSemiBox<Module>,
                builder: &'a Builder,
                ctx: &'a CBox<Context>) -> Self {
        let func_type = FunctionType::new(ret_type, &get_arg_types(&args));
        let func = module.add_function(&name, func_type);
        let entry = func.append("entry");
        builder.position_at_end(entry);
        let def = FunctionDef {
            name: name,
            args: args,
            ret_type};
        FunctionIR {name: def.name.clone(), func_type, func, def}
    }
    pub fn add_arguments_to_map(&self, map: &mut HashMap<String, &'a Value>) {
        let x = self.def.args.len();
        for i  in 0..x {
          map.insert(self.def.args[i].0.clone(), &self.func[i]);
        };
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

fn extract<'a, 'b>(x: Vec<String>, ctx: &'a CBox<Context>) -> Vec<(String, &'a Type)> {
    x.iter().map(|x| {
        let s = x.clone();
        (s, Type::get::<i64>(ctx))
    }).collect()
}

named_args!(parse <'a>( module: &'a CSemiBox<Module>,
                        builder: &'a CSemiBox<Builder>,
                        map: &mut HashMap<String, &'a Value>,
                        ctx: &'a CBox<Context>)
                        <Vec<FunctionIR<'a>>>, many0!( apply!(
                            function_parser, module, builder, map, ctx
                        ))
);

named_args!(function_parser<'a>(module: &'a CSemiBox<Module>,
                                builder: &'a CSemiBox<Builder>,
                                map: &mut HashMap<String, &'a Value>,
                                ctx: &'a CBox<Context>)
                                <FunctionIR<'a>>, do_parse!(
    func: apply!(func_type, module, builder, map, ctx)>>
    body: apply!(body_parse, ctx, builder, map) >>
    ({
        builder.build_ret(body);
        func
    })
));



named_args!(func_type<'a>(module: &'a CSemiBox<Module>,
                          builder: &'a CSemiBox<Builder>,
                          map: &mut HashMap<String, &'a Value>,
                          ctx: &'a CBox<Context>)
                          <FunctionIR<'a>>, do_parse!(
    name: alias >>
    args:  take_until!(":=") >>
    tag!(":=") >>
    ({
        let func = FunctionIR::new(
            name,
            extract(args_parser(args).unwrap().1, ctx),
            Type::get::<i64>(ctx),
            module,
            builder,
            ctx,
        );
        func.add_arguments_to_map(map);
        func
        })
));

named_args!(body_parse<'a>(ctx: &'a CBox<Context>, builder: &'a CSemiBox<Builder>, map: &mut HashMap<String, &'a Value>)<&'a Value>, do_parse!(
    init: apply!(value, map, ctx) >>
    res: fold_many0!(
        pair!(operators, apply!(value, map, ctx)),
        init,
        | acc, (op, val): (&[u8], &'a Value)| apply_operators(op, acc, val, builder)
    ) >>
    ({
        map.clear();
        res
    })
));

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

named_args!(value<'a>(map: &HashMap<String, &'a Value>, ctx: &'a CBox<Context>)<&'a Value>, alt!(
    apply!(number_to_val,ctx) | apply!(alias_to_val, map)
));

named!(alias<String>,map_res!(
    map_res!(
        ws!(alpha),
        str::from_utf8
    ),
    FromStr::from_str
));

named!(number<i64>,
    map_res!(
      map_res!(
        ws!(digit),
        str::from_utf8
      ),
      FromStr::from_str
));

named_args!(alias_to_val<'a>(values: &HashMap<String, &'a Value>)<&'a Value>,  do_parse!(
    val: alias >>
    ({
        values.get(&val).unwrap()})
));

named_args!(number_to_val<'a>(ctx: &'a CBox<Context>)<&'a Value>, do_parse!(
num: number >>
(num.compile(ctx))
));

fn main() {
    parser()
}