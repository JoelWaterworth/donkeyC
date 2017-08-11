use nom::{digit, alpha, alphanumeric};
use nom;
use std::str;
use std::str::FromStr;
use llvm::*;
use std::collections::HashMap;

use ir::{AliasAndFunctions, FunctionIR};

fn bytes_to_string(b: &[u8]) -> String {
    FromStr::from_str(str::from_utf8(b).unwrap()).unwrap()
}

pub fn parser<'a>(contents: String, module: &'a CSemiBox<Module>, builder: &'a CSemiBox<Builder>, ctx: &'a CBox<Context>) -> AliasAndFunctions<'a> {
    let mut map = AliasAndFunctions::new();
    function_type_parser( contents.as_bytes(), &mut map, module, builder, ctx);
    parse_rest( contents.as_bytes(), builder, &map, ctx);
    map
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