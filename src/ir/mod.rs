use llvm::*;
use std::fmt;
use std::collections::HashMap;

pub struct FunctionIR<'a> {
    pub name: String,
    pub func_type: &'a FunctionType,
    pub func: &'a Function,
    pub entry: Option<&'a BasicBlock>,
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
        FunctionIR {name: name.clone(), func_type, func, entry: Some(entry)}
    }

    pub fn from(name: String, func_type: &'a FunctionType, func: &'a Function) -> Self {
        FunctionIR {name, func_type , func, entry: None}
    }

    pub fn set_current(&self, builder: &'a Builder) {
        builder.position_at_end(self.entry.unwrap());
    }

    pub fn arg_len(&self) -> usize {
        self.func_type.num_params()
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


#[derive(Debug)]
pub struct AliasAndFunctions<'a>{
    functions: HashMap<String, FunctionIR<'a>>,
}

impl<'a> AliasAndFunctions<'a> {
    pub fn new() -> AliasAndFunctions<'a> {
        AliasAndFunctions {functions: HashMap::new() }
    }

    pub fn insert_func(&mut self, name: String, val: FunctionIR<'a>) -> Option<FunctionIR<'a>> {
        self.functions.insert(name, val)
    }

    pub fn get_function(&self, name: &String) -> &FunctionIR<'a> {
        let func = self.functions.get(name);
        match func {
            Some(x) => x,
            None => panic!("could not find {}", name ),
        }
    }

    pub fn declare_function(&mut self, module: &'a CSemiBox<'a, Module>, name: &'a str, sig: &'a Type) {
        let func = module.add_function(name, sig);
        let tye = func.get_signature();
        self.insert_func(String::from(name), FunctionIR::from(String::from(name), tye, func));
    }
}