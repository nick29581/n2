// N syntactic proof assistant v0.2

#![feature(struct_variant)]
#![feature(globs)]
#![allow(non_camel_case_types)]

mod lang {
    // This section defines the meta-lanaguage items.

    // Terms define the syntax of the language.
    #[deriving(Eq)]
    pub enum Term<'l> {
        constant(Constant<'l>),
        variable(Variable<'l>),
        non_terminal(NonTerminal<'l>),
    }

    #[deriving(Eq)]
    pub struct Constant<'l> {
        pub name: &'static str,
    }

    impl<'l> Constant<'l> {
        pub fn new(name: &'static str) -> Constant<'l> {
            Constant{ name: name }
        }
    }

    pub struct Variable<'l> {
        // type of the meta-variable, type it can take
        pub typ: &'l Type,
        pub takes: &'l Type,
    }

    impl<'l> Variable<'l> {
        pub fn new(typ: &'l Type, takes: &'l Type) -> Variable<'l> {
            Variable{ typ: typ, takes: takes }
        }
    }

    impl<'l> Eq for Variable<'l> {
        fn eq(&self, other: &Variable<'l>) -> bool {
            self.typ == other.typ
        }
    }

    pub struct NonTerminal<'l> {
        pub typ: &'l Type,
        pub variants: Vec<&'l Variant<'l>>
    }

    impl<'l> Eq for NonTerminal<'l> {
        fn eq(&self, other: &NonTerminal<'l>) -> bool {
            self.typ == other.typ
        }
    }

    pub struct Variant<'l> {
        pub terms: Vec<&'l Term<'l>>
    }

    #[deriving(Eq)]
    pub struct Type {
        pub repr: &'static str,
        pub aliases: Vec<&'static str>,
    }

    // This section defines language items.
    #[deriving(Eq)]
    pub enum Expr<'l> {
        e_const(Constant<'l>),
        e_var(Variable<'l>, &'static str), // str is the name of the variable
        e_non(Vec<&'l Expr<'l>>),
    }

    pub struct AstNode<'l> {
        pub expr: &'l Expr<'l>,
        pub term: &'l Term<'l>,
        pub variant: Option<uint>,
        pub children: Vec<~AstNode<'l>>
    }

    impl<'l> Eq for AstNode<'l> {
        fn eq(&self, other: &AstNode<'l>) -> bool {
            *self.expr == *other.expr &&
            *self.term == *other.term &&
            self.variant == other.variant &&
            self.children == other.children
        }
    }

    pub fn close_defs<'a, 'l>(defs: &'a Vec<&'l Term<'l>>) -> Vec<&'l Term<'l>> {
        let mut result = defs.clone();
        let mut rerun = false;
        for d in defs.iter() {
            match **d {
                non_terminal(ref nt) => {
                    for i in range(0, nt.variants.len()) {
                        let v = nt.variants.get(i);
                        for t in v.terms.iter() {
                            if !result.contains(t) {
                                result.push(*t);
                                match **t {
                                    non_terminal(_) => rerun = true,
                                    _ => {}
                                }
                            }
                        }
                    }
                }
                _ => {}                
            }
        }

        if rerun {
            close_defs(&result)
        } else {
            result
        }
    }

    pub mod parse {
        use lang::*;

        pub fn parse<'a, 'l>(expr: &'l Expr<'l>, defs: &'a Vec<&'l Term<'l>>) -> ~AstNode<'l> {
            let result = match *expr {
                e_const(ref c) => parse_const(c, defs, expr),
                e_var(ref var, name) => parse_var(var, name, defs, expr),
                e_non(ref es) => parse_non(es, defs, expr),
            };

            result
        }

        fn find_const<'a, 'l>(c: &Constant<'l>, defs: &'a Vec<&'l Term<'l>>) -> &'l Term<'l> {
            for d in defs.iter() {
                match **d {
                    constant(cc) if *c == cc => return *d,
                    _ => {}
                }
            }
            fail!("Could not find constant {}", c.name);
        }

        fn try_promote<'a, 'l>(a: ~AstNode<'l>,
                               defs: &'a Vec<&'l Term<'l>>,
                               expr: &'l Expr<'l>) -> ~AstNode<'l> {
            for d in defs.iter() {
                match **d {
                    non_terminal(ref nt) => {
                        for i in range(0, nt.variants.len()) {
                            let v = nt.variants.get(i);
                            if v.terms.len() == 1 {
                                if a.term == *v.terms.get(0) {
                                    return ~AstNode {
                                        expr: expr,
                                        term: *d,
                                        variant: Some(i),
                                        children: vec!(a)
                                    };
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            // Could not be promoted, return the bare const.
            a
        }

        fn parse_const<'a, 'l>(c: &Constant<'l>,
                               defs: &'a Vec<&'l Term<'l>>,
                               expr: &'l Expr<'l>) -> ~AstNode<'l> {
            try_promote(~AstNode {
                expr: expr,
                term: find_const(c, defs),
                variant: None,
                children: vec!()
            }, defs, expr)
        }

        fn find_var<'a, 'l>(v: &Variable<'l>, defs: &'a Vec<&'l Term<'l>>) -> &'l Term<'l> {
            for d in defs.iter() {
                match **d {
                    variable(vv) if vv == *v => return *d,
                    _ => {}
                }
            }
            fail!("Could not find variable {}", v.typ.repr);
        }

        fn parse_var<'a, 'l>(v: &Variable<'l>,
                             name: &'static str,
                             defs: &'a Vec<&'l Term<'l>>,
                             expr: &'l Expr<'l>) -> ~AstNode<'l> {
            if !v.typ.aliases.iter().any(|a| *a == name) {
                fail!("{} not an alias of {}", name, v.typ.repr);
            }
            ~AstNode {
                expr: expr,
                term: find_var(v, defs),
                variant: None,
                children: vec!()
            }
        }

        fn find_non<'a, 'l>(kids: &Vec<~AstNode<'l>>, defs: &'a Vec<&'l Term<'l>>) -> (&'l Term<'l>, uint) {
            for d in defs.iter() {
                match **d {
                    non_terminal(ref nt) => {
                        for i in range(0, nt.variants.len()) {
                            let v = nt.variants.get(i);
                            if v.terms.len() == kids.len() {
                                if kids.iter().zip(v.terms.iter()).all(|(k, t)| *t == k.term) {
                                    return (*d, i);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
            fail!("Could not find non-terminal {}; {}",
                  ::print::join(kids.iter().map(|k| ::print::node_str(&**k)), " "),
                  ::print::join(kids.iter().map(|k| ::print::use_str(k.term).to_owned()), " "));
        }

        fn parse_non<'a, 'l>(es: &Vec<&'l Expr<'l>>, defs: &'a Vec<&'l Term<'l>>, expr: &'l Expr<'l>) -> ~AstNode<'l> {
            let children: Vec<~AstNode<'l>> = es.iter().map(|e| parse(*e, defs)).collect();

            let (t, idx) = find_non(&children, defs);
            ~AstNode {
                expr: expr,
                term: t,
                variant: Some(idx),
                children: children
            }
        }
    }
}

mod print {
    use lang::*;
    use std::io::stdio::print;

    pub fn print_use<'l>(term: &Term<'l>) {
        print(use_str(term));
    }

    pub fn use_str<'l>(term: &Term<'l>) -> &'static str {
        match *term {
            constant(Constant{name: s}) => s,
            variable(Variable{typ: t, ..}) |
            non_terminal(NonTerminal{typ: t, ..}) => t.repr,
        }
    }

    fn print_def<'l>(term: &Term<'l>) {
        match *term {
            constant(_) => {},
            variable(Variable{typ: t, ..}) => {
                print(join(t.aliases.iter().map(|s| s.to_owned()), ", "));
                println!("");
            }
            non_terminal(ref nt) => {
                print(nt.typ.repr);
                print(" ::= ");
                print(join(nt.variants.iter().map(|v| {
                    join(v.terms.iter().map(|t| use_str(*t).to_owned()), " ")
                }) , " | "));
                println!("");
            }
        }
    }

    pub fn join<T: Iterator<~str>>(mut items: T, sep: &str) -> ~str {
        let mut first = true;
        let mut result = ~"";
        for s in items {
            if first {
                first = false;
            } else {
                result = result + sep;
            }
            result = result + s;
        }
        result
    }

    pub fn print_defs<'a, 'l>(terms: &'a Vec<&Term<'l>>) {
        for t in terms.iter() {
            print_def(*t);
        }
    }

    pub fn expr_str<'l>(e: &'l Expr<'l>) -> ~str {
        match *e {
            e_const(c) => c.name.to_owned(),
            e_var(_, s) => s.to_owned(),
            e_non(ref ts) => "(" + join(ts.iter().map(|t| expr_str(*t)), " ") + ")",
        }
    }

    pub fn print_expr<'l>(e: &'l Expr<'l>) {
        print(expr_str(e));
        println!("");
    }

    pub fn node_str<'l>(a: &AstNode<'l>) -> ~str {
        expr_str(a.expr)
    }
}

fn main() {
    use lang::*;

    // Describe the syntax of the untyped lambda calculus.
    let x = ~Type{repr:"x", aliases: vec!("x", "y")};
    let e = ~Type{repr:"e", aliases: vec!("e")};
    let n = ~Type{repr:"n", aliases: vec!("n")};

    let lambda = ~constant(Constant::new("/lambda"));
    let dot = ~constant(Constant::new("."));
    let _n0 = ~constant(Constant::new("0"));
    let _n1 = ~constant(Constant::new("1"));
    let n0 = ~Variant{terms: vec!(&*_n0)};
    let n1 = ~Variant{terms: vec!(&*_n1)};

    let var = ~variable(Variable::new(x, e));
    let num = ~non_terminal(NonTerminal{typ: n, variants: vec!(&*n0, &*n1)});
    let use_var = ~Variant{terms: vec!(&*var)};
    let use_num = ~Variant{terms: vec!(&*num)};
    let fun_app = ~Variant{terms: vec!()};
    let fun_def = ~Variant{terms: vec!()};
    let exp = ~non_terminal(NonTerminal{typ: e, variants: vec!()});
    let exp_ = match exp {
        ~non_terminal(ref nt) => nt,
        _ => fail!(),
    };

    // Need an unsafe block due to cycles.
    unsafe {
        let fa = std::cast::transmute_mut(fun_app);
        fa.terms = vec!(&*exp, &*exp);
        let fd = std::cast::transmute_mut(fun_def);
        fd.terms = vec!(&*lambda, &*var, &*dot, &*exp);
        let e = std::cast::transmute_mut(exp_);
        e.variants = vec!(&*use_num, &*use_var, &*fun_def, &*fun_app);
    }

    let defs = vec!(&*exp);
    let defs = close_defs(&defs);
    print::print_defs(&defs);

    // Example programs in the ulc (literal expressions).

    // 0 ~~> 0
    let z = e_const(Constant::new("0"));
    print::print_expr(&z);
    let _ast = parse::parse(&z, &defs);

    // /lambda x.0 1 ~~> 0
    let z = e_non(vec!(&z));
    let one = e_const(Constant::new("1"));
    let one = e_non(vec!(&one));
    let lambda = e_const(Constant::new("/lambda"));
    let y = e_var(Variable::new(x, e), "y");
    let x = e_var(Variable::new(x, e), "x");
    let dot = e_const(Constant::new("."));
    let f = e_non(vec!(&lambda, &x, &dot, &z));
    let apply = e_non(vec!(&f, &one));
    print::print_expr(&apply);
    let _ast = parse::parse(&apply, &defs);

    // /lambda x./lambda y.1 /lambda y.0 ~~> /lambda x.1
    let f1 = e_non(vec!(&lambda, &y, &dot, &one));
    let f2 = e_non(vec!(&lambda, &y, &dot, &z));
    let apply = e_non(vec!(&f1, &f2));
    let f3 = e_non(vec!(&lambda, &x, &dot, &apply));
    print::print_expr(&f3);

    let _ast = parse::parse(&f3, &defs);

    // TODO next add meta-variables
}
