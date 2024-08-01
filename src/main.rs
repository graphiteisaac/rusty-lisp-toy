use std::collections::HashMap;
use std::{fmt, io};
use std::fmt::Formatter;
use std::io::Write;
use std::num::ParseFloatError;
use std::ptr::write;
use clap::Parser;

#[derive(Clone)]
enum LispExpr {
    Bool(bool),
    Sym(String),
    Num(f64),
    List(Vec<LispExpr>),
    Func(fn(&[LispExpr]) -> Result<LispExpr, LispErr>),
}

impl fmt::Display for LispExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let str = match self {
            LispExpr::Bool(b) => b.to_string(),
            LispExpr::Sym(s) => s.clone(),
            LispExpr::Num(n) => n.to_string(),
            LispExpr::List(list) => {
                let xs: Vec<String> = list
                    .iter()
                    .map(|x| x.to_string())
                    .collect();
                format!("({})", xs.join(","))
            }
            LispExpr::Func(_) => String::from("Function {}")
        };

        write!(f, "{}", str)
    }
}
#[derive(Debug)]
// huh: This should be a struct....
enum LispErr {
    Reason(String),
}

#[derive(Clone)]
struct LispEnv {
    data: HashMap<String, LispExpr>
}

macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: &[LispExpr]| -> Result<LispExpr, LispErr> {
            let floats = parse_list_of_floats(args)?;
            let first = floats.first().ok_or(LispErr::Reason(String::from("expected at least one argument")))?;
            let rest = &floats[1..];
            fn f (prev: &f64, xs: &[f64]) -> bool {
                match xs.first() {
                    Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                    None => true
                }
            }

            Ok(LispExpr::Bool(f(first, rest)))
        }
    }};
}

impl LispEnv {
    fn new() -> LispEnv {
        let mut data: HashMap<String, LispExpr> = HashMap::new();

        data.insert(
            String::from("+"),
            LispExpr::Func(
                |args: &[LispExpr]| -> Result<LispExpr, LispErr> {
                    Ok(LispExpr::Num(parse_list_of_floats(args)?.iter().fold(0.0, |sum, a| sum + a)))
                }
            )
        );

        data.insert(
            String::from("-"),
            LispExpr::Func(
                |args: &[LispExpr]| -> Result<LispExpr, LispErr> {
                    let floats = parse_list_of_floats(args)?;
                    let first = *floats.first().ok_or(LispErr::Reason(String::from("expected at least one argument")))?;
                    let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

                    Ok(LispExpr::Num(first - sum_of_rest))
                }
            )
        );

        data.insert(
            String::from("/"),
            LispExpr::Func(
                |args: &[LispExpr]| -> Result<LispExpr, LispErr> {
                    let floats = parse_list_of_floats(args)?;
                    let first = *floats.first().ok_or(LispErr::Reason(String::from("expected at least one argument")))?;
                    let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);
                    if sum_of_rest == 0.0 {
                        return Err(LispErr::Reason(String::from("cannot divide 0 by 0")))
                    }

                    Ok(LispExpr::Num(first / sum_of_rest))
                }
            )
        );

        data.insert(
            String::from("*"),
            LispExpr::Func(
                |args: &[LispExpr]| -> Result<LispExpr, LispErr> {
                let floats = parse_list_of_floats(args)?;
                let first = *floats.first().ok_or(LispErr::Reason(String::from("expected at least one argument")))?;
                let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

                Ok(LispExpr::Num(first * sum_of_rest))
            })
        );

        data.insert(String::from("="), LispExpr::Func(ensure_tonicity!(|a, b| a == b)));
        data.insert(String::from(">"), LispExpr::Func(ensure_tonicity!(|a, b| a > b)));
        data.insert(String::from("<"), LispExpr::Func(ensure_tonicity!(|a, b| a < b)));
        data.insert(String::from(">="), LispExpr::Func(ensure_tonicity!(|a, b| a >= b)));
        data.insert(String::from("<="), LispExpr::Func(ensure_tonicity!(|a, b| a <= b)));

        LispEnv {data}
    }
}

fn parse_list_of_floats(args: &[LispExpr]) -> Result<Vec<f64>, LispErr> {
    args
        .iter()
        .map(|x| parse_single_float(x))
        .collect()
}

fn parse_single_float(expr: &LispExpr) -> Result<f64, LispErr> {
    match expr {
        LispExpr::Num(num) => Ok(*num),
        _ => Err(LispErr::Reason(String::from("expected a number")))
    }
}

fn tokenize(expr: String) -> Vec<String> {
    expr
        .replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn parse(tokens: &[String]) -> Result<(LispExpr, &[String]), LispErr> {
    let (token, rest) = tokens.split_first()
        .ok_or(
            LispErr::Reason(String::from("could not retrieve token")),
        )?;
    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(LispErr::Reason(String::from("unexpected token ')'"))),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq(tokens: &[String]) -> Result<(LispExpr, &[String]), LispErr> {
    let mut res: Vec<LispExpr> = vec![];
    let mut xs = tokens;
    loop {
        let (next_token, rest) = xs
            .split_first()
            .ok_or(LispErr::Reason(String::from("could not find closing `)`")))
        ?;
        if next_token == ")" {
            return Ok((LispExpr::List(res), rest))
        }

        let (exp, new_xs) = parse(&xs)?;
        res.push(exp);
        xs = new_xs
    }
}

fn parse_atom(token: &str) -> LispExpr {
    match token.as_ref() {
        "true" => LispExpr::Bool(true),
        "false" => LispExpr::Bool(false),
        _ => {
            let potential_float: Result<f64, ParseFloatError> = token.parse();
            match potential_float {
                Ok(v) => LispExpr::Num(v),
                Err(_) => LispExpr::Sym(token.to_string().clone())
            }
        }
    }
}

fn eval_if_builtin(arg_forms: &[LispExpr], env: &mut LispEnv) -> Result<LispExpr, LispErr> {
    let test_form = arg_forms.first().ok_or(
        LispErr::Reason(String::from("expected test form")),
    )?;

    match eval(test_form, env)? {
        LispExpr::Bool(b) => {
            let form_idx = if b { 1 } else { 2 };
            let res_form = arg_forms.get(form_idx).ok_or(LispErr::Reason(format!("expected form index `{}`", form_idx)))?;

            eval(res_form, env)
        },
        _ => Err(
            LispErr::Reason(format!("unexpected test form `{}`", test_form.to_string()))
        )
    }
}

fn eval_def_builtin(arg_forms: &[LispExpr], env: &mut LispEnv) -> Result<LispExpr, LispErr) {

}

fn eval_builtin_form(expr: &LispExpr, arg_forms: &[LispExpr], env: &mut LispEnv) -> Option<Result<LispExpr, LispErr>> {
    match expr {
        LispExpr::Sym(s) => match s.as_ref() {
            "if" => Some(eval_if_builtin(arg_forms, env)),
            "def" => Some(eval_def_builtin(arg_forms, env)),
            _ => None,
        },
        _ => None,
    }
}

fn eval(expr: &LispExpr, env: &mut LispEnv) -> Result<LispExpr, LispErr> {
    match expr {
        LispExpr::Sym(k) =>
            env.data.get(k)
                .ok_or(LispErr::Reason(format!("unexpected symbol key ({})", k)))
                .map(|x| x.clone()),
        LispExpr::Num(_a) => Ok(expr.clone()),
        LispExpr::List(list) => {
            let first_form = list
                .first()
                .ok_or(LispErr::Reason(String::from("expected a non-empty list")))?;
            let arg_forms = &list[1..];
            match eval_builtin_form(first_form, arg_forms, env) {
                Some(res) => res,
                None => {
                    let first_eval = eval(first_form, env)?;
                    match first_eval {
                        LispExpr::Func(f) => {
                            let args_eval = arg_forms
                                .iter()
                                .map(|x| eval(x, env))
                                .collect::<Result<Vec<LispExpr>, LispErr>>();
                            f(&args_eval?)
                        },
                        _ => Err(
                            LispErr::Reason(String::from("first form must be a function"))
                        ),
                    }
                }
            }

        },
        LispExpr::Func(_) => Err(
            LispErr::Reason(String::from("unexpected function in evaluation"))
        ),
        LispExpr::Bool(_a) => Ok(expr.clone()),
    }
}

fn slurp_expr() -> String {
    let mut expr = String::new();
    io::stdin().read_line(&mut expr).
        expect("Failed to readline");

    expr
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// String of S-expressions to evaluate
    #[arg(short, long)]
    eval: Option<String>,
}

fn main() {
    let args = Args::parse();
    let mut env = LispEnv::new();

    match args.eval {
        None => {
            loop {
                print!("LISP |> ");
                io::stdout().flush().expect("TODO: panic message");

                let expr = slurp_expr();
                let (parsed_exp, _) = parse(&tokenize(expr)).expect("could not parse");

                match eval(&parsed_exp, &mut env) {
                    Ok(res) => println!("[{}]", res),
                    Err(e) => match e {
                        LispErr::Reason(msg) => println!("! => {}", msg),
                    }
                }
            }
        }
        Some(e) => {
            let (parsed_exp, _) = parse(&tokenize(e)).expect("could not parse");
            match eval(&parsed_exp, &mut env) {
                Ok(res) => println!("[{}]", res),
                Err(e) => match e {
                    LispErr::Reason(msg) => println!("! => {}", msg),
                }
            }
        }
    }
}