/**  
                ---Plan---

  1- Syntax definition (first see ast.ts)
  2- Parsing
  3- Type checking
  4- Evaluation
  5- JS Code generation (if time allows it)
*/

import * as AST from "./ast";

/*
  For parsing we'll use the pcomb library 
  
            https://github.com/yelouafi/pcomb
  
  Other options include 
    - manually writing the parser. See for example 
    
            https://github.com/jamiebuilds/the-super-tiny-compiler

    - Use a "parser generator": a tool that takes the syntax definition in
      some specific notation and generate the parser code. See for example
      
            https://pegjs.org/

  I prefer Parser Combinators, they are simple and compositional (= powerful)
  For an introdcution to Parser Combinators  see
  
      https://abstractfun.com/2018-11-19/introduction-to-parser-combinators
*/

import {
  token,
  testParser,
  Parser,
  seq,
  collect,
  oneOf,
  lazy,
  apply,
  maybe,
  many,
} from "pcomb";
/*
First we must fix a couple of things about our syntax

  term :=    
    num
    var
  ? term op term     
    fn(x: type) => term -- binding forms
  ? term(term)
    let x = term in term -- binding forms
    ( term )
  
  The problem is in the lines marked with ?. For example the line
  `term op term` says something like: 
      
      to parse a term we need to parse a term followed by
      an op followed by term

  This is an ambigous definition, because it basically says that
  in order to parse a term we must start by parsing a term. The 
  problem is also known as "left recursion"

  To remove left recursion, a common technique is to move out
  all the non ambigous definition (ie those not starting with a term)
  into a separate rule. The final result will look like


  term :=    
    app ?op term     
    
  app :=
    factor *(term)

  factor :=
    num
    var
    fn(x: type) => term
    let x = term in term 
    ( term )
  
  we created 2 new rules : `factor` (for non left recursive rules), and
  `app` for function application: `?op term` means that the suffix  `op term` 
  becomes optional while `*(term)` means we can repete `(term)` many times 
  (including 0).

We do the same thing for types

  type :=
    num
    type => type
    ( type )

  type :=
    tprefix ?tsuffix

  tsuffix :=
    => type
      
  tprefix :=
      num
      ( type )

  **ONLY THEN** we can start implementing our parser
*/

/*
  First we define the atomic words of our language.
  
  `token(pattern)` takes a `pattern` which can be a string, a regex (or even another parser)
  and returns a parser for that `pattern`. `token` will also skip trailing spaces after the word.
*/
const NUM = token(/\d+/);
const VAR = token(/[a-z]+/);
const OP = token(/\+|\*/);
const FN = token("fn");
const LET = token("let");
const IN = token("in");
const TNUM = token("num");
const LPAR = token("(");
const RPAR = token(")");
const COLON = token(":");
const EQ = token("=");
const ARROW = token("=>");

/*
  Then we define parsers which construct our AST
*/
const tnum = TNUM.map((_) => AST.TNum);

/*
  note `type` and `tsuffix`/`tprefix` are mutually recursive.
  We have a chicken-egg problem. To circumvant the circularity
  we use `lazy` which takes a function returning a parser. This will
  delay the creation of `type` which breaks the vicious circle. 
*/
// tprefix ?tsuffix
const type: Parser<AST.Type> = lazy(() => {
  return apply(
    (ty, rest) => {
      if (rest == null) return ty;
      // num => num
      return AST.TFun(ty, rest);
    },
    tprefix, // tprefix 
    maybe(tsuffix) // ?tsuffix
  );
});

// => type
const tsuffix = seq(ARROW, type);

// num | ( type )
const tprefix = oneOf(tnum, type.between(LPAR, RPAR));

const num = NUM.map((s) => AST.Num(+s));
const var_ = VAR.map(AST.Var);

// app ?op term
const term: Parser<AST.Term> = lazy(() => {
  return apply(
    (t, rest) => {
      if (rest == null) return t;
      return AST.Op(t, rest[0], rest[1]);
    },
    app, // app
    maybe(collect(OP, term)) // ?op term
  );
});

// fn(x: type) => term
const fn = apply(
  AST.Fun,
  seq(FN, LPAR, VAR), // fn(x
  seq(COLON, type), // : type
  seq(RPAR, ARROW, term) // ) => term
);

// let x = term in term
const let_ = apply(
  AST.Let,
  seq(LET, VAR), // let x
  seq(EQ, term), // = term
  seq(IN, term) // in term
);

// oneOf succeeds with any of the given alternatives
const factor = oneOf(num, fn, let_, var_, term.between(LPAR, RPAR));

// factor *(term)
const app = apply(
  (t, args) => {
    return args.reduce((acc, arg) => AST.App(acc, arg), t);
  },
  factor,
  many(term.between(LPAR, RPAR)) // *(term)
);

/*
  evaluates a term and returns a javascript value
  In our language we have 2 possible values: numbers and functions 
*/

// Env is dictionary to represent free variables of a term
type Env = {
  [key: string]: number | Function;
};
function evaluate(t: AST.Term, env: Env = {}): number | Function {
  if (t.type === "Num") return t.value;
  if (t.type === "Var") {
    if (t.name in env) return env[t.name];
    throw new Error(`Unkown variable ${t.name}`);
  }
  if (t.type === "Fun") {
    return function (value) {
      const newEnv = { ...env, [t.paramName]: value };
      return evaluate(t.body, newEnv);
    };
  }
  if (t.type === "App") {
    const arg = evaluate(t.arg, env);
    const fun = evaluate(t.fun, env);
    // runtime errors
    if (typeof fun != "function") {
      throw new Error(`Expected a function`);
    }
    return fun(arg);
  }
  if (t.type === "Op") {
    const left = evaluate(t.left, env) as number;
    const right = evaluate(t.right, env) as number;
    if (t.op === "+") return left + right;
    if (t.op === "*") return left * right;
    throw new Error(`Unkown operator ${t.op}`);
  }
  if (t.type === "Let") {
    // let and fn are binding constructs
    const def = evaluate(t.definition, env);
    const newEnv = { ...env, [t.name]: def };
    return evaluate(t.body, newEnv);
  }
}

// Scope stores the types of free variables in a term
type Scope = {
  [key: string]: AST.Type;
};
function typeCheck(t: AST.Term, scope: Scope = {}): AST.Type {
  if (t.type === "Num") return AST.TNum;
  if (t.type === "Var") {
    if (t.name in scope) return scope[t.name];
    throw new TypeError(`Unkown variable ${t.name}`);
  }
  if (t.type === "Fun") {
    const newScope = { ...scope, [t.paramName]: t.paramType };
    const tyRes = typeCheck(t.body, newScope);
    return AST.TFun(t.paramType, tyRes);
  }
  if (t.type === "App") {
    const fun = typeCheck(t.fun, scope);
    if (fun.type !== "TFun") {
      throw new TypeError(`Expected a function. found ${AST.printType(fun)}`);
    }
    const arg = typeCheck(t.arg, scope);
    if (!AST.typeEq(fun.tyParam, arg)) {
      throw new TypeError(
        `Function expected a ${AST.printType(
          fun.tyParam
        )}. found ${AST.printType(arg)}`
      );
    }
    return fun.tyResult;
  }
  if (t.type === "Op") {
    const left = typeCheck(t.left, scope);
    if (left.type !== "TNum") {
      throw new TypeError(
        `Op ${t.op} expected a num, found ${AST.printType(left)}`
      );
    }
    const right = typeCheck(t.right, scope);
    if (right.type !== "TNum") {
      throw new TypeError(
        `Op ${t.op} expected a num, found ${AST.printType(right)}`
      );
    }
    return AST.TNum;
  }
  if (t.type === "Let") {
    const def = typeCheck(t.definition, scope);
    const newScope = { ...scope, [t.name]: def };
    return typeCheck(t.body, newScope);
  }
}

/*
  Javascipt code generation. In fact, our language is a very
  small subset of Javascript. So the transpilation shouldn't be 
  difficult.
*/
function emitJS(t: AST.Term): string {
  if (t.type === "Num") return String(t.value);
  if (t.type === "Let") {
    return `(function() {
      let ${t.name} = ${emitJS(t.definition)};
      return ${emitJS(t.body)}
    })()`;
  }
  // The rest is left an exercise
}

// quick hack to evaluate programs in developer console
(window as any)._eval = (s) => {
  const t = testParser(term, s);
  const ty = typeCheck(t);
  const val = evaluate(t);
  return `${typeof val === "number" ? val : "<function>"} : ${AST.printType(
    ty
  )}`;
};
