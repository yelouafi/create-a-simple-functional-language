/**  
                ---Plan---

  1- Syntax definition (first see ast.ts)
  2- Parsing
  3- Type checking
  4- Evaluation
  5- JS Code generation (if time allows it)
*/

import * as AST from "./ast";

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
import { buildInfix, infixTable } from "./infix";
import { isSubtype, meet } from "./subtyping";

/*

For how to convert an abstract syntax into a concrete one
see master branch

  term :=    
    app ?op term     
    
  app := factor *appSuff
  
  appSuff := (term) | .tag

  factor :=
    num
    bool
    var
    if term then term else term
    fn(x: type) => term
    let x = term in term 
    ( term )
    {tag: term, ...}

  type :=
    tprefix ?tsuffix

  tsuffix :=
    => type
      
  tprefix :=
      num
      bool
      {tag: type, ...}
      ( type )
*/

const NUM = token(/\d+/);
const BOOL_TRUE = token("true");
const BOOL_FALSE = token("false");
const VAR = token(/[a-z]+/);
const OP = token(/[\+\-\*\\\^\=\<\>\&\|\#\@\!\~\$\?\%]+/);
const FN = token("fn");
const LET = token("let");
const IN = token("in");
const IF = token("if");
const THEN = token("then");
const ELSE = token("else");
const TNUM = token("num");
const TBOOL = token("bool");
const LPAR = token("(");
const RPAR = token(")");
const COLON = token(":");
const EQ = token("=");
const ARROW = token("=>");
const LBRACE = token("{");
const RBRACE = token("}");
const PERIOD = token(".");
const COMMA = token(",");

/*
  Then we define parsers which construct our AST
*/
const tnum = TNUM.mapTo(AST.TNum);
const tbool = TBOOL.mapTo(AST.TBool);
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

// tag: type
const tpair = collect(VAR, seq(COLON, type));

// { tag: type, ... }
const trecord = tpair
  .sepBy(COMMA)
  .between(LBRACE, RBRACE)
  .map((pairs) => {
    const dict = {};
    for (let [tag, type] of pairs) {
      dict[tag] = type;
    }
    return AST.TRecord(dict);
  });

// num | ( type )
const tprefix = oneOf(tnum, tbool, trecord, type.between(LPAR, RPAR));

const num = NUM.map((s) => AST.Num(+s));

const bool = oneOf(
  BOOL_TRUE.mapTo(AST.Bool(true)),
  BOOL_FALSE.mapTo(AST.Bool(false))
);

const var_ = VAR.map(AST.Var);

// app ?op term
const term: Parser<AST.Term> = lazy(() => {
  return apply(
    (t, rest) => buildInfix(t, rest, []),
    app, // app
    many(collect(OP, app)) // ?op term
  );
});

const if_ = apply(
  AST.If,
  seq(IF, term), // if term
  seq(THEN, term), // then term
  seq(ELSE, term) // else term
);

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

// tag: term
const pair = collect(VAR, seq(COLON, term));

// { tag: term, ... }
const record = pair
  .sepBy(COMMA)
  .between(LBRACE, RBRACE)
  .map((pairs) => {
    const dict = {};
    for (let [tag, type] of pairs) {
      dict[tag] = type;
    }
    return AST.Record(dict);
  });

// oneOf succeeds with any of the given alternatives
const factor = oneOf(
  num,
  bool,
  if_,
  fn,
  let_,
  var_,
  record,
  term.between(LPAR, RPAR)
);

// .tag ? | (term)
const appSuf = oneOf<string | AST.Term>(
  seq(PERIOD, VAR),
  term.between(LPAR, RPAR)
);

// factor *(term)
const app = apply(
  (t, args) => {
    return args.reduce((acc: AST.Term, arg) => {
      return typeof arg === "string"
        ? AST.Projection(acc, arg)
        : AST.App(acc, arg);
    }, t) as AST.Term;
  },
  factor,
  many(appSuf)
);

/*
  evaluates a term and returns a javascript value
  In our language we have 2 possible values: numbers and functions 
*/

// Env is dictionary to represent free variables of a term
type Value = number | boolean | Function | object;

type Env = {
  [key: string]: Value;
};
function evaluate(t: AST.Term, env: Env = {}): Value {
  if (t.type === "Num") return t.value;
  if (t.type === "Bool") return t.value;
  if (t.type === "Var") {
    if (t.name in env) return env[t.name];
    throw new Error(`Unkown variable ${t.name}`);
  }
  if (t.type === "If") {
    const cond = evaluate(t.cond, env) as boolean;
    if (cond === true) {
      return evaluate(t.then, env);
    } else {
      return evaluate(t.elze, env);
    }
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
    const left = evaluate(t.left, env);
    const right = evaluate(t.right, env);
    const fn = opTable[t.op];
    if (fn == null) throw new Error(`Unkown operator ${t.op}`);
    return fn(left, right);
  }
  if (t.type === "Let") {
    // let and fn are binding constructs
    const def = evaluate(t.definition, env);
    const newEnv = { ...env, [t.name]: def };
    return evaluate(t.body, newEnv);
  }
  if (t.type === "Record") {
    const res = {};
    for (let key in t.dict) {
      const value = evaluate(t.dict[key], env);
      res[key] = value;
    }
    return res;
  }
  if (t.type === "Projection") {
    const rec = evaluate(t.record, env) as any;
    if (t.field in rec) return rec[t.field];
    throw new Error(`object does not have field ${t.field}`);
  }
}

const opTable = {
  "&&": (x: any, y: any) => x && y,
  "||": (x: any, y: any) => x || y,
  "==": (x: any, y: any) => x == y,
  "!=": (x: any, y: any) => x != y,
  "<": (x: any, y: any) => x < y,
  ">": (x: any, y: any) => x > y,
  "<=": (x: any, y: any) => x <= y,
  ">=": (x: any, y: any) => x >= y,
  "+": (x: any, y: any) => x + y,
  "-": (x: any, y: any) => x - y,
  "*": (x: any, y: any) => x * y,
  "/": (x: any, y: any) => x / y,
  "**": (x: any, y: any) => x ** y,
};

// Scope stores the types of free variables in a term
type Scope = {
  [key: string]: AST.Type;
};
function typeCheck(t: AST.Term, scope: Scope = {}): AST.Type {
  if (t.type === "Num") return AST.TNum;
  if (t.type === "Bool") return AST.TBool;
  if (t.type === "Var") {
    if (t.name in scope) return scope[t.name];
    typeError(t, `Unkown variable ${t.name}`);
  }
  if (t.type === "If") {
    const cond = typeCheck(t.cond, scope);
    if (cond.type !== "TBool")
      typeError(t, `Exepected a bool, found ${AST.printType(cond)}\n`);
    const then = typeCheck(t.then, scope);
    const elze = typeCheck(t.elze, scope);
    const ty = meet(then, elze);
    if (ty == null) {
      typeError(t, `Types of then/else are not compatible`);
    }
    return ty;
  }
  if (t.type === "Fun") {
    const newScope = { ...scope, [t.paramName]: t.paramType };
    const tyRes = typeCheck(t.body, newScope);
    return AST.TFun(t.paramType, tyRes);
  }
  if (t.type === "App") {
    const fun = typeCheck(t.fun, scope);
    if (fun.type !== "TFun") {
      typeError(t, `Expected a function. found ${AST.printType(fun)}`);
    } else {
      const arg = typeCheck(t.arg, scope);
      if (!isSubtype(arg, fun.tyParam)) {
        typeError(
          t,
          `type ${AST.printType(arg)} is not assignable to ${AST.printType(
            fun.tyParam
          )}`
        );
      } else return fun.tyResult;
    }
  }
  if (t.type === "Op") {
    const inf = infixTable.find((inf) => inf.symbol === t.op);
    if (inf == null) throw new TypeError(`Unkown operator ${t.op}`);
    const [tyLeft, tyRight, tyRes] = inf.tySig;
    const left = typeCheck(t.left, scope);
    if (!isSubtype(left, tyLeft)) {
      typeError(
        t,
        `type ${AST.printType(tyLeft)} is not assignable to ${AST.printType(
          left
        )}`
      );
    }
    const right = typeCheck(t.right, scope);
    if (!isSubtype(right, tyRight)) {
      typeError(
        t,
        `type ${AST.printType(tyRight)} is not assignable to ${AST.printType(
          right
        )}`
      );
    }
    return tyRes;
  }
  if (t.type === "Let") {
    const def = typeCheck(t.definition, scope);
    const newScope = { ...scope, [t.name]: def };
    return typeCheck(t.body, newScope);
  }
  if (t.type === "Record") {
    const res = {};
    for (let key in t.dict) {
      const ty = typeCheck(t.dict[key], scope);
      res[key] = ty;
    }
    return AST.TRecord(res);
  }
  if (t.type === "Projection") {
    const rec = typeCheck(t.record, scope);
    if (rec.type !== "TRecord")
      typeError(t, `type ${AST.printType(rec)} is not a record type`);
    else {
      if (!(t.field in rec.dict))
        typeError(
          t,
          `type ${AST.printType(rec)} does not have field ${t.field}`
        );
      return rec.dict[t.field];
    }
  }
}

function typeError(t: AST.Term, msg: string) {
  throw new TypeError(`\n\n${msg}  in  \n\n${AST.printTerm(t)}\n`);
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

function printVal(v: Value) {
  if (typeof v === "number" || typeof v === "boolean") return v;
  if (typeof v === "function") return "<function>";
  return `{${Object.keys(v)
    .map((key) => `${key}: ${printVal(v[key])}`)
    .join(", ")}}`;
}

// quick hack to evaluate programs in developer console
(window as any)._eval = (s) => {
  const t = testParser(term, s);
  const ty = typeCheck(t);
  const val = evaluate(t);
  return `${printVal(val)} : ${AST.printType(ty)}`;
};

(window as any)._isSub = (s, t) => {
  return isSubtype(testParser(type, s), testParser(type, t));
};
