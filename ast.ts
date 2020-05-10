/**
 
In this file we define the abstract syntax of the language
*abstract* because we care about the core structure of the
language. The language is represented as a tree data structure, hence the
name `Abstract Syntax Tree` or AST

In contrast there is the *conrete syntax* which cares about
the linear representation of the program as a sequence of
characters. This is where we deal with issus such as parentheses,
associativity, priority of multiplication over addition.

Below, examples of phrases of our language

  simple math       : 1 + 2 * x
  function        : add(x: num) => fn(y: num) => x + y
  let expresions  : let inc = fn(x: num) => x + 1 in inc(2)
  

Our syntax includes 2 "syntactic categories" : 
  - terms : represent phrases that can be evaluated : 1 + 2, increment(10) ...
  - types : represent type annotations: num, num => num

  term :=    
    num (1,2, ...)
    bool (true, false)
    var
    term op term     
    if term then term else term
    fn(x: type) => term -- binding forms
    term(term)
    let x = term in term -- binding forms
    ( term )
    {tag: term, ...}     -- records
    term.tag             -- record projection
  
  type :=
    num
    bool
    type => type
    {tag: type, ...}
    ( type )
 */

/*
  Here we define the AST of our language
 */
export interface Dict<A> {
  [key: string]: A;
}

export type Term =
  | { type: "Num"; value: number }
  | { type: "Bool"; value: boolean }
  | { type: "Var"; name: string }
  | { type: "Op"; left: Term; op: string; right: Term }
  | { type: "If"; cond: Term; then: Term; elze: Term }
  | { type: "Fun"; paramName: string; paramType: Type; body: Term }
  | { type: "App"; fun: Term; arg: Term }
  | { type: "Let"; name: string; definition: Term; body: Term }
  | { type: "Record"; dict: Dict<Term> }
  | { type: "Projection"; record: Term; field: string };

export type Type =
  | { type: "TNum" }
  | { type: "TBool" }
  | { type: "TFun"; tyParam: Type; tyResult: Type }
  | { type: "TRecord"; dict: Dict<Type> };

/**
  here we define the data constructors: i.e. functions that will
  construct the various objects of our AST
 */
export function Num(value: number): Term {
  return { type: "Num", value };
}

export function Bool(value: boolean): Term {
  return { type: "Bool", value };
}

export function Var(name: string): Term {
  return { type: "Var", name };
}

export function Op(left: Term, op: string, right: Term): Term {
  return { type: "Op", left, op, right };
}

export function If(cond: Term, then: Term, elze: Term): Term {
  return { type: "If", cond, then, elze };
}

export function Fun(paramName: string, paramType: Type, body: Term): Term {
  return { type: "Fun", paramName, paramType, body };
}

export function App(fun: Term, arg: Term): Term {
  return { type: "App", fun, arg };
}

export function Let(name: string, definition: Term, body: Term): Term {
  return { type: "Let", name, definition, body };
}

export function Record(dict: Dict<Term>): Term {
  return { type: "Record", dict };
}

export function Projection(record: Term, field: string): Term {
  return { type: "Projection", record, field };
}

export const TNum: Type = { type: "TNum" };

export const TBool: Type = { type: "TBool" };

export function TFun(tyParam: Type, tyResult: Type): Type {
  return { type: "TFun", tyParam, tyResult };
}

export function TRecord(dict: Dict<Type>): Type {
  return { type: "TRecord", dict };
}

/**
  Determines if 2 types are equal
  We'll need this for type checking
 */
export function typeEq(ty1: Type, ty2: Type): boolean {
  if (ty1.type === "TNum" && ty2.type === "TNum") return true;
  if (ty1.type === "TBool" && ty2.type === "TBool") return true;
  if (ty1.type === "TFun" && ty2.type === "TFun") {
    return (
      typeEq(ty1.tyParam, ty2.tyParam) && typeEq(ty1.tyResult, ty2.tyResult)
    );
  }
  if (ty1.type === "TRecord" && ty2.type === "TRecord") {
    const keys1 = Object.keys(ty1.dict);
    const keys2 = Object.keys(ty2.dict);
    return (
      keys1.length === keys2.length &&
      keys1.every((key) => typeEq(ty1.dict[key], ty2.dict[key]))
    );
  }
  return false;
}

/*
  Poor man's pretty printing for terms and types
*/
export function printTerm(t: Term): string {
  if (t.type === "Num") return String(t.value);
  if (t.type === "Bool") return String(t.value);
  if (t.type === "Var") return t.name;
  if (t.type === "Fun") {
    return `fn(${t.paramName}: ${printType(t.paramType)}) => ${printTerm(
      t.body
    )}`;
  }
  if (t.type === "Op") {
    return `${printTerm(t.left)} ${t.op} ${printTerm(t.right)}`;
  }
  if (t.type === "If") {
    return `if ${printTerm(t.cond)} then ${printTerm(t.then)} else ${printTerm(
      t.elze
    )}`;
  }
  if (t.type === "App") return `${printTerm(t.fun)}(${printTerm(t.arg)})`;
  if (t.type === "Let") {
    return `let ${t.name} = ${printTerm(t.definition)} in\n ${printTerm(
      t.body
    )}`;
  }
  if (t.type === "Record") {
    return `{${Object.keys(t.dict)
      .map((key) => `${key}: ${printTerm(t.dict[key])}`)
      .join(", ")}}`;
  }
  if (t.type === "Projection") {
    return `${printTerm(t.record)}.${t.field}`;
  }
}

export function printType(ty: Type): string {
  if (ty.type === "TNum") return "num";
  if (ty.type === "TBool") return "bool";
  if (ty.type === "TFun") {
    const nest = ty.tyParam.type === "TFun";
    return `${nest ? "(" : ""}${printType(ty.tyParam)}${
      nest ? ")" : ""
    } => ${printType(ty.tyResult)}`;
  }
  if (ty.type === "TRecord") {
    return `{${Object.keys(ty.dict)
      .map((key) => `${key}: ${printType(ty.dict[key])}`)
      .join(", ")}}`;
  }
}
