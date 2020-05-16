/**
  
  Since we're going to infer the types of the terms we need the following
  adjustements to our AST

    - terms : we don't need type annotations in functions anymore

    - types : 
        
        + types can now have variables to denote unkown types (e.g. a -> num). Since
          we don't have type annotations for function paramters anymore, we'll
          have to introduce temporary variables in place of `concrete types`. As 
          explained in `infer.ts` we'll try to solve for those variables just like
          we do in School Algebra.

        + we need to add a new syntactic experession for `Generic types`, those
          are like Generic functions you encounter in TypeScript, here, they're
          called `Schemes`. For exampe the term `fn(x) => fn (y) => 2` can be assigned
          a Scheme `forall a b. a -> b -> num`. The scheme bind the names `a` amd `b` 
          inside its body `a -> b -> num`, you may envision Schemes as sort of functions 
          from types to types: just as we can apply a function to terms, we can 
          `instantiate` a Scheme with types: In the above example we can instantiate
          `a` and `b` to `num` and `bool` resp., we get then the type `num -> bool -> num`

  
  term :=    
    num
    bool
    var
    term op term     
    fn(x) => term           -- note we got rid of the param's type    
    term(term)
    let x = term in term
    ( term )
    
  type :=
    num
    bool
    tvar
    type => type
    ( type )

  scheme :=
    forall xs. type
 */

export type Term =
  | { type: "Num"; value: number }
  | { type: "Bool"; value: boolean }
  | { type: "Var"; name: string }
  | { type: "Op"; left: Term; op: string; right: Term }
  | { type: "If"; cond: Term; then: Term; elze: Term }
  | { type: "Fun"; paramName: string; body: Term }
  | { type: "App"; fun: Term; arg: Term }
  | { type: "Let"; name: string; definition: Term; body: Term };

export type Type =
  | { type: "TNum" }
  | { type: "TBool" }
  | { type: "TVar"; name: string }
  | { type: "TFun"; tyParam: Type; tyResult: Type };

export type Scheme = { xs: Set<string>; type: Type };

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

export function Fun(paramName: string, body: Term): Term {
  return { type: "Fun", paramName, body };
}

export function App(fun: Term, arg: Term): Term {
  return { type: "App", fun, arg };
}

export function Let(name: string, definition: Term, body: Term): Term {
  return { type: "Let", name, definition, body };
}

export const TNum: Type = { type: "TNum" };

export const TBool: Type = { type: "TBool" };

export function TVar(name: string): Type {
  return { type: "TVar", name };
}

export function TFun(tyParam: Type, tyResult: Type): Type {
  return { type: "TFun", tyParam, tyResult };
}

export function Scheme(xs: Set<string>, type: Type): Scheme {
  return { xs, type };
}

/**
  Determines if 2 types are equal
  We'll need this for type checking
 */
export function typeEq(ty1: Type, ty2: Type): boolean {
  if (ty1.type === "TNum" && ty2.type === "TNum") return true;
  if (ty1.type === "TBool" && ty2.type === "TBool") return true;
  if (ty1.type === "TVar" && ty2.type === "TVar") return ty1.name === ty2.name;
  if (ty1.type === "TFun" && ty2.type === "TFun") {
    return (
      typeEq(ty1.tyParam, ty2.tyParam) && typeEq(ty1.tyResult, ty2.tyResult)
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
    return `fn(${t.paramName}) => ${printTerm(t.body)}`;
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
}

export function printType(ty: Type): string {
  if (ty.type === "TNum") return "num";
  if (ty.type === "TBool") return "bool";
  if (ty.type === "TVar") return ty.name;
  if (ty.type === "TFun") {
    const nest = ty.tyParam.type === "TFun";
    return `${nest ? "(" : ""}${printType(ty.tyParam)}${
      nest ? ")" : ""
    } => ${printType(ty.tyResult)}`;
  }
}

export function printScheme(sch: Scheme): string {
  let vars = [];
  for (let v of sch.xs) {
    vars.push(v);
  }
  return `∀(${vars.join(" ")}).${printType(sch.type)}`;
}
