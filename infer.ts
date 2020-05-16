import * as AST from "./ast";
import { infixTable } from "./infix";

/*
  In this file we're going to implement an algorithm for type inference, the
  one we'll write here is called `Algorithm W`.

  To ge an intuition for the process, suppose we're trying to infer a type
  for `fn(f) => f(1) + 2`. As humans we can look at the term and guess some
  pieces of the puzzle immediately, but for a machine we need some `dumb`
  process to make the reasoning automatic. In the following we present an 
  informal example of such a process

  - We're in presence of a function, so we need to infer a type for the paramter
    and another for the result.

  - Initially, since we don't know yet the type of the parameter `f` we're going
    to assign it a type variable `a`. Next we're going to infer the type of the
    body using our assumption `{f: a}`. In the next the set of all current assumptions
    will be called `scope`. 
  
  - in the body f(1) + 2, `2` ovbiously has type `num`.
  
  - for `f(1)` we've an application of `f` to a `num` (`1`)
  
  - we also know that `f(1)` must have type `num` since it's used in an addition
  
  - `f` has type `a` according to the current scope but `f` is also a function 
    from `num` to some unkown type (at this stage), we're going to create a type
    variable `b` to stand for the result of `f`.

  - So for now we know that `f` has type `a` but also `num -> b`. We're going to
    `unify` those facts, (here) unification is the process of solving an equation between
    types (where the 2 types may contain type variables). Our equation is simple
    `a = num -> b` which  is already in a solved form. So by now we know that `f`
    has type `num -> b`, which means that `f(1)` has type `b`

  - After the unification the inference for `f(1)` will return the type `b`, but 
    it's not sufficient to return the type alone, we need also to propagate our
    current knwoledge (ie {a = num -> b}) to the rest of the inference process. So
    in addition to the type we also return our solved equations: we'll call those
    `Substitutions` (substitutions are sort of equations that contains only variables
    on the left side and no duplicate reference for the solved variables in all
    the equations: we can't have something like {a = num -> b, c = a -> num}, since
    we already know what `a` is, we'll rather have c = (num -> b) -> num).

  - So we know `f(1)` has type `b`, but since the left side of `+` must be a `num`
    we'll need to also unify `b` with `num`, ie solve { b = `num` }) which is also
    already in a solved form.

  - By now we know { a = num -> b } and { b = num }, **Composing** those 2 substitutions
    yield the substitution { a = num -> num }, which means `f` is of type `num -> num`
    and `f(1)` is of type `num`

  - `f(1) + 2` is of type `num` since the 2 sides are `num`s

  - `fn(f) => f(1) + 1` is of type `(num -> num) -> num` since `f` is a `num -> num`
     and the body is a `num`


  From the above example, we can get the general procedure:

  - As in type checking with explicit type annotations, we infer the type of a term
    from the types if its components
  
  - When we don't have a type (like in a function parameter), we create a type variable

  - Later, when we use the type variable (e.g. application or addition), we unify the 
    variable with the expected types. Note the process may yield types with yet to be
    known variables (like a = num -> b).

  - We continue this process, propagating our knowledge (Substitutions) and combining
    our known facts (Composing substitutions)

  - The final result will be a type and a final substituion which we apply to our type
    to get the combined result for all our knowledge
*/

type Scope = {
  [key: string]: AST.Scheme;
};

type Substitution = {
  [key: string]: AST.Type;
};

function applySubstToType(sub: Substitution, ty: AST.Type): AST.Type {
  if (ty.type === "TVar" && ty.name in sub) return sub[ty.name];
  if (ty.type === "TFun")
    return AST.TFun(
      applySubstToType(sub, ty.tyParam),
      applySubstToType(sub, ty.tyResult)
    );
  return ty;
}

function applySubstToScheme(sub: Substitution, sch: AST.Scheme): AST.Scheme {
  let restrictedSub = {};
  for (let key in sub) {
    if (sch.xs.has(key)) continue;
    restrictedSub[key] = sub[key];
  }
  return AST.Scheme(sch.xs, applySubstToType(restrictedSub, sch.type));
}

function applySubstToScope(sub: Substitution, scope: Scope): Scope {
  let newScope = {};
  for (let key in scope) {
    newScope[key] = applySubstToScheme(sub, scope[key]);
  }
  return newScope;
}

function composeSubst2(s1: Substitution, s2: Substitution): Substitution {
  let res = {};
  for (let key in s2) {
    res[key] = applySubstToType(s1, s2[key]);
  }
  return {
    ...res,
    ...s1,
  };
}

function composeSubsts(...ss: Substitution[]): Substitution {
  return ss.reduceRight((s2, s1) => composeSubst2(s1, s2));
}

let tid = 0;
function newTVar() {
  return AST.TVar(`t${++tid}`);
}

/**
 * Here we create a special copy of our generic type
 * Fore example if we have a generic type
 *        forall a. a -> a
 * We create a special type t -> t (renaming all occurrences of `a` to `t`)
 *
 * The difference between `a` and `t` may seem confusing, but consider that `a` is
 * like the parameter of a function, while `t` is the actual argument passed
 * to our function. It's a type variable because it has yet to be solved/unified.
 *
 * This is why in the signature we take a Scheme and return a Type
 */
function instantiate(sch: AST.Scheme): AST.Type {
  const subst = {};
  sch.xs.forEach((x) => {
    subst[x] = newTVar();
  });
  return applySubstToType(subst, sch.type);
}

/**
 * generalise is the inverse of instantiate, Here we abstract over `unused`
 * type variables. For example suppose we have the term (assuming we had pairs
 * in our language)
 *
 *  let f = fn(y) y in (f(1), f(true))
 *
 * When infering the type of `f` we get something like `a -> a` because we know
 * nothing special about the parameter `y`. Since `y` is entirely inernal to the
 * function, we know that `a` isn't used elsewhere (in fact our scope in the example
 * is empty {}). So we generalise the type `a -> a` into a scheme `forall a. a -> a`.
 *
 * When generalising we must be careful to not abstract over type variables that
 * are present in the scope. Those represent assumptions shared with other terms.
 * This is the why we call `isGeneric` to make sure that the type variables in the
 * type do not occur in any type inside our scope.
 */
function generalise(ty: AST.Type, scope: Scope): AST.Scheme {
  const ftvs = freeVars(ty);
  const gtvs = ftvs.filter((tv) => isGeneric(tv, scope));
  return AST.Scheme(new Set(gtvs), ty);
}

function freeVars(ty: AST.Type, acc: string[] = []): string[] {
  if (ty.type === "TVar") acc.push(ty.name);
  else if (ty.type === "TFun") {
    freeVars(ty.tyParam, acc);
    freeVars(ty.tyResult, acc);
  }
  return acc;
}

function isGeneric(tv: string, scope: Scope): boolean {
  for (let key in scope) {
    if (isFreeInScheme(tv, scope[key])) return false;
  }
  return true;
}

function isFreeInScheme(tv: string, sch: AST.Scheme): boolean {
  if (sch.xs.has(tv)) return false;
  return isFreeInType(tv, sch.type);
}

function isFreeInType(tv: string, ty: AST.Type): boolean {
  if (ty.type === "TVar" && ty.name === tv) return true;
  if (ty.type === "TFun")
    return isFreeInType(tv, ty.tyParam) || isFreeInType(tv, ty.tyResult);
  return false;
}

/**
 * This is where have the main inference process. Notice that each time have a
 * `usage form` (e.g. Application, addition, if) we immediately call `unify` and
 * try to gain some new knowledge about our type variables. Whatever returned
 * from `unify` gets mwrged with our current knowledge (by composing the Substitutons).
 * This is charachteristic of Algorithm W: It interleaves constraint generation and
 *  solving via heavy use of substitutions.
 *
 * Note the instantiation is systematic when we retreive a type from the scope.
 * This is like inlining the type definition inside its `usage site`.
 *
 * Note also that any knwoledge we gain in a branch (like when inferring if)
 * is propagated to the scope and other branches.
 */
function typeInferInScope(t: AST.Term, scope: Scope): [Substitution, AST.Type] {
  if (t.type === "Num") return [{}, AST.TNum];
  if (t.type === "Bool") return [{}, AST.TBool];
  if (t.type === "Var") {
    if (t.name in scope) return [{}, instantiate(scope[t.name])];
    throw new TypeError(`Unkown variable ${t.name}`);
  }
  if (t.type === "If") {
    const [s1, cond] = typeInferInScope(t.cond, scope);
    const u1 = unify(cond, AST.TBool);
    const su1 = composeSubsts(u1, s1);
    let subScope = applySubstToScope(su1, scope);
    const [s2, then] = typeInferInScope(t.then, subScope);
    subScope = applySubstToScope(s2, subScope);
    const [s3, elze] = typeInferInScope(t.elze, subScope);
    const u2 = unify(applySubstToType(s3, then), elze);
    return [composeSubsts(u2, s3, s2, su1), applySubstToType(u2, elze)];
  }
  if (t.type === "Fun") {
    const tvar = newTVar();
    const newScope = { ...scope, [t.paramName]: AST.Scheme(new Set([]), tvar) };
    const [s, tyRes] = typeInferInScope(t.body, newScope);
    return [s, AST.TFun(applySubstToType(s, tvar), tyRes)];
  }
  if (t.type === "App") {
    const [s1, fun] = typeInferInScope(t.fun, scope);
    const [s2, arg] = typeInferInScope(t.arg, applySubstToScope(s1, scope));
    const tvar = newTVar();
    const u = unify(applySubstToType(s2, fun), AST.TFun(arg, tvar));
    return [composeSubsts(u, s2, s1), applySubstToType(u, tvar)];
  }
  if (t.type === "Op") {
    const inf = infixTable.find((inf) => inf.symbol === t.op);
    if (inf == null) throw new TypeError(`Unkown operator ${t.op}`);

    const [tyLeft, tyRight, tyRes] = inf.tySig;
    const [s1, left] = typeInferInScope(t.left, scope);
    const u1 = unify(left, tyLeft);
    const su1 = composeSubsts(u1, s1);
    const scope1 = applySubstToScope(su1, scope);
    const [s2, right] = typeInferInScope(t.right, scope1);
    const u2 = unify(right, tyRight);
    return [composeSubsts(u2, s2, su1), tyRes];
  }
  if (t.type === "Let") {
    const [s1, def] = typeInferInScope(t.definition, scope);
    const scope1 = applySubstToScope(s1, scope);
    // Note we generalise only inside let definition, this is characteristic
    // of `Hindley Milner` type systems and called `let polymorphism`
    // Trying to generalise also in function parameters is, helas, not
    // possible, the inference process will become `undecidable`
    const gdef = generalise(def, scope1);
    const newScope = { ...scope1, [t.name]: gdef };
    const [s2, res] = typeInferInScope(t.body, newScope);
    return [composeSubsts(s2, s1), res];
  }
}

/**
 * The unification procedure. Note that in the variable case we rule our recursive
 * equations: i.e getting something like `a = num -> a` is forbidden.
 *
 * This test is known as `occurs check` in the literature. Occulting the test means
 * we're in the presence of `infinite types` (or simply `recursive types`). This is
 * the reason behind the message `can't build the infinite type ...`you may have
 * encountered in functional languages like Haskell, Elm, Ocaml (I think ocaml have
 * a cli flag to enable recursive type inference) ...
 */
function unify(ty1: AST.Type, ty2: AST.Type): Substitution {
  if (ty1.type === "TBool" && ty2.type === "TBool") return {};
  if (ty1.type === "TNum" && ty2.type === "TNum") return {};
  if (ty1.type === "TFun" && ty2.type === "TFun") {
    const u1 = unify(ty1.tyParam, ty2.tyParam);
    const u2 = unify(
      applySubstToType(u1, ty1.tyResult),
      applySubstToType(u1, ty2.tyResult)
    );
    return composeSubsts(u2, u1);
  }
  if (ty1.type === "TVar") {
    if (ty2.type === "TVar") {
      if (ty1.name === ty2.name) return {};
      else {
        return { [ty1.name]: ty2 };
      }
    } else {
      if (isFreeInType(ty1.name, ty2))
        throw new TypeError("Occurs check failed!");
      return { [ty1.name]: ty2 };
    }
  }
  if (ty2.type === "TVar") return unify(ty2, ty1);
  throw new TypeError(`
    Unable to unify ${AST.printType(ty1)} with ${AST.printType(ty2)}}
  `);
}

/**
 * i.e. `a -> b` instead of `t12 -> t12`
 */
let vars = "abcdefghijklmnopqrstuvwxyz";
function prettyGen(ty: AST.Type): AST.Type {
  let sub = {};
  let ftvs = freeVars(ty);
  ftvs.forEach((fv, i) => {
    if (fv in sub) return;
    sub[fv] = AST.TVar(vars[i]);
  });
  return applySubstToType(sub, ty);
}

export function typeInfer(t: AST.Term): AST.Type {
  const [sub, ty] = typeInferInScope(t, {});
  return prettyGen(applySubstToType(sub, ty));
}
