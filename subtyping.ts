import { Type } from "./ast";

/**

  In system without subtyping we rely on simple equality between types,
  types are regarded are like isolated islands with no possible correspondance
  betwwen different islands. Two types are "compatible" if they habit the
  same island.

  In a system with subtyping, we allow unidirectional "bridges" between
  those islands. We can also view this islands forming a sort of hierarchy,
  with the possibility for one type to be promoted into another type.

  This function `canPromote` is the one to decide "who deserves" to be
  promoted and who don't.
 */

export function isSubtype(tyFrom: Type, tyTo: Type): boolean {
  if (tyFrom.type === "TNum" && tyTo.type === "TNum") return true;
  if (tyFrom.type === "TBool" && tyTo.type === "TBool") return true;
  if (tyFrom.type === "TFun" && tyTo.type === "TFun") {
    return (
      // notice the sense of the paramter types is inverted
      // for t1 -> t2 to be usable as s1 -> s2
      // the first function must accept all the inputs of
      // the second function. So t1 must be more permissive
      // than s1
      isSubtype(tyTo.tyParam, tyFrom.tyParam) &&
      isSubtype(tyFrom.tyResult, tyTo.tyResult)
    );
  }
  if (tyFrom.type === "TRecord" && tyTo.type === "TRecord") {
    for (let key in tyTo.dict) {
      if (!(key in tyFrom.dict)) return false;
      if (!isSubtype(tyFrom.dict[key], tyTo.dict[key])) return false;
    }
    return true;
  }
  return false;
}

/*
  Computes the greatest lower bound of 2 types, i.e. the minimum
  type that 
*/
export function meet(ty1: Type, ty2: Type): Type | null {
  if (isSubtype(ty1, ty2)) return ty2;
  if (isSubtype(ty2, ty1)) return ty1;
  // Here we could more like intersect 2 record types
  // eg for {a: num, x: bool} and {a:num, y: num} we could
  // return something like {a:num}. I don't know which is better
  // I think the latter option makes more sense in systems
  // with intersection types
  return null;
}
