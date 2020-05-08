import { Type, TFun, TNum, TBool, Term, Op } from "./ast";
import { setState } from "pcomb";

export enum Assoc {
  Left,
  Right,
  None,
}

type TySig = [Type, Type, Type];

type Infix = {
  symbol: string;
  assoc: Assoc;
  priority: number;
  tySig: TySig;
};

export function infix(
  symbol: string,
  assoc: Assoc,
  priority: number,
  tySig: TySig
): Infix {
  return { symbol, assoc, priority, tySig };
}

// Type signatures
const tyArith: TySig = [TNum, TNum, TNum]; // arithmetic
const tyComp: TySig = [TNum, TNum, TBool]; // comparaison
const tyBool: TySig = [TBool, TBool, TBool]; // boolean

export const infixTable = [
  infix("&&", Assoc.Left, 10, tyBool),
  infix("||", Assoc.Left, 11, tyBool),
  infix("==", Assoc.None, 20, tyComp),
  infix("!=", Assoc.None, 20, tyComp),
  infix("<=", Assoc.None, 20, tyComp),
  infix(">=", Assoc.None, 20, tyComp),
  infix("<", Assoc.None, 20, tyComp),
  infix(">", Assoc.None, 20, tyComp),
  infix("+", Assoc.Left, 30, tyArith),
  infix("-", Assoc.Left, 30, tyArith),
  infix("*", Assoc.Left, 40, tyArith),
  infix("/", Assoc.Left, 40, tyArith),
  infix("**", Assoc.Left, 50, tyArith),
];

function isLower(inf1: Infix, inf2: Infix): boolean {
  return inf1.priority < inf2.priority || inf1.assoc === Assoc.Right;
}

export function buildInfix(
  t2: Term,
  rest: Array<[string, Term]>,
  stack: Array<[Infix, Term]>
): Term {
  // t [] [] => t
  if (rest.length === 0 && stack.length === 0) return t2;
  // t2 ((op,t3):xs) [] => t3 xs [(t2,op)]
  else if (stack.length === 0) {
    const [[sym, t3], ...xs] = rest;
    const inf = infixTable.find((i) => i.symbol === sym);
    if (inf == null) throw new Error(`Unkown operator ${sym}`);
    return buildInfix(t3, xs, [[inf, t2]]);
  }

  // t2 [] [[]: (t1, op1)]  => t1 op1 t2
  // t2 [] [ys: (t,op) (t1,op1)]  => t1 op1 t2
  //      op < op1    ? (t1 op1 t2) [] [ys: (t,op)]
  //      op > op1    ? t2 [ys: (op1, (t op t1)]
  else if (rest.length === 0) {
    const [inf1, t1] = stack[stack.length - 1];
    if (stack.length === 1) {
      return Op(t1, inf1.symbol, t2);
    } else {
      const [inf, t] = stack[stack.length - 2];
      if (isLower(inf, inf1))
        return buildInfix(
          Op(t1, inf1.symbol, t2),
          rest,
          stack.slice(0, stack.length - 1)
        );
      else
        return buildInfix(
          t2,
          rest,
          stack
            .slice(0, stack.length - 2)
            .concat([[inf1, Op(t, inf.symbol, t1)]])
        );
    }
  }

  // t2 ((op2, t3):xs) [ys:(op1, t1)] =>
  //     op1 < op2 ? t3 xs [ys: (op1,t1) (op2,t2)]
  //     op1 > op2 ? (t1 op1 t2) ((op2, t3):xs) ys
  else {
    const [[sym, t3], ...xs] = rest;
    const inf2 = infixTable.find((i) => i.symbol === sym);
    if (inf2 == null) throw new Error(`Unkown operator ${sym}`);

    const [inf1, t1] = stack[stack.length - 1];
    if (isLower(inf1, inf2))
      return buildInfix(t3, xs, stack.concat([[inf2, t2]]));
    else
      return buildInfix(
        Op(t1, inf1.symbol, t2),
        rest,
        stack.slice(0, stack.length - 1)
      );
  }
}
