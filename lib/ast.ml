type ast
  = Abs of ast * ast
  | App of ast * ast
  | Var of char ;;

let e1 = Abs (Var 'x', Var 'x')