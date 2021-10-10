type var = string

type typ =
  | Typ_int8
  | Typ_pointer
  | Typ_var of string

type datacon = {
  datacon_name : string;
  datacon_params : string list;
  datacon_fields : typ list;
}

type datatype = {
  datacons : datacon list;
}

type exp =
  | Exp_tup of exp list
  | Exp_var of var

type pat =
  | Pat_con of string * pat list
  | Pat_num of int
  | Pat_var of var
  | Pat_any

type prop =
  [ `emp
  | `sep of prop * prop
  | `points_to of var * value
  | `exists of var * typ * prop
  | `tt
  | `ff
  | `conj of prop * prop
  | `disj of prop * prop
  | `imp of prop * prop ]

and value =
  | Val_con of string * value list
  | Val_var of string

type cmd =
  | Cmd_alloca of var * cmd
  | Cmd_call of string * value list
  | Cmd_cons of var * string * value list
  | Cmd_move of var * var
  | Cmd_dispose of var
  | Cmd_mat of var * (pat * cmd list) list
  | Cmd_seq of cmd * cmd

type proc = {
  proc_name : string;
  proc_params : string list;
  proc_pre : prop;
  proc_body : cmd;
  proc_post : prop;
}

type dec =
  | Dec_datatype of datatype
  | Dec_proc of proc
