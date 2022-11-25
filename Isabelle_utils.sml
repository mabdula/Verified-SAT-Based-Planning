structure exported : sig
  type num
  type int
  type nat
  type char
  datatype ('a, 'b) sum = Inl of 'a | Inr of 'b
  val integer_of_int : int -> IntInf.int
  val nat : int -> nat
  val integer_of_nat : nat -> IntInf.int
  val concat : ('a list) list -> 'a list
  val implode : char list -> string
  val max_var : int list -> int
  val explode : string -> char list
  val nat_of_integer : IntInf.int -> nat
  val char_of_nat : nat -> char
  val size_list : 'a list -> nat
  val nat_opt_of_integer : IntInf.int -> nat option
end = struct

datatype num = One | Bit0 of num | Bit1 of num;

datatype int = Zero_int | Pos of num | Neg of num;

val one_inta : int = Pos One;

type 'a one = {one : 'a};
val one = #one : 'a one -> 'a;

val one_int = {one = one_inta} : int one;

type 'a zero = {zero : 'a};
val zero = #zero : 'a zero -> 'a;

val zero_int = {zero = Zero_int} : int zero;

type 'a zero_neq_one = {one_zero_neq_one : 'a one, zero_zero_neq_one : 'a zero};
val one_zero_neq_one = #one_zero_neq_one : 'a zero_neq_one -> 'a one;
val zero_zero_neq_one = #zero_zero_neq_one : 'a zero_neq_one -> 'a zero;

val zero_neq_one_int =
  {one_zero_neq_one = one_int, zero_zero_neq_one = zero_int} : int zero_neq_one;

val one_integera : IntInf.int = (1 : IntInf.int);

val one_integer = {one = one_integera} : IntInf.int one;

val zero_integer = {zero = (0 : IntInf.int)} : IntInf.int zero;

type 'a ord = {less_eq : 'a -> 'a -> bool, less : 'a -> 'a -> bool};
val less_eq = #less_eq : 'a ord -> 'a -> 'a -> bool;
val less = #less : 'a ord -> 'a -> 'a -> bool;

val ord_integer =
  {less_eq = (fn a => fn b => IntInf.<= (a, b)),
    less = (fn a => fn b => IntInf.< (a, b))}
  : IntInf.int ord;

val zero_neq_one_integer =
  {one_zero_neq_one = one_integer, zero_zero_neq_one = zero_integer} :
  IntInf.int zero_neq_one;

datatype nat = Nat of IntInf.int;

datatype char = Chara of bool * bool * bool * bool * bool * bool * bool * bool;

datatype ('a, 'b) sum = Inl of 'a | Inr of 'b;

fun id x = (fn xa => xa) x;

fun dup (Neg n) = Neg (Bit0 n)
  | dup (Pos n) = Pos (Bit0 n)
  | dup Zero_int = Zero_int;

fun plus_num (Bit1 m) (Bit1 n) = Bit0 (plus_num (plus_num m n) One)
  | plus_num (Bit1 m) (Bit0 n) = Bit1 (plus_num m n)
  | plus_num (Bit1 m) One = Bit0 (plus_num m One)
  | plus_num (Bit0 m) (Bit1 n) = Bit1 (plus_num m n)
  | plus_num (Bit0 m) (Bit0 n) = Bit0 (plus_num m n)
  | plus_num (Bit0 m) One = Bit1 m
  | plus_num One (Bit1 n) = Bit0 (plus_num n One)
  | plus_num One (Bit0 n) = Bit1 n
  | plus_num One One = Bit0 One;

fun times_num (Bit1 m) (Bit1 n) =
  Bit1 (plus_num (plus_num m n) (Bit0 (times_num m n)))
  | times_num (Bit1 m) (Bit0 n) = Bit0 (times_num (Bit1 m) n)
  | times_num (Bit0 m) (Bit1 n) = Bit0 (times_num m (Bit1 n))
  | times_num (Bit0 m) (Bit0 n) = Bit0 (Bit0 (times_num m n))
  | times_num One n = n
  | times_num m One = m;

fun times_int (Neg m) (Neg n) = Pos (times_num m n)
  | times_int (Neg m) (Pos n) = Neg (times_num m n)
  | times_int (Pos m) (Neg n) = Neg (times_num m n)
  | times_int (Pos m) (Pos n) = Pos (times_num m n)
  | times_int Zero_int l = Zero_int
  | times_int k Zero_int = Zero_int;

fun less_eq_num (Bit1 m) (Bit0 n) = less_num m n
  | less_eq_num (Bit1 m) (Bit1 n) = less_eq_num m n
  | less_eq_num (Bit0 m) (Bit1 n) = less_eq_num m n
  | less_eq_num (Bit0 m) (Bit0 n) = less_eq_num m n
  | less_eq_num (Bit1 m) One = false
  | less_eq_num (Bit0 m) One = false
  | less_eq_num One n = true
and less_num (Bit1 m) (Bit0 n) = less_num m n
  | less_num (Bit1 m) (Bit1 n) = less_num m n
  | less_num (Bit0 m) (Bit1 n) = less_eq_num m n
  | less_num (Bit0 m) (Bit0 n) = less_num m n
  | less_num One (Bit1 n) = true
  | less_num One (Bit0 n) = true
  | less_num m One = false;

fun less_eq_int (Neg k) (Neg l) = less_eq_num l k
  | less_eq_int (Neg k) (Pos l) = true
  | less_eq_int (Neg k) Zero_int = true
  | less_eq_int (Pos k) (Neg l) = false
  | less_eq_int (Pos k) (Pos l) = less_eq_num k l
  | less_eq_int (Pos k) Zero_int = false
  | less_eq_int Zero_int (Neg l) = false
  | less_eq_int Zero_int (Pos l) = true
  | less_eq_int Zero_int Zero_int = true;

fun uminus_int (Neg m) = Pos m
  | uminus_int (Pos m) = Neg m
  | uminus_int Zero_int = Zero_int;

fun bitM One = One
  | bitM (Bit0 n) = Bit1 (bitM n)
  | bitM (Bit1 n) = Bit1 (Bit0 n);

fun minus_int (Neg m) (Neg n) = sub n m
  | minus_int (Neg m) (Pos n) = Neg (plus_num m n)
  | minus_int (Pos m) (Neg n) = Pos (plus_num m n)
  | minus_int (Pos m) (Pos n) = sub m n
  | minus_int Zero_int l = uminus_int l
  | minus_int k Zero_int = k
and sub (Bit0 m) (Bit1 n) = minus_int (dup (sub m n)) one_inta
  | sub (Bit1 m) (Bit0 n) = plus_int (dup (sub m n)) one_inta
  | sub (Bit1 m) (Bit1 n) = dup (sub m n)
  | sub (Bit0 m) (Bit0 n) = dup (sub m n)
  | sub One (Bit1 n) = Neg (Bit0 n)
  | sub One (Bit0 n) = Neg (bitM n)
  | sub (Bit1 m) One = Pos (Bit0 m)
  | sub (Bit0 m) One = Pos (bitM m)
  | sub One One = Zero_int
and plus_int (Neg m) (Neg n) = Neg (plus_num m n)
  | plus_int (Neg m) (Pos n) = sub n m
  | plus_int (Pos m) (Neg n) = sub m n
  | plus_int (Pos m) (Pos n) = Pos (plus_num m n)
  | plus_int Zero_int l = l
  | plus_int k Zero_int = k;

fun divmod_step_int l (q, r) =
  (if less_eq_int (Pos l) r
    then (plus_int (times_int (Pos (Bit0 One)) q) one_inta, minus_int r (Pos l))
    else (times_int (Pos (Bit0 One)) q, r));

fun divmod_int (Bit1 m) (Bit1 n) =
  (if less_num m n then (Zero_int, Pos (Bit1 m))
    else divmod_step_int (Bit1 n) (divmod_int (Bit1 m) (Bit0 (Bit1 n))))
  | divmod_int (Bit0 m) (Bit1 n) =
    (if less_eq_num m n then (Zero_int, Pos (Bit0 m))
      else divmod_step_int (Bit1 n) (divmod_int (Bit0 m) (Bit0 (Bit1 n))))
  | divmod_int (Bit1 m) (Bit0 n) =
    let
      val (q, r) = divmod_int m n;
    in
      (q, plus_int (times_int (Pos (Bit0 One)) r) one_inta)
    end
  | divmod_int (Bit0 m) (Bit0 n) = let
                                     val (q, r) = divmod_int m n;
                                   in
                                     (q, times_int (Pos (Bit0 One)) r)
                                   end
  | divmod_int One (Bit1 n) = (Zero_int, Pos One)
  | divmod_int One (Bit0 n) = (Zero_int, Pos One)
  | divmod_int (Bit1 m) One = (Pos (Bit1 m), Zero_int)
  | divmod_int (Bit0 m) One = (Pos (Bit0 m), Zero_int)
  | divmod_int One One = (Pos One, Zero_int);

fun snd (x1, x2) = x2;

fun equal_num (Bit0 x2) (Bit1 x3) = false
  | equal_num (Bit1 x3) (Bit0 x2) = false
  | equal_num One (Bit1 x3) = false
  | equal_num (Bit1 x3) One = false
  | equal_num One (Bit0 x2) = false
  | equal_num (Bit0 x2) One = false
  | equal_num (Bit1 x3) (Bit1 y3) = equal_num x3 y3
  | equal_num (Bit0 x2) (Bit0 y2) = equal_num x2 y2
  | equal_num One One = true;

fun equal_int (Neg k) (Neg l) = equal_num k l
  | equal_int (Neg k) (Pos l) = false
  | equal_int (Neg k) Zero_int = false
  | equal_int (Pos k) (Neg l) = false
  | equal_int (Pos k) (Pos l) = equal_num k l
  | equal_int (Pos k) Zero_int = false
  | equal_int Zero_int (Neg l) = false
  | equal_int Zero_int (Pos l) = false
  | equal_int Zero_int Zero_int = true;

fun adjust_mod l r = (if equal_int r Zero_int then Zero_int else minus_int l r);

fun modulo_int (Neg m) (Neg n) = uminus_int (snd (divmod_int m n))
  | modulo_int (Pos m) (Neg n) =
    uminus_int (adjust_mod (Pos n) (snd (divmod_int m n)))
  | modulo_int (Neg m) (Pos n) = adjust_mod (Pos n) (snd (divmod_int m n))
  | modulo_int (Pos m) (Pos n) = snd (divmod_int m n)
  | modulo_int k (Neg One) = Zero_int
  | modulo_int k (Pos One) = Zero_int
  | modulo_int Zero_int k = Zero_int
  | modulo_int k Zero_int = k;

fun fst (x1, x2) = x1;

fun of_bool A_ true = one (one_zero_neq_one A_)
  | of_bool A_ false = zero (zero_zero_neq_one A_);

fun adjust_div (q, r) =
  plus_int q (of_bool zero_neq_one_int (not (equal_int r Zero_int)));

fun divide_int (Neg m) (Neg n) = fst (divmod_int m n)
  | divide_int (Pos m) (Neg n) = uminus_int (adjust_div (divmod_int m n))
  | divide_int (Neg m) (Pos n) = uminus_int (adjust_div (divmod_int m n))
  | divide_int (Pos m) (Pos n) = fst (divmod_int m n)
  | divide_int k (Neg One) = uminus_int k
  | divide_int k (Pos One) = k
  | divide_int Zero_int k = Zero_int
  | divide_int k Zero_int = Zero_int;

fun less_int (Neg k) (Neg l) = less_num l k
  | less_int (Neg k) (Pos l) = true
  | less_int (Neg k) Zero_int = true
  | less_int (Pos k) (Neg l) = false
  | less_int (Pos k) (Pos l) = less_num k l
  | less_int (Pos k) Zero_int = false
  | less_int Zero_int (Neg l) = false
  | less_int Zero_int (Pos l) = true
  | less_int Zero_int Zero_int = false;

fun integer_of_int k =
  (if less_int k Zero_int then IntInf.~ (integer_of_int (uminus_int k))
    else (if equal_int k Zero_int then (0 : IntInf.int)
           else let
                  val l =
                    IntInf.* ((2 : IntInf.int), integer_of_int
          (divide_int k (Pos (Bit0 One))));
                  val j = modulo_int k (Pos (Bit0 One));
                in
                  (if equal_int j Zero_int then l
                    else IntInf.+ (l, (1 : IntInf.int)))
                end));

fun max A_ a b = (if less_eq A_ a b then b else a);

fun nat k = Nat (max ord_integer (0 : IntInf.int) (integer_of_int k));

fun integer_of_nat (Nat x) = x;

fun plus_nat m n = Nat (IntInf.+ (integer_of_nat m, integer_of_nat n));

val one_nat : nat = Nat (1 : IntInf.int);

fun suc n = plus_nat n one_nat;

fun fold f (x :: xs) s = fold f xs (f x s)
  | fold f [] s = s;

fun foldr f [] = id
  | foldr f (x :: xs) = f x o foldr f xs;

fun concat xss = foldr (fn a => fn b => a @ b) xss [];

fun map f [] = []
  | map f (x21 :: x22) = f x21 :: map f x22;

fun integer_of_char (Chara (b0, b1, b2, b3, b4, b5, b6, b7)) =
  IntInf.+ (IntInf.* (IntInf.+ (IntInf.* (IntInf.+ (IntInf.* (IntInf.+ (IntInf.* (IntInf.+ (IntInf.* (IntInf.+ (IntInf.* (IntInf.+ (IntInf.* (of_bool
                        zero_neq_one_integer
                        b7, (2 : IntInf.int)), of_bool zero_neq_one_integer
         b6), (2 : IntInf.int)), of_bool zero_neq_one_integer
                                   b5), (2 : IntInf.int)), of_bool
                     zero_neq_one_integer
                     b4), (2 : IntInf.int)), of_bool zero_neq_one_integer
       b3), (2 : IntInf.int)), of_bool zero_neq_one_integer
                                 b2), (2 : IntInf.int)), of_bool
                   zero_neq_one_integer
                   b1), (2 : IntInf.int)), of_bool zero_neq_one_integer b0);

fun implode cs =
  (String.implode
    o List.map (fn k => if 0 <= k andalso k < 128 then (Char.chr o IntInf.toInt) k else raise Fail "Non-ASCII character in literal"))
    (map integer_of_char cs);

fun gen_length n (x :: xs) = gen_length (suc n) xs
  | gen_length n [] = n;

fun abs_int i = (if less_int i Zero_int then uminus_int i else i);

fun max_var xs =
  fold (fn x => fn y =>
         (if less_eq_int (abs_int y) (abs_int x) then abs_int x else y))
    xs Zero_int;

fun bit_cut_integer k =
  (if ((k : IntInf.int) = (0 : IntInf.int)) then ((0 : IntInf.int), false)
    else let
           val (r, s) =
             IntInf.divMod (IntInf.abs k, IntInf.abs (2 : IntInf.int));
         in
           ((if IntInf.< ((0 : IntInf.int), k) then r
              else IntInf.- (IntInf.~ r, s)),
             ((s : IntInf.int) = (1 : IntInf.int)))
         end);

fun char_of_integer k = let
                          val (q0, b0) = bit_cut_integer k;
                          val (q1, b1) = bit_cut_integer q0;
                          val (q2, b2) = bit_cut_integer q1;
                          val (q3, b3) = bit_cut_integer q2;
                          val (q4, b4) = bit_cut_integer q3;
                          val (q5, b5) = bit_cut_integer q4;
                          val (q6, b6) = bit_cut_integer q5;
                          val a = bit_cut_integer q6;
                          val (_, aa) = a;
                        in
                          Chara (b0, b1, b2, b3, b4, b5, b6, aa)
                        end;

fun explode s =
  map char_of_integer
    ((List.map (fn c => let val k = Char.ord c in if k < 128 then IntInf.fromInt k else raise Fail "Non-ASCII character in literal" end) 
       o String.explode)
      s);

val zero_nat : nat = Nat (0 : IntInf.int);

fun nat_of_integer k = Nat (max ord_integer (0 : IntInf.int) k);

fun char_of_nat x = (char_of_integer o integer_of_nat) x;

fun size_list x = gen_length zero_nat x;

fun nat_opt_of_integer i =
  (if IntInf.<= ((0 : IntInf.int), i) then SOME (nat_of_integer i) else NONE);

end; (*struct exported*)
