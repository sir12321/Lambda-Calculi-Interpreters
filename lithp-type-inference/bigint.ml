open Names

module BigNum : Names.BignumSig = struct
  type sign = Neg | NonNeg
  type bigint = sign * int list
  type myBool = T | F

  exception Div_by_zero
  exception Not_an_integer

  (* convert myBool to bool *)
  let myBool2Bool b = if b = T then true else false

  (* strip leading zeros, return [0] if empty *)
  let rec drop_leading_zeros digits =
    match digits with
    | [] -> [ 0 ]
    | 0 :: rest -> drop_leading_zeros rest
    | _ -> digits

  let correct_bigint (b : bigint) : bigint =
    let sign, digits = b in
    let d = drop_leading_zeros digits in
    if d = [ 0 ] then (NonNeg, [ 0 ]) else (sign, d)

  let absolute (b : bigint) : bigint =
    let _, digits = b in
    let d = drop_leading_zeros digits in
    (NonNeg, d)

  let is_zero (b : bigint) =
    let _, digits = b in
    drop_leading_zeros digits = [ 0 ]

  let unary_negation (b : bigint) =
    let sign, digits = b in
    if sign = Neg then correct_bigint (NonNeg, digits)
    else correct_bigint (Neg, digits)

  (* compare two digit lists, most significant first *)
  let compare_digits (l1 : int list) (l2 : int list) =
    let len1 = List.length l1 in
    let len2 = List.length l2 in
    if len1 <> len2 then if len1 > len2 then 1 else -1
    else
      let rec cmp a b =
        match (a, b) with
        | [], [] -> 0
        | x :: xs, y :: ys ->
            if x = y then cmp xs ys else if x > y then 1 else -1
        | _ -> 0
      in
      cmp l1 l2

  let compare_bigint (a : bigint) (b : bigint) =
    let sign_1, digits_1 = correct_bigint a in
    let sign_2, digits_2 = correct_bigint b in
    if sign_1 = NonNeg && sign_2 = Neg then 1
    else if sign_1 = Neg && sign_2 = NonNeg then -1
    else if sign_1 = NonNeg && sign_2 = NonNeg then
      compare_digits digits_1 digits_2
    else (* both negative *)
      -compare_digits digits_1 digits_2

  let equal a b = if compare_bigint a b = 0 then T else F
  let greater_than a b = if compare_bigint a b > 0 then T else F
  let less_than a b = if compare_bigint a b < 0 then T else F
  let great_or_equal a b = if compare_bigint a b >= 0 then T else F
  let less_or_equal a b = if compare_bigint a b <= 0 then T else F

  let pretty_print (b : bigint) =
    let sign, digits = b in
    let digit_strs = List.map string_of_int digits in
    let digits_string = List.fold_left (fun acc s -> acc ^ s) "" digit_strs in
    if sign = Neg && not (digits = [ 0 ]) then "-" ^ digits_string
    else digits_string

  (* convert a regular int to bigint *)
  let int_to_bigint (n : int) : bigint =
    let sign = if n < 0 then Neg else NonNeg in
    let abs_n = if n < 0 then -n else n in
    let str = string_of_int abs_n in
    let len = String.length str in
    let rec build_digits i acc =
      if i >= len then acc
      else
        let d = Char.code str.[i] - Char.code '0' in
        if d < 0 || d > 9 then raise Not_an_integer
        else build_digits (i + 1) (acc @ [ d ])
    in
    let digits = build_digits 0 [] in
    correct_bigint (sign, digits)

  let rec add_list_rev list_1 list_2 carry =
    match (list_1, list_2, carry) with
    | [], [], c -> if c = 0 then [] else [ c ]
    | x :: xs, [], c ->
        let sum = x + c in
        (sum mod 10) :: add_list_rev xs [] (sum / 10)
    | [], y :: ys, c ->
        let sum = y + c in
        (sum mod 10) :: add_list_rev [] ys (sum / 10)
    | x :: xs, y :: ys, c ->
        let s = x + y + c in
        (s mod 10) :: add_list_rev xs ys (s / 10)

  let add_lists list_1 list_2 =
    let rev1 = List.rev list_1 in
    let rev2 = List.rev list_2 in
    let result_rev = add_list_rev rev1 rev2 0 in
    drop_leading_zeros (List.rev result_rev)

  let sub_lists list_1 list_2 =
    (* assumes list_1 >= list_2 *)
    let rec sub_rev l1 l2 borrow =
      match (l1, l2, borrow) with
      | [], [], _ -> []
      | x :: xs, [], b ->
          let diff = x - b in
          if diff < 0 then (diff + 10) :: sub_rev xs [] 1
          else diff :: sub_rev xs [] 0
      | x :: xs, y :: ys, b ->
          let diff = x - y - b in
          if diff < 0 then (diff + 10) :: sub_rev xs ys 1
          else diff :: sub_rev xs ys 0
      | _ -> raise (Invalid_argument "sub_lists: list_1 must be >= list_2")
    in
    drop_leading_zeros
      (List.rev (sub_rev (List.rev list_1) (List.rev list_2) 0))

  let addition (a : bigint) (b : bigint) : bigint =
    let sign_1, digits_1 = a in
    let sign_2, digits_2 = b in
    if sign_1 = sign_2 then correct_bigint (sign_1, add_lists digits_1 digits_2)
    else begin
      let cmp = compare_digits digits_1 digits_2 in
      if cmp = 0 then (NonNeg, [ 0 ])
      else if cmp > 0 then correct_bigint (sign_1, sub_lists digits_1 digits_2)
      else correct_bigint (sign_2, sub_lists digits_2 digits_1)
    end

  let subtraction (a : bigint) (b : bigint) : bigint =
    let neg_b = unary_negation b in
    addition a neg_b

  let multiplication (a : bigint) (b : bigint) =
    let sign_1, digits_1 = a in
    let sign_2, digits_2 = b in
    if digits_1 = [ 0 ] || digits_2 = [ 0 ] then (NonNeg, [ 0 ])
    else
      let rec repeat_0 n = if n <= 0 then [] else 0 :: repeat_0 (n - 1) in
      let rec mul_digit_rev lst d carry =
        match (lst, carry) with
        | [], c -> if c = 0 then [] else [ c ]
        | x :: xs, c ->
            let prod = (x * d) + c in
            (prod mod 10) :: mul_digit_rev xs d (prod / 10)
      in
      let mult_lists l1 l2 =
        let rev1 = List.rev l1 in
        let rev2 = List.rev l2 in
        let rec loop l2 shift acc =
          match l2 with
          | [] -> acc
          | d :: rest ->
              let partial = mul_digit_rev rev1 d 0 in
              let shifted = repeat_0 shift @ partial in
              let acc' = add_list_rev acc shifted 0 in
              loop rest (shift + 1) acc'
        in
        drop_leading_zeros (List.rev (loop rev2 0 []))
      in
      let sign_final = if sign_1 = sign_2 then NonNeg else Neg in
      correct_bigint (sign_final, mult_lists digits_1 digits_2)

  (* division helper: returns (quotient digits, remainder digits), both positive *)
  let q_r_abs a b =
    if b = [ 0 ] then raise Div_by_zero
    else if compare_digits a b < 0 then ([ 0 ], a)
    else if compare_digits a b = 0 then ([ 1 ], [ 0 ])
    else
      let mul_by_digit digits d =
        if d = 0 then [ 0 ]
        else
          let rec mul_rev lst carry =
            match lst with
            | [] -> if carry = 0 then [] else [ carry ]
            | x :: xs ->
                let p = (x * d) + carry in
                (p mod 10) :: mul_rev xs (p / 10)
          in
          drop_leading_zeros (List.rev (mul_rev (List.rev digits) 0))
      in
      (* find the largest digit 0-9 s.t. b*q_d <= rem *)
      let rec find_q_d rem q =
        if q < 0 then 0
        else
          let prod = mul_by_digit b q in
          if compare_digits prod rem <= 0 then q else find_q_d rem (q - 1)
      in
      let rec q_r_loop remaining rem q_acc =
        match remaining with
        | [] -> (drop_leading_zeros q_acc, drop_leading_zeros rem)
        | x :: xs ->
            let rem' =
              if rem = [ 0 ] then [ x ] else drop_leading_zeros (rem @ [ x ])
            in
            let q_d = find_q_d rem' 9 in
            let new_rem =
              if q_d = 0 then rem' else sub_lists rem' (mul_by_digit b q_d)
            in
            q_r_loop xs new_rem (q_acc @ [ q_d ])
      in
      q_r_loop a [ 0 ] []

  let quotient (a : bigint) (b : bigint) =
    if is_zero (correct_bigint b) then raise Div_by_zero
    else
      let sign_1, digits_1 = correct_bigint a in
      let sign_2, digits_2 = correct_bigint b in
      let q_digits, _ = q_r_abs digits_1 digits_2 in
      let sign = if sign_1 = sign_2 then NonNeg else Neg in
      correct_bigint (sign, q_digits)

  let remainder (a : bigint) (b : bigint) =
    if is_zero (correct_bigint b) then raise Div_by_zero
    else
      let sign_1, digits_1 = correct_bigint a in
      let _, digits_2 = correct_bigint b in
      let _, r_digits = q_r_abs digits_1 digits_2 in
      correct_bigint (sign_1, r_digits)

  let string_to_bigint (s : string) : bigint = int_to_bigint (int_of_string s)
end
