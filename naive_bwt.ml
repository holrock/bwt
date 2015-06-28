open Core.Std

let tail_char = Char.of_int_exn 0

let make_suffix_array text =
  let s = text ^ String.make 1 tail_char in
  String.foldi ~init:[] ~f:(fun i acc _ ->
    (i, String.drop_prefix s i) :: acc) s
  |> List.sort ~cmp:(fun (_, a) (_, b) -> String.compare a b)
  |> List.map ~f:(fun (i, _) -> i)


let bwt text sa =
  List.map ~f:(fun sai ->
    if sai = 0 then tail_char
    else text.[sai - 1])
  sa

let restore_bwt tb =
  let lfmap =
    List.zip_exn (List.mapi tb ~f:(fun i _ -> i)) tb
    |> List.stable_sort ~cmp:(fun (_, a) (_, b) -> Char.compare a b)
    |> Array.of_list_map ~f:(fun (i, _) -> i)
  in
  let pos = ref (match List.findi tb ~f:(fun _ c -> c = tail_char) with
    | Some (i, _) -> i
    | None -> assert false)
  in
  let len = Array.length lfmap in
  let buf = Buffer.create len in
  let tb_array = Array.of_list tb in
  for _ = 0 to len - 2 do
    pos := lfmap.(!pos);
    Buffer.add_char buf tb_array.(!pos)
  done;
  Buffer.contents buf

let test text =
  let sa = make_suffix_array text in
  print_endline (List.to_string ~f:Int.to_string sa);
  assert (sa = [11; 10; 7; 0; 3; 5; 8; 1; 4; 6; 9; 2]);
  let tb = bwt text sa in
  let expect = ['a'; 'r'; 'd'; tail_char; 'r'; 'c'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'] in
  print_endline (List.to_string ~f:Char.to_string tb);
  assert (tb = expect);
  let restore = restore_bwt tb in
  print_endline restore;
  assert (restore = "abracadabra")

let _ =
  let text = "abracadabra" in
  test text;
