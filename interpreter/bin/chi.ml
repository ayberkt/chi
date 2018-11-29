open Chi_parser
open Chi_interpreter.Chi
open Printf

module IC = Core.In_channel
module OC = Core.Out_channel
module A  = Core.Array

let parse (c : in_channel) : AbsChi.exp =
    ParChi.pExp LexChi.token (Lexing.from_channel c)

let prompt s = print_string s; OC.flush OC.stdout

let showTree (t : AbsChi.exp) : string =
    (fun x -> ShowChi.show (ShowChi.showExp x)) t

let main () : unit =
  let open PrintChi in
  if A.length Sys.argv > 1 then
    if Sys.file_exists Sys.argv.(1) then
      let channel = open_in Sys.argv.(1) in
      let expr =
        try (parse channel)
        with BNFC_Util.Parse_error (start_pos, end_pos) ->
            Printf.printf "Parse error at %d.%d-%d.%d\n"
                start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
                end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol);
            exit 1
      in
      if not (isClosed [] expr) then
        (printf "Program not closed; cannot evaluate!\n"; exit 1)
      else
        (match eval_top expr with
          | Success v ->
              (printf "%s\n" (printTree prtExp v);
               printf "%s\n" (show v);
               exit 1)
          | Error s ->
            (printf "Error: %s.\n" s;
              flush stdout;
              exit 1))
    else
      (printf "No file named \"%s\".\n" Sys.argv.(1); exit 1)
  else
    (printf "%s\n" "Usage: chi.exe [filename]"; exit 1)

let () = main ()
