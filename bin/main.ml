open Lib

let repeated_msg = 
  "# "

let is_str = ref true

let macros = ref []

let () = 
  Printexc.print (fun () ->
    try
      while true do
        try
          print_string repeated_msg;
          let str = 
            List.fold_right
            (fun (key, body) str -> 
              Str.global_replace (Str.regexp ("%" ^ key ^ "%")) body str)
            !macros (read_line ())
          in
          let term = Parser.program_term Lexer.token (Lexing.from_string (str ^ "\n")) in
          let ctree = Infer.make_ctree (Typing_env.empty) term in
          let cond = Cond_tree.to_cond ctree in
          if !is_str
            then
              (output_string (open_out ("\"" ^ (Term.to_str term) ^ "\"" ^ ".txt")) (Cond_tree.to_str ctree);
              print_string ("> " ^ (Condition.to_str cond) ^ "\n"))
            else
              (output_string (open_out ((Term.to_str term) ^ ".txt")) (Cond_tree.to_tex ctree);
              print_string ("> " ^ (Condition.to_tex cond) ^ "\n"))
        with
        | Lexer.RecTime n -> (Infer.set_rec n; print_endline "* set rectime")
        | Lexer.TypeDeclarement type_string -> 
          print_endline type_string; let ty = Parser.program_type Lexer.token (Lexing.from_string (type_string ^ "\n")) in
          Tag_world.push ty
        | Lexer.AsTeX -> is_str:=false;print_endline "* print as tex-command"
        | Lexer.AsString -> is_str:=true;print_endline "* print as string"
        | Lexer.Macro (key, body) ->
          let temp = !macros in
          macros:=(key, body)::temp;
          print_endline ("* macro defined: " ^ key ^ "=" ^ body)
      done
    with
      Lexer.Quit -> print_endline "* quit"; exit 0
  ) ()