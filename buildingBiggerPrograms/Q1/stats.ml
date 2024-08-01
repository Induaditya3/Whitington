try
  begin
    match Sys.argv with
    [|_;fname|] ->
      let s = Textstat.stats_from_file fname in 
      print_string "Lines : ";
      print_int (Textstat.lines s);
      print_string "\nSentences : ";
      print_int (Textstat.sentences s);
      print_string "\nWords : ";
      print_int (Textstat.words s);
      print_string "\nCharacters : ";
      print_int (Textstat.characters s);
      print_newline ();
      print_string "Occurences of each character \n";
      Textstat.histogram_print (Textstat.histogram s);
    |_ -> print_string "Usage: ./histo <filename/filepath>"; print_newline ()
  end
with e ->
  print_string "An error occured.";
  print_string (Printexc.to_string e);
  print_newline ();
  exit 1


