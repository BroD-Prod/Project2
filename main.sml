(* Brody Roth, Timothy Gulley *)
(* 03/26/2026 *)
(* This is an sml program that checks if *)
(* a given string is a palindrome. *)

fun is_uppercase ch =
    (* Takes a character and returns true if it is an uppercase letter. *)
    ord(ch) >= ord(#"A") andalso ord(ch) <= ord(#"Z");

fun is_lowercase ch =
    (* Takes a character and returns true if it is a lowercase letter. *)
    ord(ch) >= ord(#"a") andalso ord(ch) <= ord(#"z");

fun reverse_string(list) = 
    (* if string is empty, return empty list *)
    let fun reverser([], reversed_list) = reversed_list
    (* if string is not empty, add first char to reversed list, then call itself recursively *)
    (* reverses the string, by adding each char to front of new list, and the returns the new list as a string*)
          | reverser(x::xs, reversed_list) = reverser(xs, x::reversed_list)
    in 
        reverser(list, []) 
    end;

fun process_string(string) = 
    (* if string is empty, return empty list *)
    let fun processer([], processed_list) = processed_list
          | processer(x::xs, processed_list) = 
           (* if x is uppercase, convert to lowercase using ASCII values *)
           if is_uppercase x then processer(xs, (chr(ord(x) + 32) :: processed_list)) 
           (* if x is a lowercase letter, add it to the processed list *)
           else if is_lowercase x then processer(xs, x :: processed_list)
            (* if x is not a letter, ignore it *)
           else processer(xs, processed_list)
           (* since adding string to new list, returns list in reverse order*)
           (* we process string normally to get reverse string without non-letter characters *)
    in 
        processer(explode string, [])
    end;

fun is_palindrome string = 
    (* Checks if the given string is a palindrome. *)
    let val cleaned_string = process_string string
    in 
        (* reverse string again to get clean string in the normal order *)
        cleaned_string = reverse_string cleaned_string
    end;