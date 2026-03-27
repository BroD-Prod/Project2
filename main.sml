(* Brody Roth, Timothy Gulley *)
(* 03/26/2026 *)
(* This is an sml program that checks if *)
(* a given string is a palindrome. *)



fun reverse_string(string) = 
    (* if string is empty, return imploded empty list *)
    let fun reverser([], reversed_list) = implode(reversed_list)
    (* if string is not empty, add first char to reversed list, then call itself recursively *)
    (* reverses the string, by adding each char to front of new list, and the returns the new list as a string*)
          | reverser(x::xs, reversed_list) = reverser(xs, x::reversed_list)
    in 
    (* explode "string" into a list using the helper function reverser, reverser does as shown above*)
        reverser(explode string, []) 
    end;

fun is_uppercase ch =
    (* Takes a character and returns true if it is an uppercase letter. *)
    ord(ch) >= ord(#"A") andalso ord(ch) <= ord(#"B");

fun is_lowercase ch =
    (* Takes a character and returns true if it is a lowercase letter. *)
    ord(ch) >= ord(#"a") andalso ord(ch) <= ord(#"b");

fun remove_non_alphabetic(string) = 
    (* if string is empty, return imploded cleaned list *)
    let fun remover([], cleaned_list) = implode(cleaned_list);
    in 
        remover(explode string, [])
    end;

fun change_to_lowercase _ = _;
    (* if string is empty, return imploded cleaned list *)
    let fun lower([], cleaned_list) = implode(cleaned_list);
    in 
        lower(explode string, [])
    end;

fun is_palindrome string = reverse_string string = string;