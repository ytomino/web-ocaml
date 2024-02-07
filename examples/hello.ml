let print_substring = output_substring stdout;;
Web.header_content_type print_substring Web.text_html;;
Web.header_break print_substring ();;
print_string "<!DOCTYPE html>";;
print_string "<html";;
let ac = Web.HTML.open_attribute `html5 print_substring "lang" in
Web.HTML.attribute_output_string ac "en";
Web.HTML.close_attribute ac;;
print_string ">";;
print_string "<body>";;
let tc = Web.HTML.open_text `html5 print_substring in
Web.HTML.text_output_string tc "Hello, World!";
Web.HTML.close_text tc;;
print_string "</body>";;
print_string "</html>";;
print_newline ();;
