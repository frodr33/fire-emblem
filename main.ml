open Types

module Html = Dom_html
let js = Js.string (* partial function, takes in string *)
let document = Html.document

(************************ DOM HELPERS ************************)

(* [fail] is a failure/exception handler *)
let fail = fun _ -> assert false

(* [get_element_by_id id] gets a DOM element by its id *)
let get_element_by_id id =
  Js.Opt.get (Html.document##getElementById (js id)) fail

(* [append_text e s] appends string s to element e *)
let append_text e s = Dom.appendChild e (document##createTextNode (js s))



(* [main ()] is begins game execution by building the
 * GUI and then calling the REPL function *)
let main () =
  let gui = get_element_by_id "gui" in
  let h1 = Html.createH1 document in
  append_text h1 "Fire Emblem"
