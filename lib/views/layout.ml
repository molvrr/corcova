open Corcova.View

let make_view ~title elements = Corcova.Response.Html (html title elements)
