open Ppxlib

let expand_temp ~ctxt _ =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Ast_builder.Default.(estring ~loc "Abc")

let expand ~ctxt var_name _ expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let open Ast_builder.Default in
  pexp_let ~loc Nonrecursive
    [
      value_binding ~loc
        ~pat:(ppat_var ~loc { loc; txt = var_name })
        ~expr:(estring ~loc "Science, Mr. White!");
    ]
    expr

let my_extension =
  Extension.V3.declare "chor" Extension.Context.expression
    Ast_pattern.(
      single_expr_payload
        (pexp_let nonrecursive
           (value_binding ~pat:(ppat_var __) ~expr:(estring __) ^:: nil)
           __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] "chor"
