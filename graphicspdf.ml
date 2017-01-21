open Pdfutil

exception Graphic_failure of string;;

type command =
  | SetRGBColor of int * int * int
  | Plot of int * int
  | MoveTo of int * int
  | LineTo of int * int
  | RMoveTo of int * int
  | RLineTo of int * int
  | CurveTo of (int * int) * (int * int) * (int * int)
  | DrawRect of int * int * int * int
  | FillRect of int * int * int * int
  | DrawPoly of (int * int) array
  | FillPoly of (int * int) array
  | DrawArc of int * int * int * int * int * int
  | FillArc of int * int * int * int * int * int
  | DrawEllipse of int * int * int * int
  | FillEllipse of int * int * int * int
  | DrawCircle of int * int * int
  | FillCircle of int * int * int
  | SetLineWidth of int
  | DrawString of string
  | SetFont of Pdftext.standard_font
  | SetFontSize of int

let wintitle = ref ""

let commands = ref []

let output = ref stdout

let open_pdf filename = output := open_out_bin filename

let open_pdf_channel channel = output := channel

let add_command c =
  commands := c :: !commands

let opened = ref false

let sizex = ref 640
let sizey = ref 640

(* Modified from GraphPS by Pierre Weis *)
let parse_geometry s =
  let lim = String.length s in
  let rec find_x i =
   if i >= lim then raise Not_found else
   if s.[i] = 'x' then i else find_x (i + 1) in
  let rec find_digit i =
   if i >= lim then raise Not_found else
   match s.[i] with
   | '0' .. '9' -> i
   | _ -> find_digit (i + 1) in
  try
   let ix = find_x 0 in
   let dx = find_digit 0 in
   let sx = String.sub s dx (ix - dx) in
   let dy = find_digit ix in
   let sy = String.sub s dy (lim - dy) in
   int_of_string sx, int_of_string sy
  with
  | Not_found | Failure _ -> (640, 640);;

let open_graph spec =
  let x, y = parse_geometry spec in
    sizex := x;
    sizey := y;
    opened := true

let foi = float_of_int

let recompress_stream pdf = function
  (* If there is no compression, compress with /FlateDecode *)
  | Pdf.Stream {contents = (dict, _)} as stream ->
      begin match
        Pdf.lookup_direct pdf "/Filter" dict, 
        Pdf.lookup_direct pdf "/Type" dict
      with
      | _, Some (Pdf.Name "/Metadata") -> ()
      | (None | Some (Pdf.Array [])), _ ->
          Pdfcodec.encode_pdfstream pdf Pdfcodec.Flate stream
      | _ -> ()
      end
  | _ -> assert false

let recompress_pdf pdf =
  if not (Pdfcrypt.is_encrypted pdf) then
    Pdf.iter_stream (recompress_stream pdf) pdf;
    pdf

let font = ref Pdftext.TimesRoman

let fontsize = ref 12

(* Width comes from Fonttables.textwidth, height is just points. *)
let text_size str =
  try
    (int_of_float (ceil (float (Pdfstandard14.textwidth false Pdftext.StandardEncoding !font str * !fontsize) /. 1000.)), !fontsize)
  with
    _ -> raise (Graphic_failure "text_size")

(* Build the PDF *)
let cx = ref 0
let cy = ref 0
let linewidth = ref 0
let red = ref 0
let green = ref 0
let blue = ref 0

let floatcol x = foi x /. 255.0

let attributes () =
  [Pdfops.Op_w (foi !linewidth);
   Pdfops.Op_RG (floatcol !red, floatcol !green, floatcol !blue);
   Pdfops.Op_rg (floatcol !red, floatcol !green, floatcol !blue);
   Pdfops.Op_j 0 ]

let add_font = function
  | Pdftext.TimesRoman -> "/F0"
  | Pdftext.TimesBold -> "/F1"
  | Pdftext.TimesItalic -> "/F2"
  | Pdftext.TimesBoldItalic -> "/F3"
  | Pdftext.Helvetica -> "/F4"
  | Pdftext.HelveticaBold -> "/F5"
  | Pdftext.HelveticaOblique -> "/F6"
  | Pdftext.HelveticaBoldOblique -> "/F7"
  | Pdftext.Courier -> "/F8"
  | Pdftext.CourierBold -> "/F9"
  | Pdftext.CourierOblique -> "/F10"
  | Pdftext.CourierBoldOblique -> "/F11"
  | Pdftext.Symbol -> "/F12"
  | Pdftext.ZapfDingbats -> "/F13"

let mkfontdict font =
  Pdf.Dictionary
    [("/Type", Pdf.Name "/Font");
     ("/Subtype", Pdf.Name "/Type1");
     ("/BaseFont", Pdf.Name ("/" ^ Pdftext.string_of_standard_font font))]

let resources =
  Pdf.Dictionary
    [("/Font",
        Pdf.Dictionary
           ["/F0", mkfontdict Pdftext.TimesRoman;
            "/F1", mkfontdict Pdftext.TimesBold;
            "/F2", mkfontdict Pdftext.TimesItalic;
            "/F3", mkfontdict Pdftext.TimesBoldItalic;
            "/F4", mkfontdict Pdftext.Helvetica;
            "/F5", mkfontdict Pdftext.HelveticaBold;
            "/F6", mkfontdict Pdftext.HelveticaOblique;
            "/F7", mkfontdict Pdftext.HelveticaBoldOblique;
            "/F8", mkfontdict Pdftext.Courier;
            "/F9", mkfontdict Pdftext.CourierBold;
            "/F10", mkfontdict Pdftext.CourierOblique;
            "/F11", mkfontdict Pdftext.CourierBoldOblique;
            "/F12", mkfontdict Pdftext.Symbol;
            "/F13", mkfontdict Pdftext.ZapfDingbats])]
 
let rec ops_of_command = function
  | SetRGBColor (r, g, b) ->
      red := r;
      green := g;
      blue := b;
      []
  | Plot (x, y) ->
     attributes () @
     [Pdfops.Op_J 1;
      Pdfops.Op_j 1;
      Pdfops.Op_w 1.0;
      Pdfops.Op_m (foi x, foi y);
      Pdfops.Op_l (foi x, foi y);
      Pdfops.Op_h;
      Pdfops.Op_S]
  | MoveTo (x, y) ->
     cx := x; cy := y; []
  | LineTo (x, y) ->
      let ocx = !cx and ocy = !cy in
        cx := x; cy := y;
      attributes () @
      [Pdfops.Op_m (foi ocx, foi ocy);
       Pdfops.Op_l (foi x, foi y);
       Pdfops.Op_S]
  | RMoveTo (x, y) ->
      ops_of_command (MoveTo (x + !cx, y + !cy))
  | RLineTo (x, y) ->
      ops_of_command (LineTo (x + !cx, y + !cy))
  | CurveTo ((a, b), (c, d), (e, f)) ->
      let ocx = !cx and ocy = !cy in
      cx := e; cy := f;
      attributes () @
      [Pdfops.Op_m (foi ocx, foi ocy);
       Pdfops.Op_c (foi a, foi b, foi c, foi d, foi e, foi f);
       Pdfops.Op_S]
  | DrawRect (x, y, w, h) ->
      attributes () @
      [Pdfops.Op_m (foi x, foi y);
       Pdfops.Op_re (foi x, foi y, foi w, foi h);
       Pdfops.Op_S]
  | FillRect (x, y, w, h) ->
      attributes () @
      [Pdfops.Op_m (foi x, foi y);
       Pdfops.Op_re (foi x, foi y, foi w, foi h);
       Pdfops.Op_f]
  | DrawPoly points ->
     begin match points with [||] -> [] | _ ->
       attributes () @
       (* Move to first point *)
       [Pdfops.Op_m (foi (fst points.(0)), foi (snd points.(0)))] @
       List.flatten
         (Array.to_list
            (Array.map
               (fun (x, y) ->
                  [Pdfops.Op_l (foi x, foi y)])
               points)) @
       (* Close and Stroke *)
       [Pdfops.Op_h; Pdfops.Op_S]
     end
  | FillPoly points ->
     begin match points with [||] -> [] | _ ->
       attributes () @
       (* Move to first point *)
       [Pdfops.Op_m (foi (fst points.(0)), foi (snd points.(0)))] @
       List.flatten
         (Array.to_list
            (Array.map
               (fun (x, y) ->
                  [Pdfops.Op_l (foi x, foi y)])
               points)) @
       (* Close and Stroke *)
       [Pdfops.Op_h; Pdfops.Op_f']
     end
  | DrawArc (a, b, c, d, e, f) -> []
  | FillArc (a, b, c, d, e, f) -> []
  | DrawEllipse (x, y, rx, ry) ->
      let attr =
        {Pdfgraphics.path_transform =
          Pdftransform.matrix_of_op
            (Pdftransform.Scale ((foi x, foi y), 1.0, (foi ry /. foi rx)));
         Pdfgraphics.path_fill = None;
         Pdfgraphics.path_line =
           Some (Pdfspace.DeviceRGB,
             Pdfgraphics.Floats [floatcol !red; floatcol !green; floatcol !blue]);
         Pdfgraphics.path_linewidth = foi !linewidth;
         Pdfgraphics.path_joinstyle = 0;
         Pdfgraphics.path_capstyle = 0;
         Pdfgraphics.path_dash = ([], 1.0);
         Pdfgraphics.path_mitrelimit = 10.;
         Pdfgraphics.path_transparency =
           {Pdfgraphics.fill_transparency = 1.0;
            Pdfgraphics.line_transparency = 1.0};
         Pdfgraphics.path_intent = "/RelativeColorimetric"}
      in
      let graphic =
        {Pdfgraphics.elements = [Pdfgraphics.Path (Pdfshapes.circle (foi x) (foi y) (foi rx), attr)];
         Pdfgraphics.fonts = [];
         Pdfgraphics.resources = Pdf.Dictionary []}
      in
        attributes () @
        Pdfgraphics.ops_of_simple_graphic graphic
  | FillEllipse (x, y, rx, ry) ->
      let attr =
        {Pdfgraphics.path_transform =
          Pdftransform.matrix_of_op
            (Pdftransform.Scale ((foi x, foi y), 1.0, (foi ry /. foi rx)));
         Pdfgraphics.path_fill =
           Some (Pdfspace.DeviceRGB,
             Pdfgraphics.Floats [floatcol !red; floatcol !green; floatcol !blue]);
         Pdfgraphics.path_line = None;
         Pdfgraphics.path_linewidth = 0.;
         Pdfgraphics.path_joinstyle = 0;
         Pdfgraphics.path_capstyle = 0;
         Pdfgraphics.path_dash = ([], 1.0);
         Pdfgraphics.path_mitrelimit = 10.;
         Pdfgraphics.path_transparency =
           {Pdfgraphics.fill_transparency = 1.0;
            Pdfgraphics.line_transparency = 1.0};
         Pdfgraphics.path_intent = "/RelativeColorimetric"}
      in
      let graphic =
        {Pdfgraphics.elements = [Pdfgraphics.Path (Pdfshapes.circle (foi x) (foi y) (foi rx), attr)];
         Pdfgraphics.fonts = [];
         Pdfgraphics.resources = Pdf.Dictionary []}
      in
        attributes () @
        Pdfgraphics.ops_of_simple_graphic graphic
  | DrawCircle (x, y, r) ->
      let attr =
        {Pdfgraphics.path_transform = Pdftransform.i_matrix;
         Pdfgraphics.path_fill = None;
         Pdfgraphics.path_line =
           Some (Pdfspace.DeviceRGB,
             Pdfgraphics.Floats [floatcol !red; floatcol !green; floatcol !blue]);
         Pdfgraphics.path_linewidth = foi !linewidth;
         Pdfgraphics.path_joinstyle = 0;
         Pdfgraphics.path_capstyle = 0;
         Pdfgraphics.path_dash = ([], 1.0);
         Pdfgraphics.path_mitrelimit = 10.;
         Pdfgraphics.path_transparency =
           {Pdfgraphics.fill_transparency = 1.0;
            Pdfgraphics.line_transparency = 1.0};
         Pdfgraphics.path_intent = "/RelativeColorimetric"}
      in
      let graphic =
        {Pdfgraphics.elements = [Pdfgraphics.Path (Pdfshapes.circle (foi x) (foi y) (foi r), attr)];
         Pdfgraphics.fonts = [];
         Pdfgraphics.resources = Pdf.Dictionary []}
      in
        attributes () @
        Pdfgraphics.ops_of_simple_graphic graphic
  | FillCircle (x, y, r) ->
      let attr =
        {Pdfgraphics.path_transform = Pdftransform.i_matrix;
         Pdfgraphics.path_fill =
           Some (Pdfspace.DeviceRGB,
             Pdfgraphics.Floats [floatcol !red; floatcol !green; floatcol !blue]);
         Pdfgraphics.path_line = None;
         Pdfgraphics.path_linewidth = 0.;
         Pdfgraphics.path_joinstyle = 0;
         Pdfgraphics.path_capstyle = 0;
         Pdfgraphics.path_dash = ([], 1.0);
         Pdfgraphics.path_mitrelimit = 10.;
         Pdfgraphics.path_transparency =
           {Pdfgraphics.fill_transparency = 1.0;
            Pdfgraphics.line_transparency = 1.0};
         Pdfgraphics.path_intent = "/RelativeColorimetric"}
      in
      let graphic =
        {Pdfgraphics.elements = [Pdfgraphics.Path (Pdfshapes.circle (foi x) (foi y) (foi r), attr)];
         Pdfgraphics.fonts = [];
         Pdfgraphics.resources = Pdf.Dictionary []}
      in
        attributes () @
        Pdfgraphics.ops_of_simple_graphic graphic
  | SetLineWidth i ->
      linewidth := i; []
  | DrawString s ->
      let width = fst (text_size s)
      and ops =
        attributes () @
        [Pdfops.Op_q;
         Pdfops.Op_BT;
         Pdfops.Op_Tm (Pdftransform.matrix_of_op (Pdftransform.Translate (foi !cx, foi !cy)));
         Pdfops.Op_Tf (add_font !font, foi !fontsize);
         Pdfops.Op_Tj s;
         Pdfops.Op_ET;
         Pdfops.Op_Q]
      in
        cx := !cx + width;
        ops
  | SetFont f ->
      font := f; []
  | SetFontSize i ->
      fontsize := i; []

let ops_of_commands commands =
  List.flatten (List.map ops_of_command commands) 

let build_pdf () =
  let page =
    {(Pdfpage.custompage (Pdf.Array [Pdf.Integer 0; Pdf.Integer 0; Pdf.Integer !sizex; Pdf.Integer !sizey])) with
       Pdfpage.content = [Pdfops.stream_of_ops (ops_of_commands (List.rev !commands))];
       Pdfpage.resources = resources}
  in
    let pdf, pageroot = Pdfpage.add_pagetree [page] (Pdf.empty ()) in
      let pdf = Pdfpage.add_root pageroot [] pdf in
        (* Add title *)
        let info =
          Pdf.addobj pdf (Pdf.Dictionary ["/Title", Pdf.String !wintitle])
        in
          pdf.Pdf.trailerdict <-
            Pdf.add_dict_entry pdf.Pdf.trailerdict "/Info" (Pdf.Indirect info);
          recompress_pdf pdf

let close_graph () =
  try
    let pdf = build_pdf () in
      Pdfwrite.pdf_to_channel false None false pdf !output;
      close_out !output;
      output := stdout;
      opened := false
  with
    _ -> raise (Graphic_failure "close_graph")

let size_x () = !sizex
let size_y () = !sizey

let clear_graph () =
  commands := []

type color = int

let rgb r g b =
  r lsl 16 + g lsl 8 + b

let rgb_of_color c =
  (c lsr 16) land 0xFF, (c lsr 8) land 0xFF, c land 0xFF

let black   = rgb 0 0 0
let white   = rgb 255 255 255
let red     = rgb 255 0 0
let green   = rgb 0 255 0
let blue    = rgb 0 0 255
let yellow  = rgb 255 255 0
let cyan    = rgb 0 255 255
let magenta = rgb 255 0 255

let background = white

let foreground = black

let current_rgb = ref min_int

let set_color c =
  let c = if c < 0 then background else c in
    let r, g, b = rgb_of_color c in
      current_rgb := c;
      add_command (SetRGBColor (r, g, b))

let plot x y = add_command (Plot (x, y))

let plots points =
  Array.iter (function (x, y) -> plot x y) points

let curr_x = ref 0
let curr_y = ref 0

let current_x () = !curr_x
let current_y () = !curr_y

let current_point () =
  !curr_x, !curr_y

let set_point x y =
  curr_x := x;
  curr_y := y

let moveto x y =
 set_point x y;
 add_command (MoveTo (x, y));;

let lineto x y =
 set_point x y;
 add_command (LineTo (x, y));;

let rlineto dx dy = 
 set_point (current_x () + dx) (current_y () + dy);
 add_command (RLineTo (dx, dy));;

let rmoveto dx dy =
 set_point (current_x () + dx) (current_y () + dy);
 add_command (RMoveTo (dx, dy));;

let curveto (x1, y1 as b) (x2, y2 as c) (x3, y3 as d) =
 add_command (CurveTo (b, c, d));
 set_point x3 y3;;

let draw_arc x y rx ry a1 a2 = add_command (DrawArc (x, y, rx, ry, a1, a2));;
let draw_ellipse x y rx ry = add_command (DrawEllipse (x, y, rx, ry));;
let draw_circle x y r = add_command (DrawCircle (x, y, r));;

let set_line_width w = add_command (SetLineWidth w);;

let draw_rect x y w h = add_command (DrawRect (x, y, w, h));;
let fill_rect x y w h = add_command (FillRect (x, y, w, h));;

let fill_poly v = add_command (FillPoly v);;
let draw_poly v = add_command (DrawPoly v);;
let draw_poly_line =
  let draw points =
    if Array.length points > 0 then begin
      let (savex, savey) = current_point () in
      moveto (fst points.(0)) (snd points.(0));
      for i = 1 to Array.length points - 1 do
        let (x, y) = points.(i) in
        lineto x y;
      done;
      moveto savex savey;
    end in
  draw;;

let draw_segments segs =
  let (savex, savey) = current_point () in
  for i = 0 to Array.length segs - 1 do
    let (x1, y1, x2, y2) = segs.(i) in
    moveto x1 y1;
    lineto x2 y2;
  done;
  moveto savex savey;;

let fill_arc x y rx ry a1 a2 = add_command (FillArc (x, y, rx, ry, a1, a2));;

let fill_ellipse x y rx ry = add_command (FillEllipse (x, y, rx, ry));;
let fill_circle x y r = add_command (FillCircle (x, y, r));;

let draw_char c = add_command (DrawString (string_of_char c))

let draw_string s = add_command (DrawString s)

let set_font s =
  let font =
    match s with
    | "Times" -> Pdftext.TimesRoman
    | "Times-Bold" -> Pdftext.TimesBold
    | "Times-Italic" -> Pdftext.TimesItalic
    | "Times-BoldItalic" -> Pdftext.TimesBoldItalic
    | "Helvetica" -> Pdftext.Helvetica
    | "Helvetica-Bold" -> Pdftext.HelveticaBold
    | "Helvetica-Italic" -> Pdftext.HelveticaOblique
    | "Helvetica-BoldItalic" -> Pdftext.HelveticaBoldOblique
    | "Courier" -> Pdftext.Courier
    | "Courier-Bold" -> Pdftext.CourierBold
    | "Courier-Italic" -> Pdftext.CourierOblique
    | "Courier-BoldItalic" -> Pdftext.CourierBoldOblique
    | "Symbol" -> Pdftext.Symbol
    | "ZapfDingbats" -> Pdftext.ZapfDingbats
    | _ -> raise (Invalid_argument (Printf.sprintf "font %s name not known" s))
  in
    add_command (SetFont font)
  
let set_text_size s = add_command (SetFontSize s)

type image

let transp = ~-1
 
let set_window_title s =
  wintitle := s

