(** Simple visualizer for skylines *)

(*  draw_line : out_stream -> int -> int -> int -> int -> unit  *)
let draw_line ostr x y dx dy = Printf.fprintf ostr "%i %i %i %i\n" x y dx dy

(*  draw_skyline : (int * int) list -> unit  *)
let draw_skyline sk =
  let ostr = open_out "skyline.dat" in
  let rec walk sk = match sk with
    | []      -> ()
    | [(x,h)] -> ()
    | (x1,h1)::(x2,h2)::rest ->
      begin
	draw_line ostr x1 h1 (x2-x1) 0;
	draw_line ostr x2 h1 0 (h2-h1);
	walk ((x2,h2)::rest)
      end
  in
  match sk with
    | [] -> ()
    | (x1,h1)::_ ->
      begin
	draw_line ostr x1 0 0 h1;
	walk sk;
	close_out ostr
      end

(*  draw_buildings : (int * int * int) list -> unit  *)
let draw_buildings l =
  let ostr = open_out "lines.dat" in
  begin
    List.iter (fun (x,h,l) ->
      begin
	draw_line ostr x 0 0 h;
	draw_line ostr x h l 0;
	draw_line ostr (x+l) h 0 (-h);
      end) l;
    close_out ostr
  end

(*  visualize : (int * int * int) list -> unit  *)
let visualize l =
  begin
    draw_buildings l;
    draw_skyline (Skyline.skyline l);
  end

(*  random_city : unit -> (int * int * int) list *)
let random_city () =
  let len = (Random.int 15) + 1 in
  let rec build l = match l with
    | 0 -> []
    | _ ->
      let x = (Random.int 9) + 1 in
      let h = (Random.int 9) + 1 in
      let l = (Random.int 9) + 1 in
      (x,h,l)::(build (l - 1))
  in build len
