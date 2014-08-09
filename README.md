Skyline solver
==============

This is a divide-and-conquer implementation of the Skyline problem.
It is structured into two modules: one for solving the problem proper
and one for generating random input cities, invoking the solver, and
emitting output for gnuplot.


We represent both cities and skylines as lists:
- A city is represented as a (xvalue,house_height,house_length) list.
- A skyline is represented as a (xvalue,height) list.


To compute skylines recursively, we split the input city in two, and
merge the resulting skylines.

To merge two skylines, we
 - overlay one skyline over another, recording the height of each "interesting" x-point,
 - compute the maximum height for each such x-point, and
 - finally clean up the list.

This can be expressed concisely as:

    (*  skyline : (int * int * int) list -> (int * int) list  *)
    let rec skyline l = match l with
      | []        -> []
      | [(x,h,l)] -> clean_up [(x,h);(x+l,0)] 0
      | _         ->
        let first,rest = split l in
        let sk1 = skyline first in
        let sk2 = skyline rest in
        let overlay = overlay sk1 sk2 0 0 in
        let skyline = max_per_point overlay in
        clean_up skyline 0

(max_per_point could easily be inlined manually in overlay)


The implementation is purely functional and takes O(n logn) time.


To build and run:
-----------------

At one command line run:

    $ ocamlc -c skyline.ml visual.ml 
    $ ocaml skyline.cmo visual.cmo
        OCaml version 4.01.0

    # Visual.visualize (Visual.random_city());;
    - : unit = ()
    # 

These commands will generate two files (lines.dat and skyline.dat)
containing a randomly generated city and its corresponding skyline.


In another command line (in the same directory) plot the generated
data files using gnuplot (see http://www.gnuplot.info/):

    $ gnuplot plotlines.gp


(To generate more, generate a new random city from OCaml with
Visual.visualize and plot it by invoking gnuplot again)
