
(*ML Tick 5*)
(*Portable pixmap format, image of width w, height h, given by text file with h lines
which is w pixels across.*)

(* Below is an example file. there is a three line header
P3
3 3
255
   1  10 100   1  10 100   1  10 100
   1  10 100   1  10 100   1  10 100
   1  10 100   1  10 100   1  10 100
   *)

type color = int*int*int;
type xy = int*int;
datatype image = Image of xy * color array array;

(*1 Implement ML functions image, size and drawPixel*)

fun image ((w,h):xy) (c:color) = Image((w,h), Array.tabulate(h, fn(i) => Array.array(w, c)));

fun size(Image(dim,_)) = dim;

fun values(Image(_,vals)) = vals: color array array;

fun drawPixel (Image(dim, vals)) (c:color) ((x,y):xy) = Array.update(Array.sub(vals, y), x, c);

(*2 Implement a function toPPM to write an image to a file.
Base it on the following skeleton, which opens the file and
writes three lines that must appear before the lines of pixels.*)

(*automatically pads any supplied value to four characters as required for this tick.*)
fun format4 i = StringCvt.padLeft #" " 4 (Int.toString i);
(*outputs the values for one pixel*)
fun outputPixel oc ((x,y,z):color) = TextIO.output(oc, (format4 x) ^ (format4 y) ^ (format4 z));
(*call the output function for each pixel in an row, then put a newline at the end of the row*)
fun outRow oc (a:color array) = (Array.app (outputPixel oc) a; TextIO.output(oc,"\n"));

fun toPPM image filename =
  let val oc = TextIO.openOut filename
      val (w,h) = size image
  in
     TextIO.output(oc,"P3\n" ^ Int.toString w ^ " " ^ Int.toString h ^ "\n255\n");
     (* code to output image rows, one per line goes here *)
     Array.app (outRow oc) (values image);
     TextIO.closeOut oc
  end;

(*test image code*)
(*
val im=image (3,3) (1,10,100);
drawHoriz im (255,255,0) (0,0) 2;
drawVert im (0,255,255) (2,0) 2;
drawDiag im (255,0,0) (0,0) 3;
toPPM im "testimage.ppm";
*)

(*3 Implement the ML functions drawHoriz, drawVert and drawDiag.
The first draws a horizontal line to the right from the given starting
point; the second draws a vertical line downwards; the third draws a
diagonal line to the right and down. In each case, the length of the
line in pixels is given as an integer. *)

fun drawHoriz img color pos 1 = drawPixel img color pos
  | drawHoriz img color (x,y) size = (drawPixel img color (x,y); drawHoriz img color ((x+1,y):xy) (size-1));

fun drawVert img color pos 1 = drawPixel img color pos
  | drawVert img color (x,y) size = (drawPixel img color (x,y); drawVert img color ((x,y+1):xy) (size-1));
  
fun drawDiag img color pos 1 = drawPixel img color pos
  | drawDiag img color (x,y) size = (drawPixel img color (x,y); drawDiag img color ((x+1,y+1):xy) (size-1));
  
(*4 Implement a function, drawLine, to draw a line from one pixel position to another. 
Use Bresenham’s line algorithm, which can be expressed by the following pseudo-code:

 function line(x0, y0, x1, y1)
   dx := abs(x1-x0)
   dy := abs(y1-y0) 
   if x0 < x1 then sx := 1 else sx := -1
   if y0 < y1 then sy := 1 else sy := -1
   err := dx-dy
   loop
     setPixel(x0,y0)
     if x0 = x1 and y0 = y1 exit loop
     e2 := 2*err
     if e2 > -dy then 
       err := err - dy
       x0 := x0 + sx
     end if
     if e2 < dx then 
       err := err + dx
       y0 := y0 + sy 
     end if
   end loop
*)

val e2 = ref 0;
val dx = ref 0;
val dy = ref 0;
val sx = ref 0;
val sy = ref 0;
val err = ref 0;

fun loop(img: image, color:color, (x0,y0):xy, (x1,y1):xy) = 
	if x0 = x1 andalso y0 = y1 then drawPixel img color ((x0,y0):xy) else 
	((drawPixel img color ((x0,y0):xy);
	e2 := 2 *(!err));
	if (!e2) > ~(!dy) then 
		if (!e2) < (!dx) then 
			((err:= !err - (!dy); err:= !err + (!dx); loop(img, color, (x0 + !sx, y0 + !sy):xy, (x1,y1):xy)))
		else (err:= !err - (!dy); loop(img, color, (x0 + !sx,y0):xy, (x1,y1):xy))
	else if (!e2) < (!dx) then 
			(err:= !err + (!dx); loop(img, color, (x0, y0 + !sy):xy, (x1,y1):xy))
		 else ());


fun drawLine img color ((x0,y0):xy) ((x1,y1):xy) = 
	(((((dx := Int.abs(x1-x0);
	dy := Int.abs(y1-y0));
	if x0 < x1 then sx := 1 else sx := ~1);
	if y0 < y1 then sy := 1 else sy := ~1);
	err := !dx-(!dy));
	loop(img, color, (x0,y0), (x1,y1)));
	
	
val im=image (9,9) (1,10,100);
drawHoriz im (255,255,0) (0,0) 2;
drawVert im (0,255,255) (2,0) 2;
drawDiag im (255,0,0) (0,0) 3;
drawLine im (255, 255, 0) (1,1) (3,4);
toPPM im "testimage.ppm";