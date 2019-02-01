
(*parts from tick 5*)
type color = int*int*int;
type xy = int*int;
datatype image = Image of xy * color array array;

fun image ((w,h):xy) (c:color) = Image((w,h), Array.tabulate(h, fn(i) => Array.array(w, c)));

fun size(Image(dim,_)) = dim;

fun values(Image(_,vals)) = vals: color array array;

fun drawPixel (Image(dim, vals)) (c:color) ((x,y):xy) = Array.update(Array.sub(vals, y), x, c);

fun format4 i = StringCvt.padLeft #" " 4 (Int.toString i);

fun outputPixel oc ((x,y,z):color) = TextIO.output(oc, (format4 x) ^ (format4 y) ^ (format4 z));

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

fun drawHoriz img color pos 1 = drawPixel img color pos
  | drawHoriz img color (x,y) size = (drawPixel img color (x,y); drawHoriz img color ((x+1,y):xy) (size-1));

fun drawVert img color pos 1 = drawPixel img color pos
  | drawVert img color (x,y) size = (drawPixel img color (x,y); drawVert img color ((x,y+1):xy) (size-1));
  
fun drawDiag img color pos 1 = drawPixel img color pos
  | drawDiag img color (x,y) size = (drawPixel img color (x,y); drawDiag img color ((x+1,y+1):xy) (size-1));

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



(*all code above from tick 5 don't forget to load it to the compiler, starred tick below*)

(*1 Write a function drawAll that applies a function to every pixel in an image.
The function passed to drawAll will be referred to as the colouring function
and should have the type xy -> color. This function should take a pair of
integers (the coordinates of the current pixel) and return a triple of integers
(the RGB value to colour this pixel). Your function drawAll should thus take
a colouring function and colour each pixel of the image according to the output
of this colouring function.*)

fun applyToRow((f:xy -> color),img,x,y,w) = 
	if x < w-1 then (drawPixel img (f ((x,y):xy)) ((x,y):xy); applyToRow(f,img,x+1,y,w))
	else drawPixel img (f (x,y)) ((x,y):xy);
			
fun applyToCols(f,img,y,w,h) = 
	if y < h-1 then (applyToRow(f,img,0,y,w); applyToCols(f,img,y+1,w,h))
	else applyToRow(f,img,0,y,w);
	
fun applyToArrays(f,img,(w,h):xy) = 
	applyToCols(f,img,0,w,h);

fun drawAll (f:(xy -> color)) img = 
	applyToArrays(f,img,(size img));
		

(*2 Write a function gradImage: unit->unit that uses drawAll to create
an image on disk (gradient.ppm) of dimensions 640x480 pixels with each
pixel value set according to the gradient function.*)

fun gradient ((x,y):xy) : color =
    (((x div 30) * 30) mod 256, 0, ((y div 30) * 30) mod 256);

fun gradImage () = 
	let val img = image ((640,480):xy) ((100,100,100):color);
	in
		(drawAll gradient img;
		toPPM img "gradient.ppm");
		()
	end;


(*Functions provided by the tick info *)
(*This function checks to see if the point (x,y) lies within the Mandelbrot set.
The argument maxIter indicates how many attempts should be made before assuming that
the point is in the set. It returns the amount of work done as the fraction c/maxIter
so 0.0 represents zero iterations and 1.0 represents the maximum number of iterations.*)
fun mandelbrot maxIter (x,y) =
  let fun solve (a,b) c = 
      if c = maxIter then 1.0
      else
        if (a*a + b*b <= 4.0) then 
          solve (a*a - b*b + x,2.0*a*b + y) (c+1)
        else (real c)/(real maxIter)
  in
    solve (x,y) 0
  end;

(*Copy the following function to your source code. This function selects an RGB colour
based on the number of iterations returned by the mandelbrot function. The implementations
of sin and cos are provided by the Math library.*)

fun chooseColour (n:real) : color =
  let
    val r = round ((Math.cos n) * 255.0)
    val g = round ((Math.cos n) * 255.0)
    val b = round ((Math.sin n) * 255.0)
  in
    (r,g,b)
  end;

(*3 The mandelbrot function does not operate on pixel values. Instead it operates on
the (real) number plane. Our image will cover only a portion of the mandelbrot
set and so we will need to convert between pixel values and real numbers.*)

fun rescale ((w,h):xy) (cx,cy,s) ((x,y):xy) = 
	(s*(real(x)/real(w) - 0.5) + cx, s*(real(y)/real(h) - 0.5) + cy);

(*4 Write a function compute that combines your rescale function with mandelbrot
and chooseColour in order to compute the Mandelbrot set of a particular region
and save the result to a file on disk.*)


fun compute ((w,h):xy) (cx,cy,s) maxiter = 
	let val img = image ((w,h):xy) ((200,200,200):color)
		val f = fn((x,y):xy) => chooseColour(mandelbrot maxiter (rescale (w,h) (cx,cy,s) (x,y)));
	in
		(drawAll f img; toPPM img "mandelbrot.ppm");
		()
	end;