cl-geom
=======

## What is it?

cl-geom is a 2D and 3D geometry library in Common Lisp. For some reason, Lisp seems to lack a flexible geometry package; the goal of this project is to implement a full-blown geometry package which makes geometrical operations intuitive and straightforward. In particular, the implementation should be as Lisp-y as possible, utilizing all the standard Lisp idioms. The one exception is the interface to the matrix math.

## Dependencies 

cl-geom depends on gsll, the Lisp bindings to the GNU Scientific Library. It also depends on [cl-graph](https://github.com/gwkkwg/cl-graph), the graph algorithm library.

## Current features

cl-geom currently supports a number of common 2D and 3D geometric objects, including:
- points
- vectors
- line segments
- polygons (2D)
- quaternions
- matrices

It also implements a number of common geometric algorithms, including:
- line intersection
- iterative closest point registration
- polygon clipping (2D)
- polyline matching (2D)
- convex hull
- Nelder-Mead simplex optimization (2D)

In addition to implementing algorithms and objects, the goal is to make using the library easy. This means extensive use of the condition system to throw errors when objects are geometrically invalid or vectors don't have the same length or whatever. The philosophy is that the system should warn you when you make a mistake.

## Installation

Eventually this project will be in [Quicklisp](http://www.quicklisp.org), but for now, you can get it running by doing:

	git clone http://github.com/grapesmoker/cl-geom.git
	ln -s /path/to/cl-geom ~/quicklisp/local-projects/cl-geom
	
and then load up your favorite Lisp and do:

	(ql:quickload :cl-geom)

Although in principle this should work on any Common Lisp, it has only been tested on SBCL.

## Usage

You can instantiate objects directly either via `make-instance` or the helper functions provided for that purpose, e.g. `make-point`, `make-polygon`, etc. cl-geom currently supports computation with vectors, quaternions, and matrices. Algorithm functions can also be called with the appropriate arguments.

## Future work

Lots of work remains to be done. Most of the current algorithms are 2D only, which is a limitation. A nonexhaustive list of things that are on the docket to add to this library:
- parametric curves
- planes
- infinite lines
- polyhedra
- kd-trees
- binary space partitions
- polygon triangulation
- Voronoi diagrams
- visualization (either using OpenGl or Cairo)

cl-geom is a work in progress, with all the warts that entails.
