-- MFMCL: A PACKAGE FOR MEAN-FIELD MODELS OF DYNAMICS ON NETWORKS VIA MOMENT CLOSURE --

AUTHOR: Bert Wuyts - b3rtwu@gmail.com


DEPENDENCIES

1) (to be installed):
	-Horvat's IGraph/M package: szhorvat.net/pelican/igraphm-a-mathematica-interface-for-igraph.html
	 instal via command (in Mathematica): 
		Get["https://raw.githubusercontent.com/szhorvat/IGraphM/master/IGInstaller.m"]
2) (included in subdirectories):
	- ToMatlab: https://library.wolfram.com/infocenter/MathSource/577/
	- Bulatov's Chordal Graph package: http://mathematica-bits.blogspot.com/2011/02/chordal-graph-usage.html


INSTALLATION

Run in Mathematica: PacletInstall[dir<>"mfmcl-1.0.paclet"]
Here, 'dir' is the directory of the paclet.

Note that IGraph/M needs to be installed as well (see above).


DEMOS

BasicFunctions.nb: explains basic low-level functions
MomEqs_SIS3_unclosed.nb: general unclosed third order moment equations for SIS epidemic spreading 
MomEqs_SIS3SqL_closed.nb: closed third order moment equations for SIS epidemic spreading on a square lattice 


UNINSTALL

PacletUninstall["mfmcl"]


REFERENCE: arxiv.org/abs/2111.07643
