# MFMCL: A PACKAGE FOR MEAN-FIELD MODELS OF DYNAMICS ON NETWORKS VIA MOMENT CLOSURE 

Bert Wuyts - b3rtwu@gmail.com


## DEPENDENCIES

### External
- IGraph/M

source: szhorvat.net/pelican/igraphm-a-mathematica-interface-for-igraph.html

To install, run in Mathematica: 
```Get["https://raw.githubusercontent.com/szhorvat/IGraphM/master/IGInstaller.m"]```

### Included in subdirectories
- ToMatlab 
source: https://library.wolfram.com/infocenter/MathSource/577/
- Chordal Graph package 
source: http://mathematica-bits.blogspot.com/2011/02/chordal-graph-usage.html


## INSTALLATION

1) Download the paclet file from the release link.

2) Run in Mathematica: PacletInstall[dir<>"mfmcl-1.0.paclet"]
    (here, 'dir' is the directory of the paclet)

Note that IGraph/M needs to be installed as well (see above).


## DEMOS

- BasicFunctions.nb: explains basic low-level functions
- MomEqs_SIS3_unclosed.nb: general unclosed third order moment equations for SIS epidemic spreading 
- MomEqs_SIS3SqL_closed.nb: closed third order moment equations for SIS epidemic spreading on a square lattice 


## UNINSTALL
In Mathematica:
```PacletUninstall["mfmcl"]```


## REFERENCE 
arxiv.org/abs/2111.07643
