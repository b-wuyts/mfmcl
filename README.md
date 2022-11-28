# MFMCL: A MATHEMATICA PACKAGE FOR MEAN-FIELD MODELS OF DYNAMICS ON NETWORKS VIA MOMENT CLOSURE 

Bert Wuyts - b3rtwu@gmail.com


## DEPENDENCIES

### External
- [IGraph/M](https://github.com/szhorvat/IGraphM)

To install, run in Mathematica: 
```Mathematica
Get["https://raw.githubusercontent.com/szhorvat/IGraphM/master/IGInstaller.m"]
```

### (Included in subdirectories)
- [ToMatlab](https://library.wolfram.com/infocenter/MathSource/577/)
- [Chordal Graph package](http://mathematica-bits.blogspot.com/2011/02/chordal-graph-usage.html)


## INSTALLATION

1) Download the paclet file from the release link.

2) Run in Mathematica: 
```Mathematica
dir="/path/to/paclet/";
PacletInstall[dir<>"mfmcl-1.0.paclet"]
```


## DEMOS

- BasicFunctions.nb: explains basic low-level functions
- MomEqs_SIS3_unclosed.nb: general unclosed third order moment equations for SIS epidemic spreading 
- MomEqs_SIS3SqL_closed.nb: closed third order moment equations for SIS epidemic spreading on a square lattice 


## UNINSTALL
In Mathematica:
```Mathematica
PacletUninstall["mfmcl"]
```


## REFERENCE 
<div class="csl-entry">Wuyts, B., &#38; Sieber, J. (2022). Mean-field models of dynamics on networks via moment closure: An automated procedure. [<i>Physical Review E</i>](https://doi.org/10.1103/PhysRevE.106.054312), <i>106</i>(5), 054312. </div> [arXiv](www.arxiv.org/abs/2111.07643)
