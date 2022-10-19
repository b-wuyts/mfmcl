(* ::Package:: *)

(*
    PACKAGE FOR MEAN-FIELD EQUATIONS OF DYNAMICS ON NETWORKS VIA MOMENT CLOSURE 
	
	- B. WUYTS 12/06/2022 -
	
	CITE AS: Wuyts, B. and Sieber, J., Mean-field models of dynamics on networks via moment closure: an automated procedure, PRE - arxiv.org/abs/2111.07643
	CONTACT: b3rtwu@gmail.com

	TO DO: 
		- either merge closeJT and closedcl + add option triangulate (if beneficial, use junctionGraph instead of IGChordalQ to check for chordality)
		- or resolve disconnected motifs in closeJT if they occur
		- (or both)
	NOTES:
		- terminology and variable naming is still inconsistent: induced subgraphs = unlabelled motif = patterns, motif = labelled motif
		- closedcl uses junctionGraph to obtain closures but is still experimental
	DEPENDENCIES:
		- Bulatov's Chordal Graph package: mathematica-bits.blogspot.com/2011/02/chordal-graph-usage.html
		- Horvat's IGraphM package: szhorvat.net/pelican/igraphm-a-mathematica-interface-for-igraph.html
		- ToMatlab: library.wolfram.com/infocenter/MathSource/577/

*)


BeginPackage["mfmcl`"];


Needs["IGraphM`"];
Needs["Bulatov`chordal`"];
Needs["MatlabUtils`ToMatlab`"];


G::usage = "G returns a motif with given adjacency and labels."


enumerateGraphs::usage = "enumerateGraphs finds all graphs with k vertices."


countInducedSubgraphs::usage ="countInducedSubgraphs uses IGraph to count induced subgraphs in a given graph."


enumerateMotifs::usage = "enumerateMotifs returns all mutually non-isomorphic motifs for given graphs and labels."


replIsom::usage = "replIsom replaces isomorphic graphs with a single consistently indexed one."


prepElim::usage = "prepElim prepares the matrices required for substitution of the terms that will be eliminated."


dxadt::usage ="dxadt returns a mean-field equation for a single motif."


coeffTab::usage ="coefftab returns the matrix form of the hierarchy of mean-field equations."


consEqs::usage ="consEqs returns all conservation equations of the considered motifs."


mrfdm1::usage ="mrfdm1 creates a (d-1)-th order Markov random field representation of g."


intersectionGraph::usage ="intersectionGraph returns a intersection graph of the given subgraphs."


junctionGraph::usage ="junctionGraph returns a junction graph of the given subgraphs."


closeJT::usage ="closeJT finds a closure for graphs using the junction tree algorithm."


closedcl::usage ="closedcl returns a motif decomposition assuming it is an MRFd."


Options[getMomentEqs]=
{
SubgraphFrequencies->{},
ElimViaConsEqs->False,
posDynamicIrrelevantMotifs->{},
ConsEqs->{},
OutputFormat->"Equations"
}


Options[consEqs]=
{
SubgraphFrequencies->{},
HomogeneousDegree->False,
MotifList->{}
}


Options[closedcl]=
{
methodlc->"default"
};


Options[closeJT]=
{
methodlc->"default"
};


Options[exportMF]=
{
MatcontOrCoco->"coco"
};


closelc::usage ="closelc returns an ad-hoc motif decomposition via reduction in size."


exportMF::usage ="exportMF exports the mean-field equations to a MATLAB file."


Begin["`Private`"]


G[k_:0,p_:0,a_,x_,l_]:=AdjacencyGraph[a,VertexSize->If[Length[a]!= 1,{"Scaled",.22}],VertexLabels->Table[i->Placed[If[i==p,\!\(\*SubscriptBox[\(l\), \([\([k\[InvisibleComma]1]\)]\)]\),\!\(\*SubscriptBox[\(l\), \([\([
\*SubscriptBox[\(x\), \([\([i]\)]\)]\[InvisibleComma]1]\)]\)]\)],Center],{i,1,Length[x]}],VertexStyle->Table[i-> If[i==p,\!\(\*SubscriptBox[\(l\), \([\([k\[InvisibleComma]2]\)]\)]\),\!\(\*SubscriptBox[\(l\), \([\([
\*SubscriptBox[\(x\), \([\([i]\)]\)]\[InvisibleComma]2]\)]\)]\)],{i,1,Length[x]}],ImageSize->If[Length[a]==1,15,55],PlotRangePadding->None,GraphLayout->{"Dimension"->2}];


Gh[k_:0,p_,c_,a_,x_,l_]:=Module[{m,pairs,np,edges,edgesp,mots,nep},
(*graphs of one order higher*)

m=Length[x];
pairs=Thread[Append[{m+1},Range[m]]];np=Length[pairs];
edges=Subsets[Range[np],{1,m}];
edgesp=Select[edges,MemberQ[#,p]&];nep=Length[edgesp];
mots=Table[Graph[EdgeAdd[VertexAdd[AdjacencyGraph[a],m+1],UndirectedEdge@@@pairs[[edgesp[[j]]]]],VertexSize->{"Scaled",.22},VertexLabels->Table[i->Placed[If[(i==p)\[And](k!= 0),\!\(\*SubscriptBox[\(l\), \([\([k\[InvisibleComma]1]\)]\)]\),\!\(\*SubscriptBox[\(l\), \([\([
\*SubscriptBox[\(x\), \([\([i]\)]\)]\[InvisibleComma]1]\)]\)]\)],Center],{i,1,m}]~Join~{m+1-> Placed[\!\(\*SubscriptBox[\(l\), \([\([c\[InvisibleComma]1]\)]\)]\),Center]},VertexStyle->Table[i->If[(i==p)\[And](k!= 0),\!\(\*SubscriptBox[\(l\), \([\([k\[InvisibleComma]2]\)]\)]\),\!\(\*SubscriptBox[\(l\), \([\([
\*SubscriptBox[\(x\), \([\([i]\)]\)]\[InvisibleComma]2]\)]\)]\)],{i,1,m}]~Join~{m+1->\!\(\*SubscriptBox[\(l\), \([\([c\[InvisibleComma]2]\)]\)]\)},ImageSize->55,PlotRangePadding->None,GraphLayout->{"Dimension"->2}(*to avoid bug*)],{j,1,nep}];
Sum[\[LeftAngleBracket]mots[[j]]\[RightAngleBracket],{j,1,nep}]

];


enumerateGraphs[k_Integer]:=Module[{pairs,n,edges,motl},
(*This function enumerates all graphs with k vertices*)
	(*All possible pairs*)
pairs=Subsets[Range[k],{2}];
n=Length[pairs];
   (*All ways to make graph of k vertices [min edges:k-1,max edges:k(k-1)/2]*)
edges=Subsets[Range[n],{k-1,k (k-1)/2}];
motl=Table[Graph[Range[k],pairs[[edges[[i]]]],PlotTheme->"IndexLabeled",VertexSize->Large],{i,1,Length[edges]}];
	(*Keep only connected graphs*)
motl=Select[motl,ConnectedGraphQ];
	(*Retain subset in which none of the graphs is isomorphic with any of the others*)
DeleteDuplicates[CanonicalGraph/@motl]
];


countInducedSubgraphs[g_Graph,G_Graph]:=Module[{},
IGLADSubisomorphismCount[g,G,Induced->True]
];


enumerateMotifs[motsu_Graph,l_List]:=Module[{m,auto,tups,ltups,vclrs,ntups,gs,isomL,isL,cc},

m=VertexCount@motsu;
auto=FindGraphIsomorphism[motsu,motsu,All]//Normal;
tups=Tuples[Range[Length[l]],{m}];ntups=Length[tups];
isomL[i_,j_]:=If[Sort@Tally[tups[[i]]]!= Sort@Tally[tups[[j]]],0,Boole[Or@@(SameQ[tups[[i]],tups[[j]][[#]]]&/@auto[[;;,;;,2]])]];
isL=Table[If[j<i,isomL[i,j],0],{i,1,ntups},{j,1,ntups}];
cc=ConnectedComponents[AdjacencyGraph[isL,DirectedEdges->False]];
tups=tups[[cc[[;;,1]]//Sort]];ntups=Length[tups];
ltups=Map[\!\(\*SubscriptBox[\(l\), \([\([#, 1]\)]\)]\)&,tups,{2}];
vclrs=Map[\!\(\*SubscriptBox[\(l\), \([\([#, 2]\)]\)]\)&,tups,{2}];

gs=Table[Graph[motsu,VertexSize->If[m!= 1,{"Scaled",.22}],VertexLabels->Thread[Rule[Range[m],Placed[#,Center]&/@\!\(\*SubscriptBox[\(ltups\), \([\([j]\)]\)]\)]],VertexStyle->Thread[Rule[Range[m],\!\(\*SubscriptBox[\(vclrs\), \([\([j]\)]\)]\)]],ImageSize->If[m==1,15,55],PlotRangePadding->None,GraphLayout->{"Dimension"->2}(*to avoid bug*)],{j,1,ntups}]

];


addColors[g_Graph,l_List]:={g,"VertexColors"->getNodeStates[g,l]};


replIsom[sys_,l_List,motu_List:{},indvars_List:{}]:=Module[{gsys,gs,gsc,pats,isop,isopgrps,isL,cc,replixg,replvars},
(*This function makes all isomorphic motifs consistently indexed*)
	
	(*collect all occurring motifs and join them with list of all non-isomorphic motifs*)
gsys=Cases[sys,_Graph,\[Infinity]];
gs=DeleteDuplicates[Join[Flatten[indvars],gsys]];
	(*graphs (=motifs without labels)*)
pats=DeleteDuplicates[Join[Flatten[motu],CanonicalGraph/@gsys]];
isop=Boole[Outer[IsomorphicGraphQ,pats,gs]];
isopgrps=Map[Last,GatherBy[Position[isop,1],First],{2}];
	(*group according to graph type*)
gs=Map[gs[[#]]&,isopgrps,{2}];
	(*convert to coloured graph format compatible with Bliss or VF2 for IGraphM*)
gsc=Map[addColors[#,l]&,gs,{2}];
	(*use Bliss to test whether coloured graphs are isomorphic*)
isL=Table[If[k>j,Boole@IGBlissIsomorphicQ[gsc[[i,j]],gsc[[i,k]]],0],{i,1,Length[pats]},{j,1,Length[gs[[i]]]},{k,1,Length[gs[[i]]]}];
	(*keep only non-isomorphic ones*)
cc=(ConnectedComponents[AdjacencyGraph[#,DirectedEdges->False]]&)/@isL;
cc=(Select[#,Length[#]>1&]&)/@cc;
replixg=Flatten/@Map[Thread[#[[2;;]]->#[[1]]]&,cc,{2}];
replvars=Flatten[Table[Map[gs[[i,#]]&,replixg[[i]],{2}],{i,1,Length[gs]}]];

sys/.replvars

];


getMomentEqs[gs_,mots_List,process_,OptionsPattern[]]:=Module[{R0,R1,l,gfreqs,elim,pdynirr,nmots,ord,moth,mothrev,motsord,cons,Q,oform,k,B,c,tokeep,Qtilde1k,Qprev,Qp,momeqs,motskeep,Qp1krev,Qp1k,Qtildekp1,Qpkp1,vars,ctilderev,ctilde},
 {l,R0,R1}=process;
 gfreqs=OptionValue[SubgraphFrequencies];
 elim=OptionValue[ElimViaConsEqs];
 pdynirr=OptionValue[posDynamicIrrelevantMotifs];
 cons=OptionValue[ConsEqs];
 oform=OptionValue[OutputFormat];
 k=Max[VertexCount/@gs];
 
 If[\[Not]elim,
   momeqs=replIsom[dxadt[#,process,gfreqs]&/@mots,l,gs,mots];
   moth=DeleteDuplicates@Select[Cases[momeqs,_Graph,Infinity],VertexCount[#]==k+1&];
   If[oform=="Equations",
       {momeqs,\[LeftAngleBracket]#\[RightAngleBracket]&/@mots,\[LeftAngleBracket]#\[RightAngleBracket]&/@moth},
   (*ELSE*)
       Q=CoefficientArrays[momeqs,\[LeftAngleBracket]#\[RightAngleBracket]&/@(mots~Join~moth)][[2]]//Normal;
       {Q,\[LeftAngleBracket]#\[RightAngleBracket]&/@mots,\[LeftAngleBracket]#\[RightAngleBracket]&/@moth}
     ],
(*ELSE*)
  (*place dynamically irrelevant motifs at top of list such that they will be eliminated first*)
  (*reverse sort makes that larger motifs are eliminated first after dynamically irrelevant ones*)
   nmots=Length[mots];
   ord=pdynirr~Join~ReverseSort@Complement[Range@nmots,pdynirr];
   motsord=mots[[ord]];
   (*prepare matrices for substitution of to be eliminated terms*)
   {B,c,tokeep}=prepElim[cons,motsord,pdynirr];
   (*get normalised moment equations for the non-eliminated variables*)
   momeqs=replIsom[dxadt[#,process,gfreqs]&/@motsord[[tokeep]],l,gs,mots];
   (*eliminate to be eliminated terms*)
   Qtilde1k=CoefficientArrays[momeqs,\[LeftAngleBracket]#\[RightAngleBracket]&/@motsord][[2]]//Normal;
   Qp1krev=Qtilde1k . B//Simplify;
   ctilderev=Qtilde1k . c//Simplify;
   (*undo reverse sort*)
   motskeep=Reverse@motsord[[tokeep]];
   Qp1k=Reverse/@Reverse[Qp1krev];
   ctilde=Reverse[ctilderev];
   (*get matrix for largest (undetermined) motifs*)
   mothrev=DeleteDuplicates@Select[Cases[momeqs,_Graph,Infinity],VertexCount[#]==k+1&];
   Qtildekp1=CoefficientArrays[momeqs,\[LeftAngleBracket]#\[RightAngleBracket]&/@mothrev][[2]]//Normal;
   Qpkp1=Reverse/@Reverse[Qtildekp1];
   Qp=Join[Qp1k,Qpkp1,2];
   moth=Reverse@mothrev;
   If[oform=="Equations",
      vars=\[LeftAngleBracket]#\[RightAngleBracket]&/@(motskeep~Join~moth);
      {Qp . vars+ctilde,\[LeftAngleBracket]#\[RightAngleBracket]&/@motskeep,\[LeftAngleBracket]#\[RightAngleBracket]&/@moth}
   ,
   (*ELSE*)
   {{Qp,ctilde},\[LeftAngleBracket]#\[RightAngleBracket]&/@motskeep,\[LeftAngleBracket]#\[RightAngleBracket]&/@moth}
    ]
 ]
]


prepElim[cons_List,motl_List,dynirr_List]:=Module[{ord,nmotl,ccons,cconsm,IF,rankIF,pivcols,zerorows,freerows,freecols,F,tokeep,B,c},

nmotl=Length@motl;
ccons=CoefficientArrays[cons,\[LeftAngleBracket]#\[RightAngleBracket]&/@motl]//Normal;
cconsm=Join[ccons[[2]],-List/@ccons[[1]],2];
IF=RowReduce[cconsm];
rankIF=MatrixRank@IF;
pivcols=Flatten@(First/@DeleteCases[Position[#,1]&/@IF,{}]);
zerorows=Flatten@Position[IF,ConstantArray[0,Dimensions[IF][[2]]]];
freecols=Complement[Range@Dimensions[IF][[2]],pivcols];
freerows=Complement[Range@Dimensions[IF][[1]],zerorows];
F=IF[[freerows]][[All,freecols]];
B=ConstantArray[0,{nmotl,nmotl-rankIF}];
tokeep=Complement[freecols,{nmotl+1}];
B[[tokeep]]=IdentityMatrix[nmotl-rankIF];(*motifs to eliminate*)
B[[pivcols]]=-F[[;;,1;;-2]];(*motifs to keep*)
c=ConstantArray[0,nmotl];
c[[tokeep]]=0;c[[pivcols]]=F[[;;,-1]];

{B,c,tokeep}
]


getNodeStates[g_Graph,l_List]:=Position[l,#][[1,1]]&/@(AnnotationValue[{g,#},VertexLabels][[1]]&/@VertexList[g])


dxadt[g_,process_,gfreqs_List:{}]:=Module[{a,x,l,B,R,m,n,xkp,xadot,yb,posgalhs,posgsrhs,factors,replnormalise},
(*
Moment equation for motif with graph a and states x (B:=R0, R:=R1):
	- if gfreqs is passed as arg, output eq is normalised
NOTE: The equation is hard to read in coded form. To make it more readable, run in a notebook the command:
	HoldForm@TraditionalForm[replIsom[Sum[(Sum[Sum[a[[p\[InvisibleComma]j]]KroneckerDelta[c,xkp[k,p][[j]]],{j,1,m}]Subscript[R, [[k\[InvisibleComma]x[[p]]\[InvisibleComma]c]]],{c,1,n}]+Subscript[B, [[k\[InvisibleComma]x[[p]]]]])\[LeftAngleBracket]G[k,p,a,x,l]\[RightAngleBracket],{p,1,m},{k,1,n}],l]-Sum[(Sum[Sum[a[[p\[InvisibleComma]j]]KroneckerDelta[c,Subscript[x, [[j]]]],{j,1,m}]Subscript[R, [[x[[p]]\[InvisibleComma]k\[InvisibleComma]c]]],{c,1,n}]+Subscript[B, [[x[[p]]\[InvisibleComma]k]]])\[LeftAngleBracket]G[a,x,l]\[RightAngleBracket],{p,1,m},{k,1,n}]+replIsom[Sum[Subscript[R, [[k\[InvisibleComma]x[[p]]\[InvisibleComma]c]]]Gh[k,p,c,a,x,l]-Subscript[R, [[x[[p]]\[InvisibleComma]k\[InvisibleComma]c]]]Gh[p,c,a,x,l],{c,1,n},{p,1,m},{k,1,n}],l]]
*)
{l,B,R}=process;
{a,x}=If[GraphQ[g],
		{Normal@AdjacencyMatrix[g], getNodeStates[g,l]},
	    (*ELSE*)
			g];
	    
m=Length[x];n=Length[l];
xkp[k_,p_]:=ReplacePart[x,p->k];
xadot=replIsom[Sum[(Sum[Sum[a[[p\[InvisibleComma]j]]KroneckerDelta[c,xkp[k,p][[j]]],{j,1,m}]\!\(\*SubscriptBox[\(R\), \([\([k\[InvisibleComma]x[\([p]\)]\[InvisibleComma]c]\)]\)]\),{c,1,n}]+\!\(\*SubscriptBox[\(B\), \([\([k\[InvisibleComma]x[\([p]\)]]\)]\)]\))\[LeftAngleBracket]G[k,p,a,x,l]\[RightAngleBracket],{p,1,m},{k,1,n}],l]-Sum[(Sum[Sum[a[[p\[InvisibleComma]j]]KroneckerDelta[c,\!\(\*SubscriptBox[\(x\), \([\([j]\)]\)]\)],{j,1,m}]\!\(\*SubscriptBox[\(R\), \([\([x[\([p]\)]\[InvisibleComma]k\[InvisibleComma]c]\)]\)]\),{c,1,n}]+\!\(\*SubscriptBox[\(B\), \([\([x[\([p]\)]\[InvisibleComma]k]\)]\)]\))\[LeftAngleBracket]G[a,x,l]\[RightAngleBracket],{p,1,m},{k,1,n}]+replIsom[Sum[\!\(\*SubscriptBox[\(R\), \([\([k\[InvisibleComma]x[\([p]\)]\[InvisibleComma]c]\)]\)]\)Gh[k,p,c,a,x,l]-\!\(\*SubscriptBox[\(R\), \([\([x[\([p]\)]\[InvisibleComma]k\[InvisibleComma]c]\)]\)]\)Gh[p,c,a,x,l],{c,1,n},{p,1,m},{k,1,n}],l];
yb=DeleteDuplicates@Cases[xadot,\[LeftAngleBracket]_Graph\[RightAngleBracket],\[Infinity]];

(*Normalisation*)
If[gfreqs=={},
Collect[xadot,yb],
(*ELSE*)
(*first remove non-occuring motifs*)
xadot=xadot/.\[LeftAngleBracket]x_Graph\[RightAngleBracket]/;NoneTrue[gfreqs[[;;,1]],IsomorphicGraphQ[x,#]&]:>0;
yb=Flatten[yb/.\[LeftAngleBracket]x_Graph\[RightAngleBracket]/;NoneTrue[gfreqs[[;;,1]],IsomorphicGraphQ[x,#]&]:>{}];
(*obtain factors*)
posgalhs=Position[IsomorphicGraphQ[AdjacencyGraph[a],#]&/@gfreqs[[;;,1]],True][[1]];
posgsrhs=Last/@Position[Outer[IsomorphicGraphQ,yb/.\[LeftAngleBracket]x_Graph\[RightAngleBracket]:>x,gfreqs[[;;,1]]],True];
factors=(gfreqs[[#,2]]&/@posgsrhs)/gfreqs[[posgalhs,2]][[1]];
(*normalise*)
replnormalise=MapThread[Rule,{yb,factors*yb}];
Collect[xadot,yb]/.replnormalise
]

];


coeffTab[sys_List,indvars_List,show_:False]:=Module[{gs,dvcnt,vars,divs,coeffs},
(*either pass Qp or moment eqs as arugment sys*)
If[Head[sys[[1]]]!=List,
gs=DeleteDuplicates[Cases[indvars,_Graph,Infinity]~Join~Cases[sys,_Graph,Infinity]];
vars=\[LeftAngleBracket]#\[RightAngleBracket]&/@gs;
coeffs=Normal[CoefficientArrays[sys,vars][[2]]],
gs=indvars/.\[LeftAngleBracket]x_Graph\[RightAngleBracket]:>x;vars=indvars;coeffs=sys;
];
If[show,
	dvcnt=Differences[VertexCount/@gs];
	divs={True}~Join~(#==1&/@dvcnt)~Join~{True};
	Grid[{vars}~Join~coeffs,Dividers->{{divs},{{True}~Join~divs}}]/.{0->"."},
(*ELSE*)
	coeffs
]

];


consEqs[motu_List,l_List,OptionsPattern[]]:=Module[{homdeg,motl,gfreqs,gstub,istubs,isostubs,nisostubs,firstisost,stubs,vdel,tups,stubidxs,vc,laborder,constub,gnstub,connstub,c,posmotuf,cons,norm2nonnorm},
(*If motl is supplied, it is passed to replIsom to ensure that the indexing of motl is used*)

homdeg=OptionValue[HomogeneousDegree];
gfreqs=OptionValue[SubgraphFrequencies];
motl=OptionValue[MotifList];
n=Length[l];

If[homdeg,
(*homogeneous degree*)
	gstub=Graph/@Select[motu,Min[DegreeCentrality[#]]==1&];
	istubs=Flatten@Position[DegreeCentrality[#],1]&/@gstub;
	isostubs=Intersection[DeleteDuplicates@Cases[GraphAutomorphismGroup[#],_Integer,Infinity],Flatten@Position[DegreeCentrality[#],1]]&/@gstub;
	nisostubs=MapThread[Complement,{istubs,isostubs}];
	firstisost=If[#!={},{First[#]},{}]&/@isostubs;
	stubs=MapThread[Join,{nisostubs,firstisost}];
	vdel=Table[VertexDelete[gstub[[i]],#]&/@stubs[[i]],{i,1,Length[gstub]}];
	tups=Tuples[Range[n],{#}]&/@(VertexCount/@gstub-1);
	stubidxs=MapThread[VertexIndex,{gstub,stubs}];
	vc=VertexCount/@gstub;
	laborder=Table[{stubidxs[[i,j]]}~Join~Complement[Range[vc[[i]]],{stubidxs[[i,j]]}],{i,1,Length[stubidxs]},{j,1,Length[stubidxs[[i]]]}];
	(*the following assumes that #1 is ordered appropriately (to check)*)
	constub=Table[(Sum[\[LeftAngleBracket]G[AdjacencyMatrix[gstub[[i]]],({c}~Join~#1)[[Ordering@#3]],l]\[RightAngleBracket],{c,1,n}]==\[LeftAngleBracket]G[AdjacencyMatrix[#2],#1,l]\[RightAngleBracket])&[tups[[i,j]],vdel[[i,k]],laborder[[i,k]]],{i,1,Length[gstub]},{j,1,Length[tups[[i]]]},{k,1,Length[vdel[[i]]]}];
	gnstub=Graph/@Select[motu,Min[DegreeCentrality[#]]!=1&];
	connstub=Table[Total[(\[LeftAngleBracket]G[AdjacencyMatrix[gnstub[[i]]],#,l]\[RightAngleBracket])&/@Tuples[Range[n],{VertexCount[gnstub[[i]]]}]]==1,{i,1,Length[gnstub]}];
	cons=Flatten[constub]~Join~connstub;,
(*heterogeneous degree*)
	cons=Table[Total[(\[LeftAngleBracket]G[AdjacencyMatrix[motu[[i]]],#,l]\[RightAngleBracket])&/@Tuples[Range[n],{VertexCount[motu[[i]]]}]]==1,{i,1,Length[motu]}];
];

cons=replIsom[cons,l,motu,Flatten[motl]];

If[gfreqs=={},
(*Normalised*)
cons,
(*Non-normalised*)
norm2nonnorm[vars_,i_]:=vars/.\[LeftAngleBracket]x_Graph\[RightAngleBracket]/;IsomorphicGraphQ[gfreqs[[i,1]],x]:>\[LeftAngleBracket]x\[RightAngleBracket]/gfreqs[[i,2]];
cons=Fold[norm2nonnorm,cons,Range@Length[gfreqs]]//Simplify
]

];


mrfdm1[g_Graph]:=Module[{d,vl,al,aldm1,al2dm1,e2add,gdm1,bags,pairs,num,den},
(*  creates a (d-1)-th order Markov random field representation of g
     by connecting every node to all other nodes in its (d-1) nbhood.*)
	
d=GraphDiameter[g];
vl=VertexList[g];
al=AdjacencyList[g,#]&/@vl;
aldm1=AdjacencyList[g,#,d-1]&/@vl;
al2dm1=MapThread[Complement,{aldm1,al}];
e2add=Union[Sort/@Flatten[Table[(vl[[i]]\[UndirectedEdge]#)&/@al2dm1[[i]],{i,1,Length[vl]}]]];
gdm1=EdgeAdd[g,e2add]

];


plotiG[subgs_,is_,edges_]:=Module[{vlabs,elabs,nsubgs,vf2},
vf2[{xc_,yc_},name_,{w_,h_}]:={White,Rectangle[{xc-w,yc-h},{xc+w,yc+h}]};
nsubgs=Length@subgs;
vlabs=Table[i->Placed[Graph[subgs[[i]],ImageSize->60],Center],{i,1,nsubgs}];
elabs=Table[edges[[i]]->is[[i]],{i,1,Length[edges]}];
Graph[Range[nsubgs],edges,VertexLabels->vlabs,EdgeLabels->elabs,VertexStyle->White,VertexSize->{"Scaled",.26},ImageSize->320,VertexShapeFunction->vf2,EdgeStyle->Gray]
]


intersectionGraph[subgs_List,plot_:False]:=Module[{nsubgs,vlsgs,pairs,is,nonempty,edges,vlabs,elabs,iG},
(*
INPUT:  subgraphs
OUTPUT: intersection graph of subgraphs (= clique graph when the subgraphs are cliques)
*)
nsubgs=Length@subgs;
vlsgs=Sort/@(VertexList/@subgs);
pairs=Subsets[Range[nsubgs],{2}];
is=Intersection[vlsgs[[#[[1]]]],vlsgs[[#[[2]]]]]&/@pairs;
edges=Apply[UndirectedEdge]/@pairs;
(*only keep intersecting pairs*)
nonempty=Complement[{#}&/@Range[Length@is],Position[is,{}]];
pairs=Extract[pairs,nonempty];
edges=Extract[edges,nonempty];
is=Extract[is,nonempty];

If[plot,
	plotiG[subgs,is,edges],
(*ELSE*)
	{Graph[Range[nsubgs],edges],pairs,is}
]

];


junctionGraph[subgs_,plot_:False]:=Module[{JG,pairs,is,edges,vcis,ord,i,n,edgei,sse,possse,elsse,deledgei,nonempty},
(*
INPUT:  subgraphs
OUTPUT: junction graph of subgraphs (and plot if plot==True)

PROCEDURE:
-iteratively eliminate redundant edges from intersection graph, i.e. remove those edges of which the corresponing intersection is
	contained in each edge of an alternative path between its connecting nodes.
-stop elimintation when either the resulting junction graph is a tree or all edges have been checked
*)

{JG,pairs,is}=intersectionGraph[subgs];
edges=Apply[UndirectedEdge]/@pairs;
vcis=Length/@is;
ord=Ordering@vcis;
i=0;
n=Length[is];

While[\[Not]TreeGraphQ[JG]\[And]i<n,
	i=i+1;
	edgei=ord[[i]];
	sse=SubsetQ[#,is[[edgei]]]&/@is;
	sse[[edgei]]=False;
	possse=Position[sse,True];
	If[possse=={},Continue[],elsse=Extract[edges,possse]];
	deledgei=If[And@@(MemberQ[Flatten@(elsse/.UndirectedEdge->List),#]&/@pairs[[edgei]]),FindPath[Graph[Extract[edges,possse]],pairs[[edgei]][[1]],pairs[[edgei]][[2]]]!={},False];
	If[deledgei,
		JG=EdgeDelete[JG,{edges[[edgei]]}];
		is[[edgei]]={},
	(*ELSE*)
		Continue[]
	];
];

nonempty=Complement[{#}&/@Range[Length@is],Position[is,{}]];
pairs=Extract[pairs,nonempty];
edges=Extract[edges,nonempty];
is=Extract[is,nonempty];

If[plot,
	plotiG[subgs,is,edges],
(*ELSE*)
	{JG,pairs,is}
]

]


closeJT[g_Graph,l_List,OptionsPattern[]]:=Module[{methlc,isoneloop,mc,num,gmrf,clqs,iG,edgeweight,maxspanT,pairsmT,ix,isos,clqsisos,iG3isos,den},
(*TODO: DEAL WITH SINGLE LOOPS IN IG*)
methlc=OptionValue[methodlc];

closeJT::disconn="WARNING: closeJT did not find a decomposition of `1` with connected separators. These can be eliminated via conservation equations; or prevented via: non-minimal triagulation or grouping of subgraphs along loops in the intersection graph.";

isoneloop=And@@((#==2)&/@VertexDegree[g])\[And]\[Not]TreeGraphQ[g]; 
If[CompleteGraphQ[g]\[Or](isoneloop\[And]methlc=="kirkwood"),
	mc=closelc[g,l],
(*ELSE*)
	gmrf=mrfdm1[g];
	If[\[Not]IGChordalQ[gmrf],(*IGChordalQ of IGraph is faster than Bulatov's isChordal*)
		gmrf=triangulateGraph[gmrf](*triangulateGraph is minimal (?) and IGChordalCompletion of IGraph not*)
	];
	clqs=FindClique[gmrf,\[Infinity],All];
	iG=intersectionGraph[Subgraph[gmrf,#]&/@clqs];
	edgeweight=-1*Length/@iG[[3]];
	maxspanT=FindSpanningTree[iG[[1]],EdgeWeight->edgeweight];
	pairsmT=EdgeList[maxspanT]/.UndirectedEdge->List;
	ix=Flatten[#,1]&@(Position[iG[[2]],#]&/@pairsmT);
	(*find isomorphisms of g and take mean over them*)
	isos=Normal@FindGraphIsomorphism[g,g,All];
	clqsisos={clqs}~Join~((clqs/.#)&/@isos[[2;;]]);
	iG3isos={iG[[3]]}~Join~((iG[[3]]/.#)&/@isos[[2;;]]);
	num=Table[Subgraph[g,#]&/@clqsisos[[i]],{i,1,Length@isos}];
	den=Table[Subgraph[g,#]&/@Extract[iG3isos[[i]],ix],{i,1,Length@isos}];
	If[\[Not]Or@@(ConnectedGraphQ/@den[[1]]),Message[closeJT::disconn,g]];
	mc=(Mean@Table[Times@@num[[i]]/Times@@den[[i]],{i,1,Length@num}])/.x_Graph:>\[LeftAngleBracket]x\[RightAngleBracket]
];
\[LeftAngleBracket]g\[RightAngleBracket]->mc
];


gVE[g_]:={Sort@VertexList[g],Sort@(Sort/@EdgeList[g])};(*List with vertex and edge list of graph to compare graphs (Graph objects are not always the same if e and v list are the same).*)


closedcl[g_Graph,l_List,OptionsPattern[]]:=Module[{d,isoneloop,vc,dcl,g0VE,k,ssixs,numk,undm1clk,isEQg,posEQg,jg,ixtree,istree,undclk,num,den,mc},
(* Closure replacement rules via dth order Markov random field assumption VIA JUNCTION GRAPH FUNCTION (in GenMC`), resulting in factorisation into d-cliques.*)
   (*TODO: try to make more tidy*)
   
d=GraphDiameter[g]-1;
vc=VertexCount[g];
isoneloop=And@@((#==2)&/@VertexDegree[g])\[And]\[Not]TreeGraphQ[g];     

If[CompleteGraphQ[g]\[And]OptionValue[methodlc]=="default",
mc=Times@@(ConnectedGraphComponents@EdgeDelete[g,EdgeList[g]])/.x_Graph:>\[LeftAngleBracket]x\[RightAngleBracket],
(*ELSE*)
	Which[
		(isoneloop\[Or]CompleteGraphQ[g])\[And]OptionValue[methodlc]=="kirkwood",
			num={VertexDelete[g,#]&/@Range[vc]};
			jg=junctionGraph/@num;
			istree=False,
		isoneloop\[And]OptionValue[methodlc]=="default",
			dcl=FindKClique[g,d,vc,All];
			num={Subgraph[g,#]&/@dcl};
			jg=junctionGraph/@num;
			istree=False,
		True,
	(*ELSE*)
		dcl=FindKClique[g,d,vc,All];
		num={Subgraph[g,#]&/@dcl};
		jg=junctionGraph/@num;
		g0VE=gVE[g];
		k=Length[dcl]-1;
		If[k==1,istree=True];
		While[k>=2,
			If[Or@@(TreeGraphQ/@jg[[;;,1]]),
				istree=True;
				If[k==Length[dcl]-1,Break[]];
				ixtree=Position[TreeGraphQ/@jg[[;;,1]],True];
				jg=Extract[jg,#]&/@ixtree;
				num=Extract[numk[[posEQg]],#]&/@ixtree;
				Break[],
			(*ELSE*)
				istree=False;
				ssixs=Subsets[Range[Length[dcl]],{k}];
				numk=num[[1,#]]&/@ssixs;
				undclk=Apply[GraphUnion]/@numk;
				isEQg=SameQ[g0VE,#]&/@(gVE/@undclk);
				posEQg=Flatten@Position[isEQg,True];
				If[posEQg=={},
					jg=junctionGraph/@num;
					Break[],
				(*ELSE*)
					jg=junctionGraph/@numk[[posEQg]];
				];
			];
			k=k-1;
		];	
	];
	
	If[istree,
		den=Map[Subgraph[g,#]&,jg[[;;,3]],{2}];
		mc=Mean@Table[Times@@num[[i]]/Times@@den[[i]],{i,1,Length[num]}]/.x_Graph:>\[LeftAngleBracket]x\[RightAngleBracket];,
	(*ELSE*)
		mc=Mean@MapThread[closelcJG[g,#1,#2,l]&,{num,jg}]
	];
];
Rule[\[LeftAngleBracket]g\[RightAngleBracket],mc]

];


closelc[g_Graph,l_List]:=Module[{num,iG,den,mc,nlmot,nlnum,nlden,nm1,m1s},
(*closure for single loops or complete graphs*)
num=Select[VertexDelete[g,#]&/@VertexList[g],ConnectedGraphQ];
iG=intersectionGraph[num];
den=Select[Subgraph[g,#]&/@iG[[3]],ConnectedGraphQ];
(*consistency correction*)
nlmot=Count[AnnotationValue[g,VertexLabels][[;;,2,1]],#]&/@l[[;;,1]];
nlnum=Total@Table[Count[AnnotationValue[num[[i]],VertexLabels][[;;,2,1]],#]&/@l[[;;,1]],{i,1,Length[num]}];
nlden=Total@Table[Count[AnnotationValue[den[[i]],VertexLabels][[;;,2,1]],#]&/@l[[;;,1]],{i,1,Length[den]}];
nm1=nlmot-(nlnum-nlden);
m1s=If[AnyTrue[nm1,#!= 0&],\!\(
\*UnderoverscriptBox[\(\[Product]\), \(j = 1\), \(Length[l]\)]
\*SuperscriptBox[\(G[{{0}}, {j}, l]\), \(nm1[\([j]\)]\)]\),1];
(m1s(Times@@num/Times@@den))/.x_Graph:>\[LeftAngleBracket]x\[RightAngleBracket]
];


closelcJG[g_,num_,jg_,l_]:=Module[{den,mc,nlmot,nlnum,nlden,nm1,m1s},
(*closure for single loops or complete graphs generalised to a junction graph (= intersection graph with redundant edges removed)*)
(*TODO?: cons corr in junction graph: cc = \!\(
\*SubscriptBox[\(\[Product]\), \(\(i\)\(\ \)\)]\(
\*SubscriptBox[\(\[Intersection]\), \(j\)]\ 
\*SubscriptBox[\(M\), \(
\*SubscriptBox[\(L\), \(i\)]j\)]\)\), where L is the set of chordless cycles and Subscript[M, Subscript[L, i]j] the vertex set of motif j of cycle Subscript[L, i]?
						<- does not seem to work for ComleteGraph[>5]*)
den=Subgraph[g,#]&/@jg[[3]];
den=Select[den,ConnectedGraphQ[#]&];
(*consistency correction*)
nlmot=Count[AnnotationValue[g,VertexLabels][[;;,2,1]],#]&/@l[[;;,1]];
nlnum=Total@Table[Count[AnnotationValue[num[[i]],VertexLabels][[;;,2,1]],#]&/@l[[;;,1]],{i,1,Length[num]}];
nlden=Total@Table[Count[AnnotationValue[den[[i]],VertexLabels][[;;,2,1]],#]&/@l[[;;,1]],{i,1,Length[den]}];
nm1=nlmot-(nlnum-nlden);
m1s=If[AnyTrue[nm1,#!= 0&],\!\(
\*UnderoverscriptBox[\(\[Product]\), \(j = 1\), \(Length[l]\)]
\*SuperscriptBox[\(G[{{0}}, {j}, l]\), \(nm1[\([j]\)]\)]\),1];
(m1s(Times@@num/Times@@den))/.x_Graph:>\[LeftAngleBracket]x\[RightAngleBracket]
];


exportMF[dir_,funcn_,sysrf_,varskeep_,p_,OptionsPattern[]]:=Module[{fname,exprToFunction,selPars,MorC,varxs,replmotvars,sys,Fxp,Jdx,Jdp,replGr,xX,replx,strFxp,strJdx,strJdp,strt,strp,strps,stream,strNx,strNp},
exprToFunction[expr_,vars_]:=ToExpression[ToString[FullForm[expr]/.MapIndexed[#1->Slot@@#2&,vars]]<>"&"];
selPars[M_]:=Select[SparseArray[M]["NonzeroValues"],NumericQ[#]==False&];

MorC=OptionValue[MatcontOrCoco];
(*vars and pars list*)
varxs=ToExpression[Table["X"<>ToString[i],{i,1,Length[varskeep]}]];
replmotvars=MapThread[Rule,{varskeep,varxs}];
sys=sysrf//.replmotvars;

(*convert system to pure function*)
Fxp=exprToFunction[sys,varxs~Join~p];
(*compute Jacobians*)
Fxp=Fxp@@(varxs~Join~p);
Jdx=D[Fxp,{varxs}];
Jdp=D[Fxp,{p}];

(*prepare strings for output*)
xX=Table["X("<>ToString[i]<>")",{i,1,Length[varxs]}];
replx=MapThread[Rule,{varxs,xX}];
strFxp=ToMatlab[Fxp/.replx,5000];
strJdx=ToMatlab[Jdx/.replx,5000];
strJdp=ToMatlab[Jdp/.replx,5000];

fname=dir<>funcn<>".m";
If[MorC=="matcont",
(*Matcont*)
strt="t,";strp=StringTake[ToString[p],{2,-2}];strps="";,
(*Coco*)
strt="";strp="p";strps=StringJoin[Table[ToString[p[[i]]]<>"=p("<>ToString[i]<>");\n",{i,1,Length[p]}]]];
stream=OpenWrite[fname,PageWidth->Infinity];
strNx=ToString[Length[varxs]];strNp=ToString[Length[p]];
WriteString[stream,"function out="<>funcn<>"
out{1}= @init;
out{2}= @fun_eval;
out{3}= @jacobian;
out{4}= @jacobianp;

%--------------------------------------------------------------------------
function dydt = fun_eval("<>strt<>"X,"<>strp<>")\[IndentingNewLine]"<>strps<>"
dydt="<>StringTake[strFxp,{1,-3}]<>"'; %note the transpose

% --------------------------------------------------------------------------
function [tspan,y0,options] = init
y0=zeros(1,"<>strNx<>");
handles = feval("<>funcn<>");
options = odeset('Jacobian',handles(3),'JacobianP',handles(4),'Hessians',handles(5),'HessiansP',handles(6));
tspan = [0 10];

% --------------------------------------------------------------------------
function jac = jacobian("<>strt<>"X,"<>strp<>")
"<>strps<>"
jac="<>strJdx<>"
% --------------------------------------------------------------------------
function jacp = jacobianp("<>strt<>"X,"<>strp<>")
"<>strps<>"
jacp="<>strJdp
];
Close[stream];

]


End[];


EndPackage[];
