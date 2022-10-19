(* ::Package:: *)

BeginPackage["Bulatov`independencePoly`",{"Bulatov`common`","Bulatov`dictionary`","Bulatov`treeDecomposition`"}];

getIndPoly::usage="getIndPoly[graph] finds independence polynomial of graph";


Options[getIndPoly]=
{
Method->"TreeDecomposition"
};


Begin["`Private`"]

(* factor and clique children *)
ch[{i_List}]:=Cases[messages,{i,_}];
ch[{i_List,j_List}]:=Cases[messages,{j,Except[i]}];

(* converts clause representation to full form with explicit negations *)
compact2full[trueVars_List,allVars_List]:=Module[{negate},
	negate[lst_]:=Not/@lst;
	And@@(trueVars\[Tilde]negate[allVars\[Backslash]trueVars])
];

simplifyPoly[poly_,var_]:=Module[{x},
	coefs=CoefficientList[poly,var];
	poly2=MapIndexed[#1 x^(#2[[1]]-1)&,CoefficientList[poly,var]];
	Total@poly2/.x->var
];

setupVertexFactor[v_Integer]:=(
	AppendTo[nativeFactors,{v}];
	cX[{v}]={{},{v}};
	cY[{v}]={1,#};
	cD[{v}]={v};
);

setupEdgeFactor[ef_List]:=(
	AppendTo[nativeFactors,ef];
	cX[ef]={{},{First[ef]},{Last[ef]}};
	cY[ef]={1,1,1};
	cD[ef]=ef;
);

setupMessageFactor[{i_,j_}]:=(
	AppendTo[messages,{i,j}];
	cD[{i,j}]=i\[Intersection]j;
);

(* greedily assign factors to bags *)
assignFactors[factors_]:=(
	unassigned=factors;
	assignToBag[jnode_]:=(
		coveredFactors=Select[unassigned,#\[Subset]jnode&];
		cl[jnode]=coveredFactors;
		unassigned=unassigned\[Backslash]coveredFactors;
	);
	Clear[cl];
	assignToBag/@jnodes;
	Assert[unassigned=={}];
);

setupHardcore[gr_Graph]:=(
	g=gr;

	(* cX stores possible vertex subsets of X, cY is their counts, *)
	Clear[cX,cY,cD];

	(* setup native factors *)
	nativeFactors={};
	verts=VertexList[g];
	setupVertexFactor/@verts;
	edges=List@@@EdgeList[g];
	setupEdgeFactor/@edges;

	(* setup factors from tree factorization *)
	messages={};
	{jnodes,jedges}=findTreeDecomposition[g];
	assignFactors[nativeFactors];
	(* TODO: replace with messages *)
	setupMessageFactor/@(jedges\[Tilde](Reverse/@jedges));
);

(* updates current message *)
updateMsg[{i_,j_}]:=(
	{xs,ys}=evalMsg[{i,j}];
	ys2=Table[simplifyPoly[poly,#],{poly,ys}];
	setMsg[{i,j},{xs,ys2}];
);

evalMsg[{i_,j_}]:=(
	factors=cl[j]\[Tilde]ch[{i,j}];
	{xsT,ysT}=evalProduct[factors,j];
	evalSum[{xsT,ysT},i\[Intersection]j]
);

evalClique[j_]:=(
	factors=cl[j]\[Tilde]ch[{j}];
	{xs,ys}=evalProduct[factors,j]
);

setMsg[msg_,{xs_,ys_}]:=(
	cX[msg]=xs;
	cY[msg]=ys;
);

(* evaluates product of factors on given domain *)
evalProduct[factors_,domain_]:=(
	formulas=getFormula/@factors;
	instances=SatisfiabilityInstances[And@@formulas,domain,All];
	xs=Extract[domain,Position[#,True]]&/@instances;
	oneVal[x_]:=prod[f\[Element]factors,evalF[f,x]];
	ys=oneVal/@xs;
	{xs,ys}
);

(* sums over atoms orthogonal to domain *)
evalSum[{{},{}},_]:={{},{}};
evalSum[{xs_,ys_},dom_]:=(
	decorated=Transpose[{xs,ys}];
	grouped=GatherBy[decorated,First[#]\[Intersection]dom&];
	getXY[group_]:=(x=group[[1,1]]\[Intersection]dom;y=Total@group[[All,2]];{x,y});
	Transpose[getXY/@grouped]
);

(* looks up a single value of a factor *)
evalF[factor_,rawX_]:=(
	Assert[indexQ[cX,factor]];
	x=rawX\[Intersection]cD[factor];
	Assert[MemberQ[cX[factor],x]];
	vals=Extract[cY[factor],Position[cX[factor],x]];
	Assert[Length[vals]==1];
	First@vals
);

getFormula[factor_]:=(
	Or@@(compact2full[#,cD[factor]]&/@cX[factor])
);

rootUpdateSchedule[j_List]:=(
	doUpdate[node_]:=(
		children=ch[node];
		If[Head[children]==List,doUpdate/@ch[node]];
		Sow[node];
	);

	Most[Reap[doUpdate[{j}]][[2,1]]]
);

getIndPolyConnectedTree[gr_Graph]:=(
	setupHardcore[gr];
	root=First@jnodes;
	updateMsg/@rootUpdateSchedule@root;
	Total[evalClique[root]//Last]
)

getIndPolyConnectedEnum[gr_Graph]:=Module[{x},
	edge2clause[e_]:=!(e[[1]]&&e[[2]]);
	clauses=edge2clause/@EdgeList[gr];
	formula=And@@clauses;
	vars=VertexList[gr];
	instances=SatisfiabilityInstances[formula,vars,All];
	counts=Count[#,True]&/@instances;
	Total@MapThread[#2 x^#1&,Thread@Tally@counts]/.x->#
];

getIndPolyConnected[gr_Graph]:=(
	Which[
		method=="TreeDecomposition", getIndPolyConnectedTree[gr],
		method=="SAT", getIndPolyConnectedEnum[gr],
		True,Assert[False,"Unknown method"]
	]
);


(* makes sure edge pairs are in order *)
sortGraph[g_Graph]:=Module[{verts,edges},
	Assert[UndirectedGraphQ[g]];
	verts=VertexList[g];
	edges=List@@@EdgeList[g];	
	Graph[verts,UndirectedEdge@@@deepSort[edges]]
];

getIndPoly[graph_Graph,OptionsPattern[]]:=(
	gr=sortGraph[graph];
	method=OptionValue[Method];
	polys=getIndPolyConnected[Subgraph[gr,#]]&/@ConnectedComponents[gr];
	Function@@{Times@@polys}
);

End[]
EndPackage[]









