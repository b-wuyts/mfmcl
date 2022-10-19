(* ::Package:: *)

BeginPackage["Bulatov`chordal`",{"Bulatov`common`","Bulatov`treeDecomposition`"}];

isChordal::usage="isChordal[g] returns True if g is chordal";

getPerfectEliminationOrder::usage="getPerfectEliminationOrder[g] returns perfect elimination order if graph is chordal";

isPerfectEliminationOrder::usage="isPerfectEliminationOrder[g,order] returns True if order is a perfect elimination order on g"

getMaxCardinalityOrder::usage="getMaxCardinalityOrder[g] returns elimination order resulting from maximum cardinality search"

getMinFillOrder::usage="getMinFillOrder[g] gets elimination order that minimizes fill in corresponding triangulation";

getMaximalCliques::usage="getMaximalCliques[g] returns a list of maximal clique resulting from maximum cardinality search. Guaranteed to find all cliques of chordal graph g. ";

triangulateGraph::usage="triangulateGraph[g] gets the triangulation of the graph from default tree decomposition of the graph";

relabelGraph::usage="relabelGraph[g,ll] relabels vertices 1,2,3... to have labels ll[1],ll[2],...., gives a graph where adjacency matrix is in canonical order (positions of rows correspond to vertex labels)"

makeChordalGraph::usage="makeChordalGraph[g,width] makes chordal graph whose clique structure is tree 'g' and tree width is 'width'"

medusaGraph::usage="medusaGraph[numArms,len] gives star graph with each arm extended by `len` vertices"


Options[medusaGraph]=
{
GraphStyle->"DiagramBlack"
};

Begin["`Private`"]

neighbors[v_]:=VertexList@NeighborhoodGraph[g,v]\[Backslash]{v};
isSimplicial[v_]:=CompleteGraphQ[Subgraph[g,{v}\[Union]predecessors[v]]];
cardinality[v_]:=\[LeftBracketingBar]predecessors[v]\[RightBracketingBar];
negCardinality[v_]:=-cardinality[v];
(*maxCardinalityVertex[verts_]:=verts[[Last@Ordering[cardinality/@verts]]]*)
maxCardinalityVertex[verts_]:=verts[[First@Ordering[negCardinality/@verts]]];


(* p .21 in Blair's "Chordal Graphs" *)
isRepresentativeVert[v_]:=(idx[order,v]==\[LeftBracketingBar]order\[RightBracketingBar] \[Or] cardinality[v]>=cardinality[successor[v]])

(* neighbors of v before v in elimination order *)
predecessors[v_]:=If[Element[v,order],neighbors[v]\[Intersection]order[[;;idx[order,v]]],neighbors[v]\[Intersection]order];
(* vertex after v in elimination order *)
successor[v_]:=(Assert[idx[order,v]<Length[order]];order[[idx[order,v]+1]]);

(* initializes graph and computes maximum cardinality ordering *)
initChordalGraph[gg_]:=(
	g=gg;
	V=Sort[VertexList[g]];
	order={};
	Table[AppendTo[order,maxCardinalityVertex[V\[Backslash]order]],{\[LeftBracketingBar]V\[RightBracketingBar]}];
);

isPerfectOrder[]:=(
	allTrue[v\[Element]V,isSimplicial[v]]	
);

(* use the fact that every maximum cardinality ordering is perfect ordering *)
isChordal[gg_Graph]:=(
	initChordalGraph[gg];
	isPerfectOrder[]
);

getPerfectEliminationOrder[gg_Graph]:=(
	initChordalGraph[gg];
	Assert[isPerfectOrder[],"Graph is not chordal, no perfect elimination ordering possible"];
	order
);

isPerfectEliminationOrder[gg_Graph,ord_List]:=(
	initChordalGraph[gg];
	order=ord;
	isPerfectOrder[]
);

getMaxCardinalityOrder[gg_Graph]:=(
	initChordalGraph[gg];
	order
);

getMinFillOrder[gg_Graph]:=(
	{jnodes,jedges}=findTreeDecomposition[gg,PrimaryHeuristic->"minfill"];
	triangulated=triangulateGraph[gg,jnodes];
	getPerfectEliminationOrder[triangulated]
);

getGridOrder[gg_Graph]:=(
	{jnodes,jedges}=findTreeDecomposition[gg,PrimaryHeuristic->"grid"];
	triangulated=triangulateGraph[gg,jnodes];
	getPerfectEliminationOrder[triangulated]
);


getMaximalCliques[gg_Graph]:=(
	initChordalGraph[gg];
	candidates=Union[{#},predecessors[#]]&/@Select[V,isRepresentativeVert];
	Select[candidates,CompleteGraphQ[Subgraph[gg,#]]&]
);

triangulateGraph[g_Graph]:=(
	{bags,bagPairs}=findTreeDecomposition[g];
	gstyle=GraphStyle/.AbsoluteOptions[g,GraphStyle];
	vcoords=VertexCoordinates/.AbsoluteOptions[g,VertexCoordinates];
	vcoords=Thread[VertexList[g]->vcoords];
	makeClique[bag_]:=(UndirectedEdge@@@Select[Subsets[bag,{2}],OrderedQ]);
	Graph[Union@Flatten[makeClique/@bags],VertexCoordinates->vcoords,GraphStyle->gstyle]
);

triangulateGraph[g_Graph,bags_]:=(
	vcoords=VertexCoordinates/.AbsoluteOptions[g,VertexCoordinates];
	gstyle=GraphStyle/.AbsoluteOptions[g,GraphStyle];
	vcoords=Thread[VertexList[g]->vcoords];
	makeClique[bag_]:=(UndirectedEdge@@@Select[Subsets[bag,{2}],OrderedQ]);
	Graph[Union@Flatten[makeClique/@bags],VertexCoordinates->vcoords,GraphStyle->gstyle]
);

relabelGraph[g_Graph,labeling_]:=Module[{vcoords,gstyle,labelMap,adjMat,newOrder,newCoords,verts},
	verts=VertexList@g;
	vcoords=VertexCoordinates/.AbsoluteOptions[g,VertexCoordinates];
	gstyle=GraphStyle/.AbsoluteOptions[g,GraphStyle];
	labelMap=Thread[Range[Length[verts]]->labeling];

	adjMat=Normal@AdjacencyMatrix[g];
	newOrder=Ordering[VertexList[g]/.labelMap];

	newCoords=Thread[(VertexList[g]/.labelMap)->vcoords];

	AdjacencyGraph[adjMat[[newOrder,newOrder]],VertexCoordinates->newCoords,GraphStyle->gstyle]
];

makeChordalGraph[g_Graph,width_Integer]:=Module[{ch,branch,tree,counter},
	Assert[width>=1,"tree width must be at least 1 (that makes singly connected tree)"];
	Assert[AcyclicGraphQ[g],"clique graph must be a tree"];
	ch[{i_}]:=Cases[tree,{i,_}];
	ch[{i_,j_}]:=Cases[tree,{j,Except[i]}];
	
	branch[node_]:=(
		counter=width+1;
		firstBag=Range[width+1];
		AppendTo[edges,#]&/@Subsets[firstBag,{2}];
		branch[Rest[firstBag],#]&/@ch[{node}]
	);

	branch[frontier_,subtree_]:=Block[{current},
		(* add connnection to all vertices in frontier *)
		counter++;
		AppendTo[edges,{counter,#}]&/@frontier;

		current=counter;
		branch[Rest[frontier]\[Tilde]{current},#]&/@ch[subtree]
	];

	tree=List@@@EdgeList[g];
	tree=tree\[Tilde](Reverse/@tree);
	counter=0;
	edges={};
	branch[Min[VertexList[g]]];
	Graph[UndirectedEdge@@@edges]
]


medusaGraph[numarms_,len_,OptionsPattern[]]:=Module[{arms},
	arms=Table[PathGraph[{1}\[Tilde]Range[len k+1,len k+len]],{k,1,numarms}];
	gstyle=OptionValue[GraphStyle];
	AdjacencyGraph[AdjacencyMatrix[GraphUnion@@arms],GraphStyle->gstyle]
]

End[];
EndPackage[];



