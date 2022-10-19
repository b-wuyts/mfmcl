(* ::Package:: *)

BeginPackage["Bulatov`showGraphs`",{"Bulatov`common`"}];


showGraphs::usage="showGraphs[n,type1,!type2,type3] shows grid of graphs over n vertices with given types truncating to fit in grid";
getGraphs::usage="getGraphs[n,type1,!type2,type3] gives all graphs of given types";

Options[showGraphs]={gridSize->6};

Begin["`Private`"]

showGraphs[n_Integer,cl__]:=showGraphs[{n,n},cl];
	showGraphs[{nmin_Integer,nmax_Integer},cl__,OptionsPattern[]]:=Module[{},
	k=OptionValue[gridSize];
	graphs=getGraphs[{nmin,nmax},cl];
	Print["Total: ",Length[graphs]];
	graphs=graphs[[;;Min[k^2,Length[graphs]]]];
	GraphicsGrid[partitionList[Tooltip[GraphData[#],#]&/@graphs,k,k,Graph[{}]],ImageSize->400]
];

getGraphs[n_Integer,cl__]:=getGraphs[{n,n},cl];

getGraphs[{nmin_Integer,nmax_Integer},cl__]:=Module[{maxgraphnum=100},
	classes=Sort@{cl};
	builtinClasses=GraphData["Classes"]\[Tilde](Not/@GraphData["Classes"]);
	Assert[classes\[Subset]builtinClasses];

	posClasses=Cases[classes,_String];
	posGroup=If[posClasses=={},GraphData[nmin;;nmax],GraphData[posClasses,nmin;;nmax]];
	negClasses=classes\[Backslash]posClasses;
	negGroups=GraphData[#[[1]],nmin;;nmax]&/@negClasses;

	result=Complement[posGroup,Sequence@@negGroups]
]

End[]
EndPackage[]











