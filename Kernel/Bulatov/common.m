(* ::Package:: *)

BeginPackage["Bulatov`common`"]

Tilde::usage="Tilde[a,b] is equivalent to Join[a,b]"
Element::usage="Element[a,b_List] is equivalent to MemberQ[b,a] (custom redefinition)"
NotMyElement::usage="NotElement[a,b_List] is equivalent to !MemberQ[b,a]"
Subset::usage="Subset[a,b] is equivalent to a\[Intersection]b==a"
NotSubset::usage=""
BracketingBar::usage="BracketingBar[a] is Length[a]"
disjoint::usage="disjoint[a,b] returns True if intersection of a and b is empty"
intersect::usage="like Intersection, but preserves original order"
fl1::usage="flatten one level of list"
idx::usage="idx[list,item] position of first occurence of item in list or -1 if doesn't occur"

deepSort::usage="deepSort[expr] sorts all levels of expression"

table::usage="table[a\[Element]list,expr] is equivalent to Table[expr,{a,list}]"
prod::usage="prod[var\[Element]list,expr] is equivalent to Times@@table[a\[Element]list,expr]"
sum::usage="sum[var\[Element]list,expr] is equivalent to Plus@@table[a\[Element]list,expr]"
anyTrue::usage="anyTrue[var\[Element]list,expr] checks if expression is true for any value of var in list"
allTrue::usage="allTrue[var\[Element]list,expr] checks if expression is true for all values of var in list"
partitionList::usage="partitionList[list,rows,cols,dummy:Null] partitions list into grid, truncating or filling with dummy values if necessary"
deepSort::usage="deepSort[list,1] is equivalent to Sort"

SetAttributes[sum,HoldAll];
SetAttributes[prod,HoldAll];
SetAttributes[table,HoldAll];
SetAttributes[anyTrue,HoldAll];
SetAttributes[allTrue,HoldAll];

Begin["Private`"]
Tilde[x__]:=Join[x];
Unprotect[Element,NotElement];
(* clear Combinatorica definitions conflicting *)
Element[x_,list_List]:=MemberQ[list,x];
NotElement[x_,list_List]:=Not[Element[x,list]];
Protect[Element,NotElement];
Subset[a_List,b_List]:=(Assert[OrderedQ[a]&&OrderedQ[b]];(a\[Intersection]b==a));
NotSubset[a_List,b_List]:=Not[Subset[a,b]];
Backslash[a_List,b_List]:=(Complement[a,b]);
BracketingBar[a_List]:=Length[a];
disjoint[A_,B_]:=Length[A\[Intersection]B]==0;
intersect[a_,b_]:=Select[a,MemberQ[b,#]&];
fl1[lst_]:=Flatten[lst,1];
idx[lst_,item_]:=If[MemberQ[lst,item],Position[lst,item][[1,1]],-1];

(*deepSort[expr_,n_:Infinity]:=Module[{k},
	k=If[n==Infinity,Depth[expr]-1,n];
	Map[Sort,{expr},k]//First
];*)

(* http://stackoverflow.com/questions/4939120/sort-all-levels-of-expression *)
deepSort[expr_] := Map[Sort, expr, {0, -2}]

table[var_\[Element]lis_,expr_]:=ReleaseHold[Hold[expr]/.HoldPattern[var]->#]&/@lis;
prod[var_\[Element]lis_,expr_]:=Times@@table[var\[Element]lis,expr];
sum[var_\[Element]lis_,expr_]:=Plus@@table[var\[Element]lis,expr];
anyTrue[var_\[Element]lis_,expr_]:=LengthWhile[Developer`FromPackedArray@lis,Not[TrueQ[ReleaseHold[Hold[expr]/.HoldPattern[var]->#]]]&]<Length[lis];
allTrue[var_\[Element]lis_,expr_]:=LengthWhile[Developer`FromPackedArray@lis,TrueQ[ReleaseHold[Hold[expr]/.HoldPattern[var]->#]]&]==Length[lis];

partitionList[list_,rows_,cols_,fill_:Null]:=Module[{size,needed,list2},
	size=Length[list];
	needed=rows*cols;
	list2=list[[;;Min[size,needed]]]\[Tilde]ConstantArray[fill,Max[0,needed-size]];
	Partition[list2,cols]
]

End[]
EndPackage[]





















