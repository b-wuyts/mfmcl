(* ::Package:: *)

BeginPackage["Bulatov`dictionary`"]

indices::usage="indices[dict] gives list of keys in dict"
values::usage="values[dict] gives list of values in dict"
items::usage="items[dict] gives list of key/value pairs"
indexQ::usage="indexQ[dict,idx] checks if given key has a mapping in dict"

Begin["Private`"]
index[downvalue_,dict_]:=(downvalue[[1]]/.HoldPattern[dict[x_]]->x)//ReleaseHold;
value[downvalue_]:=downvalue[[-1]];
indices[dict_]:=Map[#[[1]]/.{HoldPattern[dict[x_]]->x}&,DownValues[dict]]//ReleaseHold;
values[dict_]:=Map[#[[-1]]&,DownValues[dict]];
items[dict_]:=Map[{index[#,dict],value[#]}&,DownValues[dict]];
indexQ[dict_,index_]:=If[MatchQ[dict[index],HoldPattern[dict[index]]],False,True];

(* doesn't work *)
reverseLookup[multimap_,val_]:=Select[indices[multimap],MemberQ[multimap[#],val]&];
End[]
EndPackage[]
