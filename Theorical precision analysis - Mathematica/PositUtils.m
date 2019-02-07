(* ::Package:: *)

BeginPackage["PositUtils`"]

{nbits,es,npat,useed,minpos,maxpos,qsize,qextra};
setpositenv::usage = "setpositenv[{nbits, es}] uses nbits and es to compute values that characterize the posit environment";
colorcodep::usage = "colorcodep[p] shows the bit fields of the posit p";
p2x::usage = "p2x[x] converts the posit x to its numerical value";
x2p::usage = "x2p[x] converts the number x into a posit";
OverBar::usage = "Restricts the numerical vocabulary to what the posit environment supports";
positfix::usage = "Makes an adjustment to put the posits into increasing order";
ringplot::usage = "Shows posits representing projective real numbers arranged on a ring";

Begin["`Private`"]

setpositenv[{n_Integer/;n>=2,e_Integer/;e>=0}]:=
	({nbits, es} = {n,e};
	npat=2^nbits;
	useed=2^(2^es);
	{minpos, maxpos}={useed^(-nbits+2), useed^(nbits-2)};
	qsize=2^(Ceiling[Log[2,(nbits-2)*2^(es+2)+5]]);
	qextra=qsize-(nbits-2)*2^(es+2);)
(*Extracting the sign bit*)
positQ[p_Integer]:=0<=p<npat
twoscomp[sign_,p_]:=Mod[If[sign>0,p,npat-p],npat]
signbit[p_/;positQ[p]]:=IntegerDigits[p,2,nbits][[1]]
(*Extracting the regime bits*)
regimebits[p_/;positQ[p]]:=
	Module[{q=twoscomp[1-signbit[p],p],bits,bit2,npower,tempbits},
		bits=IntegerDigits[q,2,nbits];
		bit2=bits[[2]];(*Look for the run length after the sign bit.*)tempbits=Join[Drop[bits,1],{1-bit2}];(*Drop the sign bit,but append a complement bit as a sure-fire way to end the run.*)npower=(Position[tempbits,1-bit2,1,1])[[1]]-1;
		(*Find first opposite bit.*)Take[bits,{2,Min[npower+1,nbits]}]]
regimevalue[bits_]:=If[bits[[1]]==1,Length[bits]-1,-Length[bits]]
(*Extracting the exponent bits*)
exponentbits[p_/;positQ[p]]:=
	Module[{q=twoscomp[1-signbit[p],p],bits,startbit},
		startbit=Length[regimebits[q]]+3;
		bits=IntegerDigits[q,2,nbits];
		If[startbit>nbits,{},Take[bits,{startbit,Min[startbit+es-1,nbits]}]]]
(*Extracting the fraction bits*)
fractionbits[p_/;positQ[p]]:=
	Module[{q=twoscomp[1-signbit[p],p],bits,startbit},
		startbit=Length[regimebits[q]]+3+es;
		bits=IntegerDigits[q,2,nbits];
		If[startbit>nbits,{},Take[bits,{startbit,nbits}]]]
colorcodep[p_/;positQ[p]]:=
	Module[{s=signbit[p],r=regimebits[p],e=exponentbits[p],f=fractionbits[p]},
		Row[{IntegerString[p,2,nbits],"\[RightArrow]",Style[If[s ==0,"+","-"], RGBColor[1, 0.125, 0]],
		Style[Row[r],RGBColor[0.8, 0.6, 0.2] ],If[Length[r]<=nbits-2,Style[1-r[[1]] , RGBColor[0.6, 0.4, 0.2]],""],
		If[Length[e]>0,Style[Row[e], RGBColor[0.25, 0.5, 1]],""],If[Length[f]>0,Row[f],""]}]]
(*The p2x function*)
p2x[p_/;positQ[p]]:=
	Module[{s=(-1)^(signbit[p]), k=regimevalue[regimebits[p]],
		e=exponentbits[p], f=fractionbits[p]},
		e=Join[e, Table[0,es-Length[e]]];
		(* Pad with 0s on the right of they are clipped off. *)
		e=FromDigits[e,2];
		If[f=={},f=1,f=1+FromDigits[f,2]*2^(-Length[f])];
		Which[
			p==0,0,
			p==npat/2,ComplexInfinity,
			(* The two exception values, 0 and +-Infinity *)
			True, s*useed^k*2^e*f]]
(* Converting values into posits *)
(* Building the x2p conversion function *)
positableQ[x_]:=(Abs[x] == \[Infinity] || x\[Element]Reals)
x2p[x_/;positableQ[x]]:=
	Module[{i,p,e=2^(es-1),y=Abs[x]},
		Which[(*First,take care of the two exception values:*)y == 0,0,
			(*all 0 bits s*)y == \[Infinity],BitShiftLeft[1,nbits-1],
			(*1 followed by all 0 bits*)True,If[y>=1,(*Northeast quadrant:*)p=1;
			i=2;(*Shift in 1s from the right and scale down.*)
			While[y>=useed&&i<nbits,{p,y,i}={2 p+1,y/useed,i+1}];
			p=2 p;i++,(*Else,southeast quadrant:*)p=0;
			i=1;(*Shift in 0s from the right and scale up.*)
			While[y<1&&i<=nbits,{y,i}={y*useed,i+1}];
			If[i>=nbits,p=2;
				i=nbits+1,p=1;
				i++]];
			(*Extract exponent bits:*)While[e>1/2&&i<=nbits,p=2 p;
			If[y>=2^e,y/=2^e;p++];
			e/=2;i++];
			y--;(*Fraction bits;subtract the hidden bit*)
			While[y>0&&i<=nbits,y=2 y;p=2 p+Floor[y];y-=Floor[y];i++];
			p*=2^(nbits+1-i);
			i++;
			(*Round to nearest;tie goes to even*)
			i=BitAnd[p,1];p=Floor[p/2];
			p=Which[i == 0,p,(*closer to lower value*)y == 1||y == 0,
				p+BitAnd[p,1],(*tie goes to nearest even*)True,p+1 (*closer to upper value*)];
			Mod[If[x<0,npat-p,p],npat (*Simulate 2's complement*)]]]
OverBar[x_]:=p2x[x2p[x]];
SetAttributes[OverBar,Listable];
positfix[i_Integer]:=If[i<npat/2,i+npat/2,i-npat/2];

ringplot:=(
	nums=
		Table[
			Row[{Style[p2x[j]//InputForm, Purple]}],
			{j,0,npat-1}];
	nums[[x2p[ComplexInfinity]+1]]=
		Row[{Style["\[PlusMinus]\[Infinity]", Purple]}];
	bits=
		Table[
			Row[{Style[colorcodep[j], FontFamily->"Courier", FontWeight->Bold]}],
			{j,0,npat-1}];
	(* Plot the circle *)
	Graphics[{RGBColor[0,0.75,0],Thick,Circle[{0,0},1,{Pi/2-0.05, 0}],
		Circle[{0,0},1,{Pi/2+0.05, 2Pi}],
		{Arrowheads[0.06],Arrow[{{0.2,0.98},{0.01,1}}]},
		{Arrowheads[0.06],Arrow[{{-0.2,0.98},{-0.01,1}}]},
		Black,PointSize[Large],Point[c=CirclePoints[{1,-Pi/2},npat]],
		Table[{
Text[Style[nums[[i]],15],c[[i]],Scaled[{2/(StringLength[ToString[nums[[i]]]]^0.35),.5}],c[[i]]], Text[Style[bits[[i]],15],c[[i]],Scaled[{-.1,.5}],c[[i]]],Text[Style[nums[[npat-i+1]],15],c[[-i]],Scaled[{-1/StringLength[ToString[nums[[npat-i+1]]]],.5}],-c[[-i]]],
Text[Style[bits[[npat-i+1]],15],c[[-i]],Scaled[{1.1,.5}],-c[[-i]]]
},{i,npat/2}]
	}]
);

End[]
EndPackage[]
