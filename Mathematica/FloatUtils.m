(* ::Package:: *)

BeginPackage["FloatUtils`"]

{nfbits, esize, fsize, bias, smallsubnormal, smallnormal, maxfloat, minroundable, maxroundable};
setfloatenv::usage = "setfloatenv[{n_Integer,e_Integer}] uses n_Integer and e_Integer to compute values that characterize the float environment";
f2x::usage = "f2x[x] converts the float x to its numerical value";
x2f::usage = "x2f[x] converts the number x into a float";
colorcodef::usage = "colorcodef[f] shows the bit fields of the float f";
UnderBar::usage = "Restricts the numerical vocabulary to what the float environment supports";
floatfix::usage = "Makes an adjustment to put the floats into increasing order";

Begin["`Private`"]
(* Creating an IEEE 754 float environment *)
setfloatenv[{n_Integer/;n>=4,e_Integer/;e>=2}]:=
	({nfbits,esize,fsize}={n,e,n-e-1};
	bias=2^(esize-1)-1;
	smallsubnormal=2^(1-bias-fsize);
	smallnormal=2^(1-bias);
	maxfloat=2^(bias)*( 1+(2^(fsize)-1)/(2^(fsize)));
	minroundable=smallsubnormal/2;
	maxroundable=2^(bias )*( 1+(2^(fsize)-1/2)/(2^(fsize))););
(* The function that converts a float to its numerical value:f2x *)
f2x [f_Integer/;0<=f<2^nfbits]:=
	Module[{emask=FromDigits[Table[1,esize],2],
		exp,fmask=FromDigits[Table[1,fsize],2],
		frac,sgn,smask=BitShiftLeft[1,nfbits-1]},
	sgn=BitShiftRight[BitAnd[smask,f],nfbits-1];
	exp=BitAnd[BitShiftRight[f,fsize],emask];
	frac=BitAnd[fmask,f];
	Which[
		exp==emask,If[frac==0,(-1)^sgn*\[Infinity],Indeterminate],
		exp==0,(-1)^sgn*2^(1-bias)*(frac / (2^fsize)),
		True,(-1)^sgn*2^(exp-bias)*(1+frac /( 2^fsize))]];
(* Converting numbers into floats *)
floatableQ[x_]:=(x===Indeterminate\[Or]x===\[Infinity]\[Or]x===-\[Infinity]\[Or]x\[Element]Reals)
x2f[x_/;floatableQ[x]]:=
	Module[{e=0,f,sgn,y},
		Which[
			x===Indeterminate||\[Not]floatableQ[x],
			FromDigits[Table[1,nfbits],2],(*NaN exceptions*)
			y=Abs[x];
			sgn=BitShiftLeft[Boole[x<0],nfbits-1];
			y==0,0,(*Zero exception*)
			y>=maxroundable,
			BitOr[sgn,BitShiftLeft[FromDigits[Table[1,esize],2],fsize]],
			(*Infinity and overflow exceptions*)
			y<smallnormal,BitOr[sgn,Round[y/smallsubnormal]],
			(*subnormal exceptions*)
			True,(*Else x is normal.Find the exponent and fraction fields.*)
			(*At most one of the next two While loops will execute.*)
			While[y>=2,y/=2;e++];
			While[y<1,y*=2;e--];
			(*We now have 1\[LessEqual]y<2.*)
			f=Round[(y-1)*2^fsize];
			(*This may round up to 2,so we add f,instead of ORing it.*)
			BitOr[sgn,BitShiftLeft[bias+e,fsize]]+f]]
colorcodef[f_/;floatableQ[f]]:=
	Module[{fbits=IntegerDigits[f,2,nfbits]},
		Row[{Style[fbits[[1]], RGBColor[1, 0.125, 0]],"",
		Style[Row[Take[fbits,{2,esize+1}]],RGBColor[0.25, 0.5, 1]],"",
		Row[Take[fbits,-nfbits+esize+1]]}]];
(* Restriction to a float vocabulary:the underbar operator *)
UnderBar[x_]:=f2x[x2f[x]];
SetAttributes[UnderBar,Listable];
floatfix[i_Integer]:=If[i<2^(nfbits-1),2^(nfbits)-i-1,i-2^(nfbits-1)];

End[]
EndPackage[]



