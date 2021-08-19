(* ::Package:: *)

(* ::Section:: *)
(*Introduction*)


(* ::Text:: *)
(*Shadow channel*)


(* ::Section:: *)
(*Load dependencies*)


(* Load dependencies *)
ClearAll["Global`*"]
Get[(NotebookDirectory[]<>"/QuantumMinh.m") ];
SetDirectory[NotebookDirectory[]];
(*<<MaTeX`*)
color = ColorData[97,"ColorList"]


(* ::Section:: *)
(*Setup*)


nSys = 3;
nAnc = 3;


(* ::Text:: *)
(*Give  a channel \[CapitalGamma]. In this case just a depolarizing channel first*)


\[CapitalGamma][\[Rho]_]:=1/2 (\[Rho]+SparseIdentityMatrix[Dimensions[\[Rho]][[1]]]/Dimensions[\[Rho]][[1]])


ChangeAxes={({
 {1/Sqrt[2], 1/Sqrt[2]},
 {-1/Sqrt[2], 1/Sqrt[2]}
}),({
 {1/Sqrt[2], -I/Sqrt[2]},
 {-I/Sqrt[2], 1/Sqrt[2]}
}),({
 {1, 0},
 {0, 1}
})};


drawRandU2[n_,d_:2]:=Table[RandomUnitary[d],n];
drawRandPaulis[n_]:=Table[ChangeAxes[[RandomChoice[{1,2,3}]]],n];
applyRandomLocalU[Ulist_,\[Rho]0_]:=Block[{\[Rho],U},
	\[Rho] = \[Rho]0;
	Table[
		U = KP[SparseIdentityMatrix[2^(i-1)],Ulist[[i]],SparseIdentityMatrix[2^(Length[Ulist]-i)]];
		\[Rho] = U . \[Rho] . U\[ConjugateTranspose];
	,{i,1,Length[Ulist]}];
	\[Rho]
]
randString[n_,d_:2]:=Table[Round[RandomReal[]],n];
basisState[i_,d_:2]:=SparseArray[{{i+1,1}->1},{d,1}];
randProdState[n_,d_:2]:=KP@@(basisState[#,d]&/@randString[n,d]);
string2State[str_,d_:2]:=KP@@(basisState[#,d]&/@str);
projector[i_,d_:2]:=# . #\[ConjugateTranspose]&@basisState[i,d]
getCondOp[n_,prevOutcome_,normalized_:False]:=Block[{Op,nMeasured},
	Op = {{1}};
	nMeasured = Length[prevOutcome];
	Table[
		Op = KP[Op,projector[prevOutcome[[k]]]]
	,{k,1,nMeasured}];
If[normalized,
	Op = KP[Op,\[Sigma][[1]]],
		Op = KP[Op,\[Sigma][[4]]];
];
	Table[
		Op = KP[Op,SparseIdentityMatrix[2]]
	,{k,nMeasured + 2,n}];
	Op
]
getSnapshot[\[Rho]_,d_:2]:=Block[{outcome,n,prob0,nextOut},
	outcome = {};
	n = Round[Log[d,Length[\[Rho]]]];
	
	Table[
		Op = getCondOp[n,outcome];
Op2 = getCondOp[n,outcome,True];
	
		prob0 = Re[(1+Tr[\[Rho] . Op]/Tr[\[Rho] . Op2])/2];
		nextOut = If[#<prob0,0,1]&@RandomReal[];
		AppendTo[outcome,nextOut];
	,{j,1,n}];
	outcome
]
invertSnapshot[initialState_,UinList_,snapshot_,UoutList_]:=Block[{n,M1list,M2list,eta},
	n = Length[snapshot];
	M1list = Table[
		3 #\[ConjugateTranspose] . projector[initialState[[j]]] . #&@UinList[[j]] - SparseIdentityMatrix[2^n]
	,{j,1,n}];
	M2list = Table[
		3 #\[ConjugateTranspose] . projector[snapshot[[j]]] . #&@UoutList[[j]] - SparseIdentityMatrix[2^n]
	,{j,1,n}];
	eta = KP@@Join[M1list,M2list];
]
getApproxChoiMatrix[initialStateList_,UinList_,snapshotList_,UoutList_]:=Block[{eta,nSample},
	nSample = Length[initialStateList];
	eta =1/nSample \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(j = 1\), \(nSample\)]\(invertSnapshot[initialStateList[\([j]\)], UinList[\([j]\)], snapshotList[\([j]\)], UoutList[\([j]\)]]\)\)
]
maximallyEntangledState[d_]:=1.0/Sqrt[d] Total[KP[basisState[#,d],basisState[#,d]]&@Range[1,d]];
applyChannel[\[Rho]_,\[Eta]_]:=partrace[KP[SparseIdentityMatrix[Length[\[Rho]]],Transpose[\[Rho]]] . \[Eta],2,{Length[\[Rho]],Length[\[Rho]]}];


nSample = 1000;
getSamples[\[Eta]_,nSample_,d_:2]:=Block[{samples,Uin,Uout,\[Rho],snapshot,nSys},
	nSys = Round[Log[d,Length[\[Eta]]]/2];
	samples = {};
	Table[
		Uin = drawRandPaulis[nSys];
		Uout = drawRandPaulis[nSys];
		initialState = randString[nSys];
		\[Rho] = KP@@(projector/@initialState);
		\[Rho] = applyRandomLocalU[Uin,\[Rho]];
		\[Rho] = applyChannel[\[Rho],\[Eta]];
		\[Rho] = applyRandomLocalU[Uout,\[Rho]];
		snapshot = getSnapshot[\[Rho]];
		AppendTo[samples,{initialState,Uin,snapshot,Uout}];
	,{s,1,nSample}];
	samples
]


getChannelFromSample[\[Rho]_,sample_]:=Block[{n,M1list,M2list,initialState,Uin,Uout,snapshot},
	initialState = sample[[1]];
	Uin = sample[[2]];
	snapshot = sample[[3]];
	Uout = sample[[4]];
	n = Length[snapshot];
	M1list = Table[
		3 #\[ConjugateTranspose] . projector[initialState[[j]]] . #& @Uin[[j]] - SparseIdentityMatrix[2]
	,{j,1,n}];
	M2list = Table[
		3 #\[ConjugateTranspose] . projector[snapshot[[j]]] . #& @Uout[[j]] - SparseIdentityMatrix[2]
	,{j,1,n}];
	Tr[Transpose[\[Rho]] . KP@@M2list]*(KP@@M1list)
]


(* ::Subsection:: *)
(*Channel Concatenation*)


getConcatMatrix[samples1_,samples2_]:= Block[{nsysqubits,nsamp1,nsamp2,concatMatrix,sysstate,sysmats,ancstate,ancmats,M1list,M2list,Mcombine},
nsysqubits = Length[samples1[[1]][[1]]];
nsamp1 = Length[samples1];
nsamp2=Length[samples2];
concatMatrix = Array[0&,{nsamp1,nsamp2}];
Do[
sysstate = samples1[[i]][[3]];
sysmats =  samples1[[i]][[4]];
ancstate = samples2[[j]][[1]];
ancmats = samples2[[j]][[2]];
M1list = Table[
		3 #\[ConjugateTranspose] . projector[sysstate[[jj]]] . #& @sysmats[[jj]] - SparseIdentityMatrix[2]
	,{jj,1,nsysqubits}];
	M2list = Table[
		3 #\[ConjugateTranspose] . projector[ancstate[[jj]]] . #& @ancmats[[jj]] - SparseIdentityMatrix[2]
	,{jj,1,nsysqubits}];
concatMatrix[[i,j]]= Product[{1,0,0,1} . (KP[M1list[[k]],M2list[[k]]]) . {1,0,0,1}/2,{k,nsysqubits}] ;
,{i,1,nsamp1},{j,1,nsamp2}];
concatMatrix
] (*Method used in channel concatenation. Implements the projection of the "intermediate" system and ancilla onto the maximally entangled state. 
Returns matrix whose ith,jth entry is the projection result for the ith sample from channel 1 and the jth sample from channel 2*)
getConcatChannel[samples1_,samples2_]:=Block[{concMat,nsamp1,nsamp2,\[Rho]outShadowFinal},
nsamp1 = Length[samples1];
nsamp2=Length[samples2];
concMat =getConcatMatrix[samples1,samples2];

\[Rho]outShadowFinal = 1/nSample^2 Sum[getChannelFromSample[\[Rho]in,{samples1[[i]][[1]],samples1[[i]][[2]],samples2[[j]][[3]],samples2[[j]][[4]]}] concMat[[i,j]]
,{i,1,nsamp1},{j,1,nsamp2}];
\[Rho]outShadowFinal
]


(* ::Section:: *)
(*Test*)


nSys = 2;
\[Eta] = RandomState[2^(2nSys)];

\[Rho]in = RandomState[2^nSys];
\[Rho]out = applyChannel[\[Rho]in,\[Eta]];






nSample = 10000;
Monitor[
	samples = getSamples[\[Eta],nSample]
,s];


\[Rho] = 0;
tab = Table[
	\[Rho]k = getChannelFromSample[\[Rho]in,samples[[k]]];
	\[Rho] = (\[Rho] (k-1) + \[Rho]k)/k;
	Norm[\[Rho]out-\[Rho]]
,{k,1,nSample}];


plot = ListLogLogPlot[tab,PlotRange->All];
fit = FindFit[Log[{Range[1,nSample],tab}\[Transpose]],{a x + b},{a,b},x];
fitplot = LogLogPlot[(E^b x^a)/.fit,{x,1,10^4}];
Show[plot,fitplot]
fit


Eigenvalues[\[Rho]]


Chop@Eigenvalues[\[Rho]out]


(* ::Subsection:: *)
(*Channel Concatenation*)


nSys = 2;
\[Eta]1 = RandomState[2^(2nSys)];
\[Eta]2 = RandomState[2^(2nSys)];

\[Rho]in = RandomState[2^nSys];
\[Rho]out1 = applyChannel[\[Rho]in,\[Eta]1];
\[Rho]out2= applyChannel[\[Rho]out1,\[Eta]2];


tabcomp = Table[{0,0},70];
Monitor[Do[
nSample = Round[2 10^(3 logtestn/100)];
samples1 = getSamples[\[Eta]1,nSample];
samples2 = getSamples[\[Eta]2,nSample];


tabcomp[[logtestn]]={nSample,Norm[getConcatChannel[samples1,samples2]-\[Rho]out2]}
,{logtestn,1,70}];(*the shadow representation of system matrix after channel composition is very similar to getting the output matrix from a single channel,
 but the weight of different samples is given by the what I've called the concatenation matrix*)
,logtestn]


plot = ListLogLogPlot[tabcomp,PlotRange->All,AxesLabel->{"Nsample per channel","Error"}];
fit = FindFit[Log[tabcomp],{a x + b},{a,b},x];
fitplot = LogLogPlot[(E^b x^a)/.fit,{x,1,10^4}];
Show[plot,fitplot]
fit



