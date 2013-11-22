BeginPackage[ "SmD`",{"Global`",
                      "SARAH`", (* main functions *)
                      "Susyno`LieGroups`" (* conj[x] function *)
                     }]
MakeDIANA::usage = 
    "MakeDIANA[\"filename\"] creates filename.inc and filename.hh"

Print["SmD - SSARAH meets DIANA "(* ,SA`Version *) ]
Print["by Andrey Pikelner, 2013"]
Print[""];
(* Print["References:"] *)
(* Print["  Comput.Phys.Commun.181 (2010) 1077-1086. (arXiv:0909.2863[hep-ph])"] *)
(* Print["  Comput.Phys.Commun.182 (2011) 808-833. (arXiv:1002.0840[hep-ph])"] *)
(* Print["  Comput.Phys.Commun.184 (2013) 1792-1809. (arXiv:1207.0906[hep-ph])"] *)
(* Print["  arXiv:1309.7223[hep-ph]"] *)
Print["Download and Documentation:"]
Print["  https://github.com/apik/SmD"]
Print[""]


Begin[ "Private`"]

MakeDIANA[fname_]:=
    Module[{str,dianaIdxSubs},
           (* Testing contextes *)
          
           dianaIdxSubs = {lorentz[4]->lind,color[3]->colF,color[8]->colA,generation[3]->genidx};
           formIdxSubs = {lorentz[4]->mul,color[3]->cOlFind,color[8]->cOlAind,generation[3]->jj};
           strModel = OpenWrite[fname<>".inc"];
           strSubs = OpenWrite[fname<>".hh"];
           (* Rules to convert conj and bar fields to symbols *)
           noanti[F_] := F/. {bar[a_] :> ToExpression["anti"<>ToString[a]], conj[a_] :> ToExpression["anti"<>ToString[a]]};
           Print[noanti[conj[H0]]];
           PrintBosons[]:=
           Module[{scalarParticles, vectorParticles},
                  scalarParticles=Cases[Particles[GaugeES], {_, _, _, S, ___}, Infinity];
                  vectorParticles=Cases[Particles[GaugeES], {_, _, _, V, ___}, Infinity];
                  Print[vectorParticles];
                  ScalarLine[field_,nn_]:=
                  Module[{},
                         WriteString[strModel,"["<>ToString[field[[1]]]<>","<>ToString[noanti[AntiField[field[[1]]]]]<>"; ; VV(              vec,"];
                         If[MemberQ[field, {generation, 3}, Infinity], WriteString[strModel,ToString[generation[3]/.dianaIdxSubs]<>":1,"<>ToString[generation[3]/.dianaIdxSubs]<>":2,"],Null];
                         WriteString[strModel,ToString[50+nn]<>",num)"]
                         If[MemberQ[field, {color, 3}, Infinity], WriteString[strModel,"*d_("<>ToString[color[3]/.dianaIdxSubs]<>":1,"<>ToString[color[3]/.dianaIdxSubs]<>":2)"], 
                            If[MemberQ[field, {color, 8}, Infinity], WriteString[strModel,"*d_("<>ToString[color[8]/.dianaIdxSubs]<>":1,"<>ToString[color[8]/.dianaIdxSubs]<>":2)"], WriteString[strModel,"       "]]];
                         WriteString[strModel,";0;     line,  8, 3]\n"];
                        ];
                  VectorLine[field_,nn_]:=
                  Module[{},
                         WriteString[strModel,"["<>ToString[field[[1]]]<>","<>ToString[noanti[AntiField[field[[1]]]]]<>"; ; VV(lind:1, lind:2, vec,"];
                         If[MemberQ[field, {generation, 3}, Infinity], WriteString[strModel,ToString[generation[3]/.dianaIdxSubs]<>":1,"<>ToString[generation[3]/.dianaIdxSubs]<>":2,"],Null];
                         WriteString[strModel,ToString[40+nn]<>",num)"]
                         If[MemberQ[field, {color, 3}, Infinity], WriteString[strModel,"*d_("<>ToString[color[3]/.dianaIdxSubs]<>":1,"<>ToString[color[3]/.dianaIdxSubs]<>":2)"], 
                            If[MemberQ[field, {color, 8}, Infinity], WriteString[strModel,"*d_("<>ToString[color[8]/.dianaIdxSubs]<>":1,"<>ToString[color[8]/.dianaIdxSubs]<>":2)"], WriteString[strModel,"       "]]];
                         WriteString[strModel,";0;     wavy,  8, 3]\n"];
                        ];
                  
                  WriteString[strModel,"\\Begin(boson)\n"];
                  WriteString[strModel,"* Scalar particles\n"];
                  MapIndexed[(ScalarLine[#1,First[#2]])&, scalarParticles];
                  WriteString[strModel,"* Vector particles\n"];
                  MapIndexed[(VectorLine[#1,First[#2]])&, vectorParticles];
                  WriteString[strModel,"\\End(boson)\n"];
                  
                 ];
           PrintFermions[]:= (* fermion block *)
           Module[{fermionParticles},
                  WriteString[strModel,"\\Begin(fermion)\n"];
                  fermionParticles=Cases[Particles[GaugeES], {_, _, _, F, ___}, Infinity];
                  FermionLine[field_, nn_]:=
                  Module[{},
                         WriteString[strModel,"["<>ToString[field[[1]]]<>","<>ToString[noanti[AntiField[field[[1]]]]]<>"; ; FF(fnum,num,vec,"];
                         If[MemberQ[field, {generation, 3}, Infinity], WriteString[strModel,ToString[generation[3]/.dianaIdxSubs]<>":1,"<>ToString[generation[3]/.dianaIdxSubs]<>":2,"],Null];
                         WriteString[strModel,ToString[60+nn]<>")"];
                         If[MemberQ[field, {color, 3}, Infinity], WriteString[strModel,"*d_("<>ToString[color[3]/.dianaIdxSubs]<>":1,"<>ToString[color[3]/.dianaIdxSubs]<>":2)"], 
                            If[MemberQ[field, {color, 8}, Infinity], WriteString[strModel,"*d_("<>ToString[color[8]/.dianaIdxSubs]<>":1,"<>ToString[color[8]/.dianaIdxSubs]<>":2)"], WriteString[strModel,"       "]]];
                         WriteString[strModel,";0;    arrowLine,  0,"<>ToString[nn]<>"]\n"];
                        ];
                  MapIndexed[(FermionLine[#1,First[#2]])&, fermionParticles];
                  WriteString[strModel,"\\End(fermion)\n"];
                 ];
           PrintGhosts[]:= (* ghost block *)
           Module[{ghostParticles},
                  WriteString[strModel,"\\Begin(ghost)\n"];
                  ghostParticles=Cases[Particles[GaugeES], {_, _, _, G, ___}, Infinity];
                  GhostLine[field_,nn_]:=
                  Module[{},
                         WriteString[strModel,"["<>ToString[field[[1]]]<>","<>ToString[noanti[AntiField[field[[1]]]]]<>"; ; SS(vec,"]
                         If[MemberQ[field, {generation, 3}, Infinity], WriteString[strModel,ToString[generation[3]/.dianaIdxSubs]<>":1,"<>ToString[generation[3]/.dianaIdxSubs]<>":2,"],Null];
                         WriteString[strModel,ToString[70+nn]<>")"];
                         If[MemberQ[field, {color, 8}, Infinity], WriteString[strModel,"*d_("<>ToString[color[8]/.dianaIdxSubs]<>":1,"<>ToString[color[8]/.dianaIdxSubs]<>":2)"], WriteString[strModel,"       "]];
                         WriteString[strModel,";0;     wavy,  8, 3]\n"];
                        ];
                  MapIndexed[(GhostLine[#1,First[#2]])&, ghostParticles];

                  WriteString[strModel,"\\End(ghost)\n"];
                 ];
           PrintBosons[];
           PrintFermions[];
           PrintGhosts[];

           (* Group of vertices with the same Lorentz structure *)
           PrintVertices[VS_]:= 
           Module[{vl},
                  WriteString[strModel,"* "<>ToString[VS[[1]]]<>"\n"];

                  vl=SA`VertexList[VS[[1]]];
                  (* Print["VertexList::::::::::: ", vl];*)
                  (* Output single vertex *)
                  PrintVertex[v_]:=
                  Module[{fields,indices},
                         fields = If[Length[v] > 0, (If[Head[#] === conj|| Head[#] === bar,AntiField[getParticleName[#]],getParticleName[#]])& /@ v[[1]],{}];
                         
                         getPartIndices[p_]:=
                         Module[{lidx,lirSubs},
                                lidx = getIndizes[p];
                                (* Incorrect index range for adjoint representation *)
                                (* lirSubs  = (#[[1]]->#[[1]][#[[2]]])& /@ getIndexRange[p];  *)
                                (* Need to distinguish color,3 = colF and color,8 = colA *)
                                lirSubs = 
                                Module[{idxRanges},
                                       idxRanges=(Cases[Cases[Particles[GaugeES], {getParticleName[p], _, _, _, {___, {#, _}, ___}}, Infinity] , {#, _}, Infinity])& /@ lidx;
                                       (#[[1,1]]->#[[1,1]][#[[1,2]]])& /@ idxRanges
                                      ];
                                lidx /. lirSubs 
                               ];
                         
                         indices = If[Length[v] > 0, MapIndexed[({getPartIndices[#1],#2[[1]]})& , v[[1]]],{}];
                         
                         (* string of indices for vertex function *)
                         indexSTR[]:=
                         Module[{},  

                                ff[bb_]:=({#,bb[[2]]})& /@ bb[[1]];
                                (* ToExpression[StringReplace[ToString[noanti /@ fields],{"{"->"","}"->"",", "->"xx"}]] @@  *)((ToString[#[[1]]]<>":"<>ToString[#[[2]]])& /@ Flatten[(ff[(# /. dianaIdxSubs)])& /@ indices,1])
                                
                               ];
                         funcSTR[]:= StringReplace[ToString[noanti /@ fields],{"{"->"","}"->"",", "->"xx"}];
                               
                         
                         WriteString[strModel,"["<>StringReplace[ToString[noanti /@ fields],{"{"->"","}"->""}]<>"; ;"<>funcSTR[]<>"("];
                         (* Fermion line numbering *)
                         If[VS[[1]]===FFV || VS[[1]]===FFS, WriteString[strModel,"fnum,"],Null];                         
                         (* if RHS needs momentums of particles in vertex *)
                         MapIndexed[(If[Count[v[[2]],Mom[#1,_],Infinity] > 0, WriteString[strModel,"vec:"<>ToString[First[#2]]<>","]])&, v[[1]]];
                         WriteString[strModel,StringReplace[ToString[indexSTR[]],{"["->"(","]"->")","{"->"","}"->""}]<>")]\n"];
                         
                         (* Arguments of FORM subs instruction *)
                         ArgumentsSTR[]:=
                         Module[{},  
                                ff[bb_]:=({#,bb[[2]]})& /@ bb[[1]];
                                ((ToString[#[[1]]]<>ToString[#[[2]]]<>"?")& /@ Flatten[(ff[(# /. formIdxSubs)])& /@ indices,1])
                               ];
                         (* Output subs rule *)
                         SubMom[vvi_]:=
                         Module[{},
                                vvi[[2]] /. Mom[a_,b_]:> xxpmom[Position[vvi[[1]],a],b]
                                (* /. Mom[a_,b_]:> p[Position[vvi[[1]],a]] *)                                
                               ];
                         WriteString[strSubs,"id "<> StringReplace[ToString[noanti /@ fields],{"{"->"","}"->"",", "->"xx"}] <> ToString[ArgumentsSTR[]]<>" = "<>ToString[InputForm[SubMom[v]]]<>"\n"];
                        ];
                  
                  (* (WriteString[strModel,ToString[#[[1]]]<>"\n"])& /@ vl; *)
                  (PrintVertex[#])& /@ vl;
                 ];
           WriteString[strModel,"\\Begin(vertex)\n"];
           PrintVertices /@ ITypes;
           WriteString[strModel,"\\End(vertex)\n"];
          ]

End[]
    
EndPackage[]



