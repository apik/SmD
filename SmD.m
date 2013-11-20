MakeDIANA[fname_]:=
    Module[{str,dianaIdxSubs},
           
           dianaIdxSubs = {lorentz[4]->lind,color[3]->colF,color[8]->colA,generation[3]->genidx};
           strModel = OpenWrite[fname<>".inc"];
           strSubs = OpenWrite[fname<>".hh"];
           (* Rules to convert conj and bar fields to symbols *)
           noanti[F_] := F/. {bar[a_] :> ToExpression["anti"<>ToString[a]], conj[a_] :> ToExpression["anti"<>ToString[a]]};
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

                         indexSTR[]:=
                         Module[{},  

                                ff[bb_]:=({#,bb[[2]]})& /@ bb[[1]];
                                ToExpression[StringReplace[ToString[noanti /@ fields],{"{"->"","}"->"",", "->"xx"}]] @@ ((ToString[#[[1]]]<>":"<>ToString[#[[2]]])& /@ Flatten[(ff[(# /. dianaIdxSubs)])& /@ indices,1])
                                
                               ];
                         
                         WriteString[strModel,"["<>StringReplace[ToString[noanti /@ fields],{"{"->"","}"->""}]<>"; ;"<>StringReplace[ToString[indexSTR[]],{"["->"(","]"->")"}]<>"]\n"];
                         
                         (* Output subs rule *)
                         WriteString[strSubs,"id "<> StringReplace[ToString[noanti /@ fields],{"{"->"","}"->"",", "->"xx"}] <> " = "<>"\n"];
                        ];
                  
                  (* (WriteString[strModel,ToString[#[[1]]]<>"\n"])& /@ vl; *)
                  (PrintVertex[#])& /@ vl;
                 ];
           WriteString[strModel,"\\Begin(vertex)\n"];
           PrintVertices /@ ITypes;
           WriteString[strModel,"\\End(vertex)\n"];
          ];



