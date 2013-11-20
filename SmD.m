MakeDIANA[fname_]:=
    Module[{str,dianaIdxSubs},
           
           dianaIdxSubs = {lorentz[4]->lmu,color[3]->colF,color[8]->colA,generation[3]->genidx};
           strModel = OpenWrite[fname<>".inc"];
           (* Rules to convert conj and bar fields to symbols *)
           noanti[F_] := F/. {bar[a_] :> ToExpression["anti"<>ToString[a]], conj[a_] :> ToExpression["anti"<>ToString[a]]};
           PrintBosons[]:=
           Module[{scalarParticles, vectorParticles},
                  scalarParticles=Cases[Particles[GaugeES], {_, _, _, S, ___}, Infinity];
                  vectorParticles=Cases[Particles[GaugeES], {_, _, _, V, ___}, Infinity];
                  ScalarLine[field_]:=
                  Module[{},
                         WriteString[strModel,"["<>ToString[field[[1]]]<>","<>ToString[noanti[AntiField[field[[1]]]]]<>"; ; VV(              vec,51,num)"];
                         If[MemberQ[field, {color, 8}, Infinity], WriteString[strModel,"*d_(colind:1,colind:2)"], WriteString[strModel,"       "]];
                         WriteString[strModel,";0;     line,  8, 3]\n"];
                        ];
                  VectorLine[field_]:=
                  Module[{},
                         WriteString[strModel,"["<>ToString[field[[1]]]<>","<>ToString[noanti[AntiField[field[[1]]]]]<>"; ; VV(lind:1, lind:2, vec,51,num)"]
                         If[MemberQ[field, {color, 8}, Infinity], WriteString[strModel,"*d_(colind:1,colind:2)"], WriteString[strModel,"       "]];
                         WriteString[strModel,";0;     wavy,  8, 3]\n"];
                        ];
                  
                  WriteString[strModel,"\\Begin(boson)\n"];
                  WriteString[strModel,"* Scalar particles\n"];
                  ScalarLine /@ scalarParticles;
                  WriteString[strModel,"* Vector particles\n"];
                  VectorLine /@ vectorParticles;
                  WriteString[strModel,"\\End(boson)\n"];
                  
                 ];
           PrintFermions[]:= (* fermion block *)
           Module[{fermionParticles},
                  WriteString[strModel,"\\Begin(fermion)\n"];
                  fermionParticles=Cases[Particles[GaugeES], {_, _, _, F, ___}, Infinity];
                  WriteString[strModel,"\\End(fermion)\n"];
                 ];
           PrintGhosts[]:= (* ghost block *)
           Module[{ghostParticles},
                  WriteString[strModel,"\\Begin(ghost)\n"];
                  ghostParticles=Cases[Particles[GaugeES], {_, _, _, G, ___}, Infinity];
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
                  Print["VertexList::::::::::: ", vl];
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
                                lirSubs = Module[{idxRanges},
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
                        ];
                  
                  (* (WriteString[strModel,ToString[#[[1]]]<>"\n"])& /@ vl; *)
                  (PrintVertex[#])& /@ vl;
                 ];
           WriteString[strModel,"\\Begin(vertex)\n"];
           PrintVertices /@ ITypes;
           WriteString[strModel,"\\End(vertex)\n"];
          ];



