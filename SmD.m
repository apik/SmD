BeginPackage[ "SmD`",{"Global`" ,
                      "SARAH`"(* main functions *)
                      (* "Susyno`LieGroups`" (\* conj[x] function *\) *)
                     }]

              (* Defualt package options: *)
              Options[MakeDIANA] = {Model -> "SM", State->GaugeES};

              MakeDIANA::usage = 
              "MakeDIANA[\"filename\", Model->\"SM\", State->GaugeES] creates filename.inc and filename.hh"
              
              Print["SmD - SARAH meets DIANA "(* ,SA`Version *) ]
              Print["by Andrey Pikelner, 2013"]
              Print[""];
              Print["Download and Documentation:"]
              Print["  https://github.com/apik/SmD"]
              Print[""]

              
              Begin[ "Private`"]
              
              MakeDIANA[fname_,OptionsPattern[]]:=
              Module[{str,formIdxSubs},

                     Start[OptionValue[Model]];
                     MakeVertexList[OptionValue[State]];

                     (* Substitutions for DIANA index groups definitions *)
                                          
                     Format[lorentz[4]]:="lind6666666";
                     Format[color[3]]:="colF";
                     Format[color[8]]:="colA";
                     Format[generation[3]]:="genER";
                     
                     (* Print["Context: ",Context[dianaIdxSubs]]; *)

                     (* Substitutions for FORM code index names *)
                     formIdxSubs  = {lorentz[4]->mul,color[3]->cOlFind,color[8]->cOlAind,generation[3]->jj};

                     (* DIANA model file *)
                     strModel = OpenWrite[fname<>".inc"];
                     (* FORM program subsitution rules *)
                     strSubs = OpenWrite[fname<>".hh"];

                     (* Rules to convert conj and bar fields to symbols *)
                     noanti[F_] := F/. {SARAH`bar[a_] :> ToExpression["anti"<>ToString[a]], Susyno`LieGroups`conj[a_] :> ToExpression["anti"<>ToString[a]]};

                     (* Propagators output functions *)
                     PrintBosons[]:=
                     Module[{scalarParticles, vectorParticles},
                            scalarParticles=Cases[SARAH`Particles[SARAH`GaugeES], {_, _, _, SARAH`S, ___}, Infinity];
                            vectorParticles=Cases[SARAH`Particles[SARAH`GaugeES], {_, _, _, SARAH`V, ___}, Infinity];
                            (* Print[vectorParticles]; *)
                            ScalarLine[field_,nn_]:=
                            Module[{},
                                   WriteString[strModel,"["<>ToString[field[[1]]]<>","<>ToString[noanti[SARAH`AntiField[field[[1]]]]]<>"; ; VV(              vec,"];
                                   If[MemberQ[field, {SARAH`generation, 3}, Infinity], WriteString[strModel,ToString[generation[3]]<>":1,"<>ToString[generation[3]]<>":2,"],Null];
                                   WriteString[strModel,ToString[50+nn]<>",num)"]
                                   If[MemberQ[field, {SARAH`color, 3}, Infinity], WriteString[strModel,"*d_("<>ToString[color[3]]<>":1,"<>ToString[color[3]]<>":2)"], 
                                      If[MemberQ[field, {SARAH`color, 8}, Infinity], WriteString[strModel,"*d_("<>ToString[color[8]]<>":1,"<>ToString[color[8]]<>":2)"], WriteString[strModel,"       "]]];
                                   WriteString[strModel,";0;     line,  8, 3]\n"];
                                  ];
                            VectorLine[field_,nn_]:=
                            Module[{},
                                   WriteString[strModel,"["<>ToString[field[[1]]]<>","<>ToString[noanti[SARAH`AntiField[field[[1]]]]]<>"; ; VV(lind:1, lind:2, vec,"];
                                   If[MemberQ[field, {SARAH`generation, 3}, Infinity], WriteString[strModel,ToString[generation[3]]<>":1,"<>ToString[generation[3]]<>":2,"],Null];
                                   WriteString[strModel,ToString[40+nn]<>",num)"]
                                   If[MemberQ[field, {SARAH`color, 3}, Infinity], WriteString[strModel,"*d_("<>ToString[color[3]]<>":1,"<>ToString[color[3]]<>":2)"], 
                                      If[MemberQ[field, {SARAH`color, 8}, Infinity], WriteString[strModel,"*d_("<>ToString[color[8]]<>":1,"<>ToString[color[8]]<>":2)"], WriteString[strModel,"       "]]];
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
                            fermionParticles=Cases[SARAH`Particles[SARAH`GaugeES], {_, _, _, SARAH`F, ___}, Infinity];
                            FermionLine[field_, nn_]:=
                            Module[{},
                                   WriteString[strModel,"["<>ToString[field[[1]]]<>","<>ToString[noanti[SARAH`AntiField[field[[1]]]]]<>"; ; FF(fnum,num,vec,"];
                                   If[MemberQ[field, {SARAH`generation, 3}, Infinity], WriteString[strModel,ToString[generation[3]]<>":1,"<>ToString[generation[3]]<>":2,"],Null];
                                   WriteString[strModel,ToString[60+nn]<>")"];
                                   If[MemberQ[field, {SARAH`color, 3}, Infinity], WriteString[strModel,"*d_("<>ToString[color[3]]<>":1,"<>ToString[color[3]]<>":2)"], 
                                      If[MemberQ[field, {SARAH`color, 8}, Infinity], WriteString[strModel,"*d_("<>ToString[color[8]]<>":1,"<>ToString[color[8]]<>":2)"], WriteString[strModel,"       "]]];
                                   WriteString[strModel,";0;    arrowLine,  0,"<>ToString[nn]<>"]\n"];
                                  ];
                            MapIndexed[(FermionLine[#1,First[#2]])&, fermionParticles];
                            WriteString[strModel,"\\End(fermion)\n"];
                           ];
                     PrintGhosts[]:= (* ghost block *)
                     Module[{ghostParticles},
                            WriteString[strModel,"\\Begin(ghost)\n"];
                            ghostParticles=Cases[SARAH`Particles[SARAH`GaugeES], {_, _, _, SARAH`G, ___}, Infinity];
                            GhostLine[field_,nn_]:=
                            Module[{},
                                   WriteString[strModel,"["<>ToString[field[[1]]]<>","<>ToString[noanti[SARAH`AntiField[field[[1]]]]]<>"; ; SS(vec,"]
                                   If[MemberQ[field, {SARAH`generation, 3}, Infinity], WriteString[strModel,ToString[generation[3]]<>":1,"<>ToString[generation[3]]<>":2,"],Null];
                                   WriteString[strModel,ToString[70+nn]<>")"];
                                   If[MemberQ[field, {SARAH`color, 8}, Infinity], WriteString[strModel,"*d_("<>ToString[color[8]]<>":1,"<>ToString[color[8]]<>":2)"], WriteString[strModel,"       "]];
                                   WriteString[strModel,";0;     wavy,  8, 3]\n"];
                                  ];
                            MapIndexed[(GhostLine[#1,First[#2]])&, ghostParticles];

                            WriteString[strModel,"\\End(ghost)\n"];
                           ];

                     (* Propagators output to DIANA model file *)
                     PrintBosons[];
                     PrintFermions[];
                     PrintGhosts[];



                     (* Group of vertices with the same Lorentz structure  *)
                     (* Possible structures are:                           *)
                     (* SSS,SSSS,SSVV,SSV,SVV,FFS,FFV,VVV,VVVV,GGS,GGV,ASS *)
                     PrintVerticesOfType[VS_]:= 
                     Module[{vl},
                            WriteString[strModel,"* "<>ToString[VS[[1]]]<>"\n"];

                            (* list of all vertices with Lorentz structure defined in VS *)
                            vl=SA`VertexList[VS[[1]]];
                            
                            (PrintVertex[#])& /@ vl;
                           ];
                     
                     (* Output for single vertex *)
                     PrintVertex[v_]:=
                     Module[{fields,indices,vertexLegs,hasFermionLegs=True},
                            (* fields = If[Length[v] > 0, (If[Head[#] === Susyno`LieGroups`conj|| Head[#] === SARAH`bar,SARAH`AntiField[SARAH`getParticleName[#]],SARAH`getParticleName[#]])& /@ v[[1]],{}]; *)
                            (* Collecting information needed for output *)
                            (* For each leg we have entry in form: *)
                            (*  {      FIELD,       isVECTOR,       INDEX LIST, isFERMION} *)
                            (*   its name with   |  corresponds to| {idx,type}| True/False *)
                            (*   conj[] and bar[]|  Mom[] in RHS  |           | mark FNUM  *)
                            vertexLegs = GetLegs[v];
                            Print["Get legs: ", vertexLegs];
                            
                            fields = (First[#])& /@ vertexLegs;
                            
                            Print["Fields: ",fields];
                            indices = If[Length[v] > 0, MapIndexed[({getPartIndices[#1],#2[[1]]})& , v[[1]]],{}];
                            
                            (* string of indices for vertex function *)
                            indexSTR[]:=
                            Module[{},  
                                   
                                   ff[bb_]:=({#,bb[[2]]})& /@ bb[[1]];
                                   (* ToExpression[StringReplace[ToString[noanti /@ fields],{"{"->"","}"->"",", "->"xx"}]] @@  *)((ToString[#[[1]]]<>":"<>ToString[#[[2]]])& /@ Flatten[(ff[(#)])& /@ indices,1])
                                   
                                  ];
                            funcSTR[]:= StringReplace[ToString[noanti /@ fields],{"{"->"","}"->"",", "->"xx"}];
                            
                            
                            WriteString[strModel,"["];
                            (* Fields list: VG,VG,VG,VG *)
                            WriteString[strModel,StringReplace[ToString[noanti /@ fields],{"{"->"","}"->""}]];
                            WriteString[strModel,"; ;"];
                            (* function subs name like VGxxVGxxVGxxVG *)
                            WriteString[strModel,funcSTR[]<>"("];
                            (* Fermion line numbering *)
                            If[Count[vertexLegs,{_,_,_,True},Infinity] > 0,WriteString[strModel,"fnum,"]];
                            (* if RHS needs momentums of particles in vertex *)
                            MapIndexed[(If[#1[[2]], WriteString[strModel,"vec:"<>ToString[First[#2]]<>","]])&, vertexLegs];
                            WriteString[strModel,StringReplace[ToString[indexSTR[]],{"["->"(","]"->")","{"->"","}"->""}]<>")]\n"];
                            
                            (* Arguments of FORM subs instruction *)
                            ArgumentsSTR[]:=
                            Module[{},  
                                   ff[bb_]:=({#,bb[[2]]})& /@ bb[[1]];
                                   ((ToString[#[[1]]]<>ToString[#[[2]]]<>"?")& /@ Flatten[(ff[((* Print[FullForm[#]]; *)# /. formIdxSubs)])& /@ indices,1])
                                  ];
                            (* Output subs rule *)
                            SubMom[vvi_]:=
                            Module[{},
                                   vvi[[2]] /. Mom[a_,b_]:> xxpmom[Position[vvi[[1]],a],b]
                                   (* /. Mom[a_,b_]:> p[Position[vvi[[1]],a]] *)                                
                                  ];
                            WriteString[strSubs,"id "<> StringReplace[ToString[noanti /@ fields],{"{"->"","}"->"",", "->"xx"}] <> ToString[ArgumentsSTR[]]<>" = "<>ToString[InputForm[SubMom[v]]]<>"\n"];
                           ];
                     
                     (* Return list with info about all legs ingoing for vertex *)
                     GetLegs[vl_]:= ({GetFieldName[#], MomentumNeeded[#,vl[[2]]], GetIndexList[#], IsFermionLeg[#]})& /@ vl[[1]];
                     
                     (* Extraction procedures *)
                     (* 1: Field wo indices  *)
                     GetFieldName[vf_]:= If[Head[vf] === Susyno`LieGroups`conj|| Head[vf] === SARAH`bar,SARAH`AntiField[SARAH`getParticleName[vf]],SARAH`getParticleName[vf]]; 
                     (* 2: Is this momentum needed for RHS *)
                     MomentumNeeded[vf_,rhs_]:= (Count[rhs,Mom[vf,_],Infinity] > 0);
                     (* 3: Index List *)
                     GetIndexList[vf_]:=If[Length[getPartIndices[vf]] > 0, MapThread[({#1,#2})&, {Flatten[{vf /. GetFieldName[vf]:>List}],getPartIndices[vf]}],{}]; (* getPartIndices[vf]; *)(* MapIndexed[()&,getIndizes[vf]]; *)
                     (* 4: Mark Fermion Legs *)
                     IsFermionLeg[vf_]:= (SARAH`getType[vf] === F);

                     (* Incorrect index range for adjoint representation *)
                     (* lirSubs  = (#[[1]]->#[[1]][#[[2]]])& /@ getIndexRange[p];  *)
                     (* Need to distinguish color,3 = colF and color,8 = colA *)
                     getPartIndices[p_]:=                           
                     SARAH`getIndizes[p] /. ((#[[1,1]]->#[[1,1]][#[[1,2]]])& /@ ((Cases[Cases[SARAH`Particles[SARAH`GaugeES], {SARAH`getParticleName[p], _, _, _, {___, {#, _}, ___}}, Infinity] , {#, _}, Infinity])& /@ SARAH`getIndizes[p]));
                     

                     (* Printing all vertex rules *)
                     WriteString[strModel,"\\Begin(vertex)\n"];
                     PrintVerticesOfType /@ SARAH`ITypes;
                     WriteString[strModel,"\\End(vertex)\n"];
                    ]

              End[]
              
              EndPackage[]



