<<"SARAH.m";

Start["SM"];

MakeVertexList[GaugeES];

(* Get list of vertex with signature VVVV *)
SA`VertexList[VVVV]

(* List of signatures *)
ITypes


MakeDIANA[fname_]:=Module[{str},
                          str = OpenWrite[fname];
                          PrintBosons[]:=Module[{scalarParticles, vectorParticles},
                                               scalarParticles=Cases[Particles[GaugeES], {_, _, _, S, ___}, Infinity];
                                                vectorParticles=Cases[Particles[GaugeES], {_, _, _, V, ___}, Infinity];
                                                ScalarLine[field_]:=Module[{},
                                                                           WriteString[str,"["<>ToString[field[[1]]]<>","<>ToString[conj[field[[1]]]]<>"; ; VV(              vec,51,num)"];
                                                                           If[MemberQ[field, {color, 8}, Infinity], WriteString[str,"*d_(colind:1,colind:2)"], WriteString[str,"       "]];
                                                                           WriteString[str,";0;     line,  8, 3]\n"];
                                                                      ];
                                                VectorLine[field_]:=Module[{},
                                                                          WriteString[str,"["<>ToString[field[[1]]]<>","<>ToString[conj[field[[1]]]]<>"; ; VV(lind:1, lind:2, vec,51,num)"]
                                                                           If[MemberQ[field, {color, 8}, Infinity], WriteString[str,"*d_(colind:1,colind:2)"], WriteString[str,"       "]];
                                                                           WriteString[str,";0;     wavy,  8, 3]\n"];
                                                                      ];
                                                WriteString[str,"\\begin(boson)\n"];
                                               WriteString[str,"* Scalar particles\n"];
                                                
                                                ScalarLine /@ scalarParticles;
                                               WriteString[str,"* Vector particles\n"];
                                                VectorLine /@ vectorParticles;
                                               WriteString[str,"\\end(boson)\n"];
                                               ];
                          PrintFermions[]:=Module[{fermionParticles},
                                               WriteString[str,"\\begin(fermion)\n"];
                                                 fermionParticles=Cases[Particles[GaugeES], {_, _, _, F, ___}, Infinity];
                                               WriteString[str,"\\end(fermion)\n"];
];
                          PrintGhosts[]:=Module[{ghostParticles},
                                               WriteString[str,"\\begin(ghost)\n"];
                                               ghostParticles=Cases[Particles[GaugeES], {_, _, _, G, ___}, Infinity];
                                               WriteString[str,"\\end(ghost)\n"];
];
                          PrintBosons[];
                          PrintFermions[];
                          PrintGhosts[];

                          ];
MakeDIANA["testFILE"];
