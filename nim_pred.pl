% -----------------------------------------------------------------------------
%                               program NIM 
%                             Martin Dvorak I5
%                   Prohledavanim stromu reseni odspodu ...
% -----------------------------------------------------------------------------
% vymen(+Sez,-Vymen_sez)
vymen([X,Y|R],[Y,X|R]):-X<Y.
vymen([Z|R],[Z|R1]):-vymen(R,R1).
%----------------------------------------------------------------------------%
% buble(+Sez,-Utrideny)
buble(L,S):-vymen(L,L1),!,buble(L1,S).
buble(S,S).
%----------------------------------------------------------------------------%
% min(+A,+B,-V)
min(A,B,V):-A=<B,V is A.
min(A,B,V):-A>B,V is B.
% -----------------------------------------------------------------------------
% uber(+Seznam,+Kde,+Kolik,-Uspech,-Vysl,Pozice) , uspech 1 , neuspech 0
uber([H|T],C,P,1,[OH|T],InC):-InC = C,H >= P,         % uspesne da 1
                              OH is H - P. 
uber([H|_],C,P,0,_,InC ):-InC = C,H<P.                 % neuspesne da 0
uber([H|T],C,P,U,[H|OT],InC):-InC < C,MInC is InC+1,  % posun na dalsi
                              uber(T,C,P,U,OT,MInC).
%----------------------------------------------------------------------------%
% udela seznam vsech moznych tahu : moznosti(+Stav,+K,+P,+R,-Moznosti)
moznosti(Sez,K,P,R,Vysl):-MP is P-1,moznosti(Sez,1,K,MP,P,R,[],MVysl),
                          sort(MVysl,Vysl).
moznosti(_,MK,K,MR,_,R,V,V):-MR=R,MK=K.
moznosti(Sez,MK,K,MR,P,R,MV,V):-MR=R,MK<K,
                                NK is MK+1,NR is P,           % dalsi hrom
                                uber(Sez,NK,NR,OK,OSez,1),
                            ((OK=0,!,moznosti(Sez,NK,K,NR,P,R,MV,V));
                             (OK=1,!,buble(OSez,OOSez),
                              moznosti(Sez,NK,K,NR,P,R,[OOSez|MV],V))).
moznosti(Sez,MK,K,MR,P,R,MV,V):-MR<R,MK=<K,
                                NR is MR+1,                   % dalsi sirka
                                uber(Sez,MK,NR,OK,OSez,1),
                            ((OK=0,!,moznosti(Sez,MK,K,NR,P,R,MV,V));
                             (OK=1,!,buble(OSez,OOSez),
                              moznosti(Sez,MK,K,NR,P,R,[OOSez|MV],V))).
%----------------------------------------------------------------------------%
% pridej(+Vzor,+Stav,+Kam,+Kolik,-Uspech,-Vysl,Pos)
pridej([HZ|_],[H|T],C,P,1,[OH|T],InC):-InC = C,OH is H + P,OH=<HZ.
pridej([HZ|_],[H|T],C,P,0,[OH|T],InC):-InC = C,OH is H + P,OH>HZ.
pridej([_|TZ],[H|T],C,P,U,[H|OT],InC):-InC<C,MInC is InC+1,
                                        pridej(TZ,T,C,P,U,OT,MInC).
%----------------------------------------------------------------------------%
% pridava , generuje vsechny predchudce stavu Seznam_z_ceho mensi nez Stav
% gen(+Stav,+Seznam_z_ceho,+K,+P,+R,-Nove_stavy)
gen(_,[],_,_,_,[]).
gen(Stav,[H|T],K,P,R,Out):-gen(Stav,T,K,P,R,Gen),MP is P-1,
                           gen(Stav,H,1,K,MP,P,R,[],MVysl),
                           sort(MVysl,Vysl),append(Vysl,Gen,Out). 
gen(_,_,MK,K,MR,_,R,V,V):-MR=R,MK=K.
gen(Stav,Sez,MK,K,MR,P,R,MV,V):-MR=R,MK<K,
                                NK is MK+1,NR is P,              % dalsi hrom
                                pridej(Stav,Sez,NK,NR,OK,OSez,1),
                                ((OK=0,!,gen(Stav,Sez,NK,K,NR,P,R,MV,V));
                                 (OK=1,!,buble(OSez,OOSez),
                                  gen(Stav,Sez,NK,K,NR,P,R,[OOSez|MV],V))).
gen(Stav,Sez,MK,K,MR,P,R,MV,V):-MR<R,NR is MR+1,                % dalsi sirka
                                pridej(Stav,Sez,MK,NR,OK,OSez,1),
                                ((OK=0,!,gen(Stav,Sez,MK,K,NR,P,R,MV,V));
                                 (OK=1,!,buble(OSez,OOSez),
                                  gen(Stav,Sez,MK,K,NR,P,R,[OOSez|MV],V))).
%----------------------------------------------------------------------------%
% prvek(+Kde,+Co,-Priznak)
prvek([],_,0).
prvek([C|_],C,1).
prvek([_|T],C,P):-prvek(T,C,P).
%----------------------------------------------------------------------------%
% zkontroluje ,je-li nektera z mych moznosti jak tahnout ve stavech , pokud
% ano , vrati ji a to je muj optimalni tah : check(+Tahy,+Moznosti,-Opt_Tah)
check([],_,[]).
check([H|T],Stavy,V):-prvek(Stavy,H,C),
                       ((C=0,!,check(T,Stavy,V));
                        (C=1,!,V=H)).
%----------------------------------------------------------------------------%
% cykldvatahy(+Mozn,+Horni_hranice,+Z_ceho_gen,+K,+P,+R,-Opt_Tah)
cykldvatahy(Mohu,Konec,H,K,P,R,OptTah):-
                dvatahy(Konec,H,K,P,R,MV),
                ((MV=[],!,OptTah=[]);                 % neex.dalsi opt. tah
                 (MV\=[],!,check(MV,Mohu,C),          % ex. ale mohu ?
                           ((C=[],!,cykldvatahy(Mohu,Konec,MV,K,P,R,OptTah));
                            (C\=[],!,OptTah = C)))).  % nalezen opt. tahu
%----------------------------------------------------------------------------%
% udela dvoutah ( z A do D dle dokumentace )
% dvatahy(+Horni_hranice,+Mnoz_A,+K,+P,+R,-Mnoz_D
dvatahy(Konec,Stav,K,P,R,Vysl):-gen(Konec,Stav,K,P,R,MV),
                                cykl(Konec,MV,MV,K,P,R,NVysl),
                                sort(NVysl,Vysl).
%----------------------------------------------------------------------------%
% vyrobi vsechny tahy z nichz lze tahnout do seznamu stavu [H,T]
% dostane moje tahy vedouci na vitezstvi MTVNV a vrati mi tahy z kterych
% souper nemuze hrat jinam nez do MTVNV
% cykl(+Horni_hranice,+MTVNV,+MTVNV,+K,+P,+R,-Vysledek)
cykl(_,_,[],_,_,_,[]).
cykl(Konec,Cil,[H|T],K,P,R,V):-cykl(Konec,Cil,T,K,P,R,OK2),
                               gen(Konec,[H],K,P,R,MV),
                               sel(MV,Cil,K,P,R,OK1),
                               append(OK1,OK2,V).
%----------------------------------------------------------------------------%
% kontroluje jestli vsechny [H|T] padli do seznamu Stavy ( zda mnozina
% stavu Moznosti je podmozina mnoziny stavu Cile
% check(+Moznosti,+Cil,-Priznak)    uspech : 1
check1([],_,1).
check1([H|T],Stavy,OK):-prvek(Stavy,H,C),
                     ((C=1,!,check1(T,Stavy,OK));
                      (C=0,!,OK = 0)).
%----------------------------------------------------------------------------%
% sel(+Seznam_tahu,+Kam_musi_padnout_vsechny_z_nich,+K,+P,+R,-OK_stavy)
sel([],_,_,_,_,[]).
sel([H|T],Cil,K,P,R,V):-moznosti(H,K,P,R,Mozn),
                        check1(Mozn,Cil,C),
                        ((C=1,!,sel(T,Cil,K,P,R,MV),V = [H|MV]);
                         (C=0,!,sel(T,Cil,K,P,R,V))).
%----------------------------------------------------------------------------%
% vyrobi seznam cisel Cis delky Kolik
% sez_cis(+Delka,+Cislo,Pos,-Vysledek)
sez_cis(C,_,C,[]).
sez_cis(Kolik,Cis,C,[Cis|T]):-C<Kolik,CN is C+1,sez_cis(Kolik,Cis,CN,T).
%----------------------------------------------------------------------------%
% vyrobi vsechny prohravajici pozice
% zname_pos(+K,+P,-Prohravajici)
zname_pos(K,P,Proh):-MP is P-1,sez_cis(K,MP,0,Max),sez_cis(K,0,0,Nul),
                     zname(Max,[Nul],K,K,1,MP,Proh).
%----------------------------------------------------------------------------%
% zname(+Horni_hranice,+Sez_znamych,+Pos,+K,+P,+R,-Vysledek)
zname(_,V,_,_,_,0,V).
zname(_,_,0,_,_,_,[]).
zname(Max,Sez,C,K,P,R,Out):-C\=0,gen(Max,Sez,K,P,R,MV),NC is C-1,
                            zname(Max,MV,NC,K,P,R,V),
                            append(MV,V,NOut),sort(NOut,Out).
%----------------------------------------------------------------------------%
% vraci optimalni tah ze stavu Stav ( pokud existuje ), jinak []
% tah(+Stav,+K,+P,+R,-Opt_tah)
tah(Sez,K,P,R,Opt):-moznosti(Sez,K,P,R,Moz),sez_cis(K,0,0,Nul),
                    zname_pos(K,P,Proh),buble(Sez,USez),
                    cykldvatahy(Moz,USez,[Nul|Proh],K,P,R,Opt). % opt.tah
                          



