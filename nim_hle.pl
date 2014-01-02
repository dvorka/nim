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
% vraci optimalni tah ze stavu Stav
% tah(+Stav,+K,+P,+R,-Opt_tah,-Priznak_konce)
tah(Sez,K,P,R,OptTah,OK):-moznosti(Sez,K,P,R,Moz),sez_cis(K,0,0,Nul),
                          zname_pos(K,P,Proh),buble(Sez,USez),
                          cykldvatahy(Moz,USez,[Nul|Proh],K,P,R,Opt),
                          ((Opt=[],[A|_]=Moz,OptTah=A);
                           (Opt\=[],nl,
                            write(' Nalezena vyhravajici pozice!'),
                            OptTah=Opt)),
                          nl,write(' MUJ TAH:  '),write(OptTah),
                          konec(OptTah,P,R,ja,OK).     % neni tah - vyhravam
%----------------------------------------------------------------------------%
% ohodnot(+Stav,+P,+R,-Vyhodnost) ,1/-1,0 znamena ze nejde ohodnotit.
ohodn([],_,_,0,Hodn):-Hodn is -1.                % soucet 0 - prohravam
ohodn([],_,_,MH,Hodn):-MH\=0,Hodn is 0.    % soucet vetsi - nelze
ohodn([H|_],_,R,_,Hodn):-H > R,Hodn is 0.        % mimo obor - nelze
ohodn([H|T],P,R,MH,Hodn):-H < P,H < R,           % nemohou brat => posun
                          ohodn(T,P,R,MH,Hodn).
ohodn([H|T],P,R,MH,Hodn):-H >= P,H =< R,           % mohu dobrat hromadku
                          MOH is MH + 1,           % pridam do souctu
                          ohodn(T,P,R,MOH,Hodn).
%----------------------------------------------------------------------------%
% test konce hry,chyby : konec(+Stav,+P,+R,+Odkud_volam,-Priznak)
konec(Tah,P,R,ja,0):-ohodn(Tah,P,R,0,Hodn),Hodn = -1,
                     nl,write(' Vyhral jsem ! '),!.
konec(Tah,P,R,on,0):-ohodn(Tah,P,R,0,Hodn),Hodn = -1,
                     nl,write(' Gratuluji , vyhral jste !'),!.
konec(Tah,P,R,test,0):-ohodn(Tah,P,R,0,Hodn),Hodn = -1,
                     nl,write(' Nesmyslne zadani !'),!.
konec(_,_,_,_,1).
%----------------------------------------------------------------------------%
% kdo vyhraje pri predem rozhodnute hre : kdo(+Stav,+P,C)
kdo([],_,C):-((liche(C),nl,write(' Vyhral by jste !'));
              (sude(C),nl,write(' Prohral by jsem !'))).
kdo([H|T],P,C):-CO is (C+(H//P)),kdo(T,P,CO).
%----------------------------------------------------------------------------
% test zda je hra predem rozhodnuta: smysl(+Stav,+P,+R,-Priznak)
smysl(Sez,P,R,OK):-((P=R,nl,write(' Toto je predem rozhodnuta hra .'),
                     kdo(Sez,P,0),OK = 0);
                    (P\=R,OK = 1)).
%----------------------------------------------------------------------------
% testuje koreknost zadani : test_zac(+Stav,+P,+R)
test_zac(Sez,P,R):-((lst(Sez),length(Sez,K),K>0,P=<R,integer(P),integer(R));
                    (nl,write(' Nesmyslne zadani! Vlozte prosim nove.'),fail)).
%----------------------------------------------------------------------------
% vstup zadani : zacatek(-Stav,-K,-P,-R,-Priznak)
zacatek(Sez,K,P,R,OK):- repeat,
                        nl,write(' Vlozte pocatecni stav: '),read(Sez),
                        write(' Odebrat minimalne: '),read(P),
                        write(' Odebrat maximalne: '),read(R),
                        test_zac(Sez,P,R),length(Sez,K),
                        konec(Sez,P,R,test,OK),!.
%----------------------------------------------------------------------------%
% test korektnosti pri odebirani: test_vstup(+K,+P,+R,+Hrom,+Sir)
test_vstup(K,P,R,Hrom,Sir):-((integer(Hrom),integer(Sir),Hrom=<K,Sir>=P,
                            Sir=<R);
                           (nl,write(' Nesmyslne zadani! Vlozte prosim nove.'),
                            fail)).
%----------------------------------------------------------------------------
% tah soupere : vstup(+Stav,+K,+P,+R,-Jeho_tah,-Priznak)
vstup(Sez,K,P,R,OSez,OOK):- repeat,
                        nl,write('   Vlozte cislo hromadky :'),read(Hrom),
                        write('   Pocet odebiranych sirek :'),read(Sirky),
                        test_vstup(K,P,R,Hrom,Sirky),
                        uber(Sez,Hrom,Sirky,OK,OSez,1),nonvar(OK),OK=1,
                        !,
                        write(' VAS TAH:  '),write(OSez),
                        konec(OSez,P,R,on,OOK).
%----------------------------------------------------------------------------%
% pokud neuspeje OK = 1 hra konci.
%----------------------------------------------------------------------------
% tah_hrace(+Stav,+K,+P,+R)
tah_hrace(Sez,K,P,R):-vstup(Sez,K,P,R,OSez,OK),nonvar(OK),OK = 1,
                      tah_programu(OSez,K,P,R).
%----------------------------------------------------------------------------%
% tah_programu(+Stav,+K,+P,+R)
tah_programu(Sez,K,P,R):-tah(Sez,K,P,R,OSez,OK),nonvar(OK),OK = 1,
                         tah_hrace(OSez,K,P,R).
%----------------------------------------------------------------------------%
% spusteni hry
nim:-nl,nl,nl,nl,nl,tab(32),write('NIM'),nl,
     tab(19),write(' Prohrava hrac , ktery nema tah .'),nl,
     zacatek(Sez,K,P,R,OK1),smysl(Sez,P,R,OK2),    % korektnost a smysl zadani
     min(OK1,OK2,OK),nonvar(OK),OK=1,
     tah_hrace(Sez,K,P,R).
%--------------------------- Konec programu ---------------------------------%





