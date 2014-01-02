%----------------------------------------------------------------------------
%                               program NIM 
%                             Martin Dvorak I5
%                    Vyhledavanim strategickych pozic ...
%----------------------------------------------------------------------------
% sude(+Cislo),liche(+Cislo)
sude(A):-V is (2*( A // 2)),A=V.
liche(A):-not(sude(A)).
%----------------------------------------------------------------------------
% min(+Cislo1,+Cislo2,-Minimum)
min(A,B,V):-A=<B,V is A.
min(A,B,V):-A>B,V is B.
%----------------------------------------------------------------------------
% strategy(+Stav,Pocet_k*(min+max)+min+s,+P,+R,-Je_vyhr)
strategy([],C,_,_,V):-((sude(C),V=1);
                      (liche(C),V=0)).
strategy([H|T],C2,P,R,V):-H<P,strategy(T,C2,P,R,V).

strategy([H|T],C2,P,R,V):-H>=P,
                          C is (H mod (P+R)),C =< (P-1),
                          strategy(T,C2,P,R,V).
strategy([H|T],C2,P,R,V):-H>=P,C is (H mod (P+R)),
                          C =< (P+P-1),C >= P,
                          C2O is C2+1,
                          strategy(T,C2O,P,R,V).
strategy([H|_],_,P,R,0):-H>=P,C is (H mod (P+R)),
                         C > (P+P-1).
%----------------------------------------------------------------------------
% uber(+Stav,+Kde,+Kolik,-Uspech,-Vysl,Pozice) , uspech 1 , neuspech 0
uber([H|T],C,P,1,[OH|T],InC):-InC = C,H >= P,          % uspesne da 1
                              OH is H - P. 
uber([H|_],C,P,0,_,InC ):-InC = C,H<P.                 % neuspesne da 0
uber([H|T],C,P,U,[H|OT],InC):-InC < C,MInC is InC+1,   % posun na dalsi
                              uber(T,C,P,U,OT,MInC).
%----------------------------------------------------------------------------%
% udela seznam vsech moznych tahu
% moznosti(+Stav,+K,+P,+R,-Moznosti)
moznosti(Sez,K,SP,R,Vysl):-P is SP-1,                         % SP - pravne P
                           moznosti(Sez,1,K,P,SP,R,[],Vysl).
moznosti(_,MK,K,MR,_,R,V,V):-MR=R,MK=K.
moznosti(Sez,MK,K,MR,P,R,MV,V):-MR=R,MK<K,
                                NK is MK+1,NR is P,           % dalsi hrom
                                uber(Sez,NK,NR,OK,OSez,1),
                                ((OK=0,!,moznosti(Sez,NK,K,NR,P,R,MV,V));
                                 (OK=1,!,
                                  moznosti(Sez,NK,K,NR,P,R,[OSez|MV],V))).
moznosti(Sez,MK,K,MR,P,R,MV,V):-MR<R,MK=<K,
                                NR is MR+1,                   % dalsi sirka
                                uber(Sez,MK,NR,OK,OSez,1),
                                ((OK=0,!,moznosti(Sez,MK,K,NR,P,R,MV,V));
                                 (OK=1,!,
                                  moznosti(Sez,MK,K,NR,P,R,[OSez|MV],V))).
%----------------------------------------------------------------------------%
% zkontroluje zda nektera z mych moznosti jak tahnout vede dle strategie 
% na vyhru ve hre.
% check(+Moznosti,+P,+R,-Opt.Pos)
check([],_,_,[]).
check([H|T],P,R,V):-strategy(H,0,P,R,C),
                    ((C=0,!,check(T,P,R,V));
                     (C=1,!,V=H)).
%----------------------------------------------------------------------------%
% vybere ze seznamu stav , z ktereho nejde hrat na vyhravajici pozici
% vyber(+Stavy,K,P,R,-Tah)
vyber([],_,_,_,[]).
vyber([H|T],K,P,R,V):-moznosti(H,K,P,R,Moz),
                      check(Moz,P,R,Out),
                      ((Out=[],V=H);          % z H nejde hrat na strategii
                       (Out\=[],vyber(T,K,P,R,V))).
%----------------------------------------------------------------------------%
% byla nalezena vyhr. pozice , vzda uzivatel ?
% vzdej(-OK)
vzdej(OK):-nl,write(' Nalezl jsem pozici vedouci na vitezstvi!'),
           nl,write(' Chcete pokracovat ve hre (a/n) :'),read(Volba),
           ((Volba=a,OK=1);(Volba\=a,OK=0,write(' Vyhral jsem !'))).
%----------------------------------------------------------------------------
% nachazi optimalni tah ze stavu stav
% tah(+Stav,+K,+P,+R,-Opt,-Konec)
tah(Sez,K,P,R,OptTah,OK):-moznosti(Sez,K,P,R,Moz),
                          check(Moz,P,R,Opt),       % muzu tahnout na o.p. ?
                          ((Opt=[],OK1 is 1,vyber(Moz,K,P,R,V), % nemohu
                                   ((V=[],[A|_]=Moz,OptTah=A);
                                    (V\=[],OptTah=V))
                           );
                           (Opt\=[],vzdej(OK1),OptTah=Opt)),    % mohu
                          nl,write(' MUJ TAH:  '),write(OptTah),
                          konec(OptTah,P,R,ja,OK2), % neni tah , vyhravam
                          min(OK1,OK2,OK).          % priznak konce hry
%----------------------------------------------------------------------------%
% ohodnot(+Stav,+P,+R,-Vyhodnost) ,1/-1,0 znamena ze nejde ohodnotit.
ohodn([],_,_,0,Hodn):-Hodn is -1.                % soucet 0 - prohravam
ohodn([],_,_,1,Hodn):-Hodn is 1.                 % soucet 1 - vyhravam
ohodn([],_,_,MH,Hodn):-MH\=0,MH\=1,Hodn is 0.    % soucet vetsi - nelze
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
     zacatek(Sez,K,P,R,OK1),smysl(Sez,P,R,OK2),         % kor. a smysl zadani
     min(OK1,OK2,OK),nonvar(OK),OK=1,
     tah_hrace(Sez,K,P,R).
%--------------------------- Konec programu ---------------------------------%
