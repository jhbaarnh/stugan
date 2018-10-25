SOURCE
PRECISION= 7
AUTODEF=ON
OPTION BASE=0
ERL=OFF
ERRORMODE=LOCAL
RESUME=LINE
FORMODE=BB
PRINTMODE=BB
SCOPE=ON
PROCS=22
INTEGER ARRAY(60): Flagga
BYTE ARRAY(35): Pryl 'inventory?
BYTE ARRAY(101): Jack
INTEGER: A,A1,A1%,A2%,B,D,E,E1
INTEGER: G,I,I%,I1,J,M2%,M3%,S
INTEGER: X,X%,X1,X2,Z,Poang,Maxpoang
STRING: A$[80],A1$[80]
STRING: C$[80],R$[25],M2$[80],M3$[80],W$[80]
STRING ARRAY(35,4)[20]: SAK$ 'alla saker i spelet?
STRING: Namn$[80]
BYTE ARRAY(35): Genus 'genus for alla ord?
BYTE ARRAY(16): Varde
BYTE: Version
REAL: F2
BYTE: Old

STRING FUNCTION: STORSV$
STRING ARG: A$
END FUNCTION

STRING FUNCTION: LASIN$
STRING ARG: Prompt$
END FUNCTION

STRING FUNCTION: INGEN$
BYTE ARG: I1
END FUNCTION

PROCEDURE: SOV
STRING ARG: Meddel$
INTEGER ARG: Tid
END PROCEDURE

PROCEDURE: Vanta
STRING ARG: Meddel$
INTEGER ARG: Tid
END PROCEDURE

STRING FUNCTION: Insov$
INTEGER ARG: Tid
BYTE ARG: Tflagg
END FUNCTION

INTEGER FUNCTION: Testasak
STRING ARG: S$
END FUNCTION

INTEGER FUNCTION: Kommando
BYTE ARG: Riktning
END FUNCTION

PROCEDURE: Thorvald
BYTE ARG: Stora
END PROCEDURE

PROCEDURE: Felmeddelande
END PROCEDURE

PROCEDURE: Underbyggnad
END PROCEDURE

PROCEDURE: Rumsbeskrivning
END PROCEDURE

PROCEDURE: Telefon
END PROCEDURE

INTERRUPT PROCEDURE: Itelefon
END PROCEDURE

PROCEDURE: Telefonsignal
END PROCEDURE

INTERRUPT PROCEDURE: Poangsiffra
END PROCEDURE

PROCEDURE: Faunsang
END PROCEDURE

PROCEDURE: Piratsang
END PROCEDURE

PROCEDURE: Skrivrad
INTEGER ARG: Avkoda,Borja,Sluta
END PROCEDURE

STRING FUNCTION: Kryptera
STRING ARG: Rad$
END FUNCTION

INTEGER FUNCTION: Jasvar
STRING ARG: Ledtext$
END FUNCTION

INTEGER FUNCTION: Lika
INTEGER ARG: Inne
STRING ARG: A$,Ord1$,Ord2$
END FUNCTION

'konvertera en strang till versaler
STRING FUNCTION: STORSV$
BYTE: X1,X2
STRING: X1$[80]
900 REM Översätt till stora bokstäver
902 IF A$="" THEN X1$="" : GOTO 965
905 X1$=SPACE$(LEN(A$))
910 FOR X2=LEN(A$) TO 1 STEP -1
915   X1=ASC(MID$(A$,X2,1))
920   IF X1<97 THEN GOTO 925
921   IF X1<123 THEN X1=X1-32
922   IF X1=132 THEN X1=142'##### IBM-bokstaven ä
923   IF X1=148 THEN X1=153'##### IBM-bokstaven ö
924   IF X1=134 THEN X1=143'##### IBM-bokstaven å
925   MID$(X1$,X2,1)=CHR$(X1)'#####
930 NEXT X2
965 RESULT=X1$
END FUNCTION

'skriv ut prompt och las in strang fran tangentbordet, vanstertrimma
STRING FUNCTION: LASIN$
EXTERNAL: M2%,W$,M3%,Storsv$,Insov$
1000 REM ************ Inläsning ***************
1010 IF M2%=1 AND W$<>CHR$(3) THEN PRINT #2,W$'&&&&&
1015 SET CURSOR ,,0
1020 PRINT Prompt$;
1030 IF M3%=0 THEN W$=Insov$(3600,2) : GOTO 1060
1040 IF EOF(3) THEN M3%=0 : GOTO 1030'&&&&&
1050 LINE INPUT #3,W$ : PRINT W$'&&&&&
1060 IF LEFT$(W$,1)=" " THEN W$=MID$(W$,2) : GOTO 1060
1070 RESULT=Storsv$(W$)
END FUNCTION

'skriver ut "ingen" med korrekt genus
STRING FUNCTION: INGEN$
EXTERNAL: Genus()
10 IF Genus(I1)=0 THEN RESULT="ingen " ELSE IF Genus(I1)=1 THEN RESULT="inget " ELSE RESULT="inga "
END FUNCTION

'vantar bestamd tid, skriver ut meddelande om man trycker tangenter under tiden och startar da om timern
PROCEDURE: SOV
REAL: Nu
STRING: Tryck$[2]
10 NU=TIMER
20 Tryck$=INKEY$
25 IF TIMER>NU+Tid THEN GOTO 60
30 IF Tryck$="" THEN GOTO 20
40 PRINT "Tyst, jag ";Meddel$;"!" : BEEP
50 GOTO 10
60 REM Färdigt Olles, Viggos och Kimmos sovrutin
END PROCEDURE

'vantar bestamd tid, skriver ut meddelande om man trycker tangenter under tiden
PROCEDURE: Vanta
REAL: Nu
STRING: Tryck$[2]
10 NU=TIMER
20 Tryck$=INKEY$
25 IF TIMER>NU+Tid THEN GOTO 60
30 IF Tryck$="" THEN GOTO 20
40 PRINT Meddel$ : BEEP
50 GOTO 20
60 REM Färdigt Olles, Viggos och Kimmos väntarutin
END PROCEDURE

'laser text fran tangentbordet under bestamd tid
'om tflagg=1 lases maximalt 1 tecken
'tflagg>1 => fler tecken kan lasas
'om tflagg=2 visas speciell prompt och inga tecken visas på skarmen
'nyradstecken lases ej in
STRING FUNCTION: Insov$
EXTERNAL: Storsv$
REAL: Nu
STRING: Tryck$[2],Rad$[80]
BYTE: Teckenpos
10 NU=TIMER
15 Rad$=""
20 IF Tflagg=2 THEN COLOR 22,0,0 : PRINT "_"; : COLOR 7,0,0
21 Tryck$=INKEY$
25 IF TIMER>NU+Tid THEN PRINT CHR$(8);" "; : Rad$="" : GOTO 60
28 IF Tryck$="" THEN GOTO 21
30 IF Tflagg=2 THEN PRINT CHR$(8);" ";CHR$(8);
31 IF Tryck$=CHR$(8) AND Rad$="" THEN GOTO 20
32 IF Tryck$<>CHR$(8) THEN GOTO 35
33 PRINT CHR$(8);" ";CHR$(8); : Rad$=LEFT$(Rad$,LEN(Rad$)-1) : GOTO 20
35 IF Tryck$=CHR$(13) OR Tryck$=CHR$(10) THEN PRINT : GOTO 50
40 IF LEN(Tryck$)=1 AND ASC(Tryck$)>31 THEN PRINT Tryck$;
45 Rad$=Rad$+Tryck$
47 IF Tflagg<>1 THEN GOTO 20
50 Rad$=Storsv$(Rad$)
60 RESULT=Rad$
END FUNCTION

'testar om en given strang motsvarar en sak i inventory
'alternativt kollar om bestamd artikel pa sak med nummer Flagga(2) ar schysst 
INTEGER FUNCTION: Testasak
EXTERNAL: Sak$(),Flagga(),Pryl(),I,Genus(),Lika
10 I=Flagga(2)
20 IF Lika(1,S$,"DEN","") THEN GOTO 100
30 IF Lika(1,S$,"DET","") THEN GOTO 120
35 IF Lika(1,S$,"DEM","DOM") THEN GOTO 200
40 FOR I=1 TO Pryl(0)
50   IF Sak$(I,1)<>"" THEN IF INSTR(1,S$,Sak$(I,2))>0 OR INSTR(1,S$,Sak$(I,3))>0 THEN EXIT
60 NEXT I
70 IF I>Pryl(0) THEN I=0
80 GOTO 200
100 IF I>0 THEN IF Genus(I)=1 THEN PRINT Sak$(I,1);" är faktiskt neutrum!"
110 GOTO 200
120 IF I>0 THEN IF Genus(I)=0 THEN PRINT Sak$(I,1);" är faktiskt reale!"
200 IF I>0 THEN Flagga(2)=I
210 RESULT=I
END FUNCTION

INTEGER FUNCTION: Kommando
EXTERNAL: Flagga(),Pryl(),Jack(),Poang,Maxpoang,SAK$(),A,A1
EXTERNAL: A1%,A2%,B,D,E,E1,G,I
EXTERNAL: I%,I1,J,M2%,M3%,S,X,X%
EXTERNAL: X1,X2,Z,A$,A1$,C$,R$,M2$
EXTERNAL: M3$,W$,STORSV$,LASIN$,INGEN$,Sov,Vanta,Insov$
EXTERNAL: Felmeddelande,Testasak,Skrivrad
CONSTANT: PI=2.67051!
BYTE: Objekt
REAL: KOD,KOD2
EXTERNAL: Namn$,Varde(),Genus(),Version,Thorvald
EXTERNAL: Lika,Telefon,Itelefon,Jasvar
1 X=0 : X1=0
10 IF A$="" THEN GOTO 12210
20 IF Riktning=1 THEN GOTO 12000 ELSE GOTO 8600
6300 IF Objekt>0 THEN GOTO 6399 ELSE IF C$<>"" THEN GOTO 6305'Ta
6301 IF Z=37 AND Flagga(38)=0 THEN X=13 : GOTO 12999
6302 C$=LASIN$("Ta vadå? ") : A$=C$
6305 Objekt=Testasak(C$)
6308 IF Objekt>0 THEN GOTO 6399
6309 IF Lika(0,C$,"ALLTING","4") THEN GOTO 6500
6310 IF Lika(1,C$,"VATTEN","VATTNET") THEN GOTO 6330
6311 IF Lika(1,C$,"GRAVSTENEN","") AND Z=61 THEN PRINT "Gravstenen väger alldeles för mycket." : GOTO 12210
6312 IF Lika(1,C$,"KISTAN","") AND Z=15 THEN PRINT "Kistan väger 300 kilogram!" : GOTO 12210
6313 IF (Lika(1,C$,"FAMILJEVAPEN","FAMILJEVAPNET") OR Lika(1,C$,"VAPEN","VAPNET")) AND Z=81 THEN PRINT "Vapnet sitter för hårt fast." : GOTO 12210
6314 IF Lika(1,C$,"TAVLAN","") AND Flagga(36)=0 THEN GOTO 6360
6315 IF Lika(1,C$,"SAFTFLASKAN","") THEN PRINT "Jag ser ingen SAFTFLASKA här." : GOTO 12210
6316 IF Lika(1,C$,"FLASKAN","") THEN GOTO 6370
6317 IF Lika(1,C$,"PORTEN","4") AND (Z=81 OR Z=62) THEN PRINT "Porten sitter fast i väggen." : GOTO 12210
6318 IF Lika(1,C$,"JACKEN","4") AND Jack(Z)=1 THEN PRINT "Jacken sitter fastskruvad i väggen!" : GOTO 12210
6319 IF Lika(1,C$,"BRUNNEN","") AND Z=99 THEN PRINT "Brunnen är gjuten i marken!" : GOTO 12210
6320 IF Lika(1,C$,"KASSASKÅPET","") AND (Z=30 OR Z=31) THEN PRINT "Det är fastgjutet i berget." : GOTO 12210
6321 IF Lika(1,C$,"BÅTEN","3") AND (Z=49 OR Z=78 OR Z=50) THEN GOTO 6380
6322 IF Lika(1,C$,"GUBBEN","") AND Flagga(30)=Z THEN GOTO 30002
6323 IF Lika(1,C$,"LÅDAN","4") AND Pryl(28)=2 AND Z=56 THEN PRINT "Lådan är för tung!" : GOTO 12210
6324 IF Lika(1,C$,"RUBINEN","") AND A1=1 AND Z=70 THEN GOTO 6355
6325 IF Lika(1,C$,"ASKEN","3") AND Pryl(27)=2 AND Z=57 THEN PRINT "Asken sitter fast i väggen." : GOTO 12210
6326 IF Lika(1,C$,"FÖNSTER","FÖNSTRET") AND Z=16 AND Pryl(31)=0 THEN PRINT "Fönstret är fastkittat i väggen." : GOTO 12210
6327 IF Z=37 AND Flagga(38)=0 THEN X=13 : GOTO 12999
6328 IF Lika(1,C$,"RIBBSTOLEN","") AND Z=13 THEN PRINT "Ribbstolen är fastsvetsad i väggen." : GOTO 12210
6329 PRINT "Jag ser ingen ";C$;" här." : GOTO 12210
6330 IF Z=25 OR Z=33 OR Z=49 OR Z=50 OR Z=66 OR Z=70 OR Z=91 THEN GOTO 6338
6332 IF Z=72 OR Z=74 OR Z=78 OR Z=79 OR Z=83 OR Z=87 OR Z=88 THEN GOTO 6338
6334 PRINT "Jag ser inget VATTEN här."
6336 GOTO 12210
6338 IF Pryl(19)<>1 THEN PRINT "Du har inget att ta vattnet i." : GOTO 12210
6340 IF Flagga(32)=0 THEN PRINT "Din vattenflaska är redan full." : GOTO 12210
6342 PRINT "Du fyller på vattenflaskan med vatten från ";
6344 IF Z=91 THEN PRINT "vattenfallet." ELSE PRINT "sjön."
6346 Flagga(32)=0 : GOTO 12210
6355 Skrivrad 1, 2,3 : A1=0 : GOTO 12210
6360 PRINT "Du kan väl inte sno en av husets tavlor!"
6362 Poang=Poang-1 : GOTO 12210
6370 PRINT "Du måste skriva vilket slags flaska du menar, t ex TA SAFTFLASKA."
6372 GOTO 12210
6380 PRINT "Du orkar inte bära roddbåten."
6382 GOTO 12210
6399 IF Pryl(Objekt)=1 THEN GOTO 6412
6400 IF Flagga(1)>=9 THEN PRINT "Du kan inte bära fler saker." : GOTO 12210
6401 IF Pryl(Objekt)=2 AND (Pryl(29)=Z OR Pryl(29)=1) THEN GOTO 28130
6402 IF (Objekt=10 OR Objekt=11 OR Objekt=19) AND Flagga(30)=Z AND Pryl(Objekt)=0 THEN GOTO 30020
6404 IF Objekt=22 AND Z=63 THEN GOTO 28160
6406 IF Objekt=29 AND Flagga(6)<>1 THEN PRINT "Det kan du inte." : GOTO 12210
6408 IF Objekt=14 AND Pryl(14)=0 AND (Pryl(32)=1 OR Pryl(32)=Z) THEN GOTO 6480
6410 IF Pryl(Objekt)=Z OR (Objekt=30 AND Flagga(44)=Z) THEN GOTO 6420
6412 IF Pryl(Objekt)=1 THEN PRINT "Du bär redan ";SAK$(Objekt,0);"." : GOTO 12210
6414 IF Pryl(Objekt)=5 THEN PRINT "Man kan inte ta tillbaka saker från maskinen." : GOTO 12210
6418 GOTO 6430
6420 Flagga(1)=Flagga(1)+1 : Pryl(Objekt)=1
6423 IF Objekt=32 THEN Flagga(24)=Flagga(50)
6424 IF Objekt=30 AND (Jack(Z)=1 OR Flagga(44)=Z) THEN Flagga(44)=0 : PRINT "Du rycker loss sladden." : GOTO 12210
6425 IF Objekt=30 THEN Flagga(44)=0
6426 IF Objekt=25 AND (Flagga(44)=Z OR Jack(Z)=1) THEN PRINT "Du kopplar ur telefonen." ELSE PRINT "Ok."
6427 IF Objekt=25 AND Z=100 THEN PLAY OFF : PLAY "MF"
6428 GOTO 12210
6430 IF Pryl(Objekt)=Z THEN PRINT "Du måste ta den först!" : GOTO 12210
6432 IF Pryl(Objekt)=2 AND (Pryl(29)=Z OR Pryl(29)=1) THEN PRINT "Vakten håller hårt i ";SAK$(Objekt,0);"." : GOTO 12210
6434 IF (Objekt=11 OR Objekt=19 OR Objekt=10) AND Pryl(Objekt)=0 AND Flagga(30)=Z THEN PRINT "Gubben håller hårt i ";SAK$(Objekt,0);"." : GOTO 12210
6436 IF Objekt=14 AND Pryl(14)=0 AND (Pryl(32)=1 OR Pryl(32)=Z) THEN PRINT "Algerna sitter på sengångaren!" : GOTO 12210
6438 PRINT "Jag ser ";INGEN$(Objekt);SAK$(Objekt,1);" här." : GOTO 12210
6480 IF Pryl(32)=Z THEN PRINT "Du måste få tag i sengångaren först." : GOTO 12210
6482 Flagga(1)=Flagga(1)+1
6484 Pryl(14)=1
6486 Skrivrad 1, 501,502 : GOTO 12210
6500 I=0'TA ALLT
6505 FOR I1=1 TO Pryl(0)
6510   IF Pryl(I1)<>Z AND (I1<>30 OR Flagga(44)<>Z) THEN GOTO 6548
6512   IF I1=29 AND Flagga(6)<>1 THEN GOTO 6548
6515   IF Flagga(1)<9 THEN GOTO 6535
6520   IF I=0 THEN PRINT "Du kan inte bära fler saker."
6525   IF I>0 THEN PRINT "." : PRINT "Du kan inte bära resten."
6530   EXIT
6535   IF I=0 THEN PRINT "Du tar "; ELSE PRINT " och ";
6537   Flagga(2)=I1
6540   PRINT SAK$(I1,0); : Flagga(1)=Flagga(1)+1 : Pryl(I1)=1 : I=I+1
6542   IF I1=32 THEN Flagga(24)=Flagga(50)
6545   IF I1=22 AND Z=63 THEN Poang=Poang-30 : Flagga(52)=0
6546   IF I1=25 AND Z=100 THEN PLAY OFF : PLAY "MF"
6547   IF I1=30 THEN Flagga(44)=0
6548 NEXT I1
6550 IF I1<=Pryl(0) THEN GOTO 12210
6551 IF Z=70 AND A1=1 THEN PRINT : GOTO 6355
6552 IF I=0 AND Z=37 AND Flagga(38)=0 THEN PRINT "Jag ser inget du kan ta här." : GOTO 6560
6555 IF I=0 THEN PRINT "Det finns inget som du kan ta här." ELSE PRINT "."
6560 GOTO 12210
7000 REM XXXXX SLÄPP XXXXX
7001 IF Objekt>0 THEN GOTO 7030
7003 IF C$="" THEN C$=LASIN$("Släpp vadå? ") : A$=C$
7005 Objekt=Testasak(C$)
7007 IF Objekt>0 THEN GOTO 7030
7008 IF Lika(0,C$,"ALLTING","4") THEN GOTO 7100
7009 IF C$="DEJ" OR C$="DIG" THEN GOTO 7020
7018 PRINT "Du bär väl ingen ";C$;"!" : GOTO 12210
7020 PRINT "Fy! Det vill jag inte." : Poang=Poang-1 : GOTO 12210
7030 IF Pryl(Objekt)=1 THEN GOTO 7040
7034 PRINT "Du bär väl ";INGEN$(Objekt);SAK$(Objekt,1);"!"
7036 GOTO 12210
7040 IF (Objekt=10 OR Objekt=19) AND Flagga(30)=Z THEN GOTO 7090
7041 IF Objekt=25 AND (Flagga(44)=Z OR Jack(Z)=1) THEN GOTO 7075
7042 IF Z=51 AND Objekt<=Varde(0) THEN X=15 : GOTO 12999
7043 IF Z=4 THEN PRINT "En mystisk kraft hindrar dej från att släppa någonting här." : GOTO 12210
7044 IF Objekt=22 AND Z=63 THEN Flagga(52)=Flagga(50) : Poang=Poang+25
7045 IF Objekt=30 AND Jack(Z)=1 THEN Flagga(44)=-1 ELSE IF Objekt=30 THEN Flagga(44)=0
7046 IF Objekt=33 THEN GOTO 7070
7047 IF Objekt=32 THEN PRINT "Du känner dej genast mycket piggare."
7048 IF Objekt=15 AND Varde(15)>0 THEN PRINT "Oj! Spegeln sprack! Det var olyckligt!" : Varde(15)=0 : Flagga(3)=1 : Flagga(41)=1
7050 Pryl(Objekt)=Z : Flagga(1)=Flagga(1)-1
7052 IF Objekt=22 AND Z=63 THEN PRINT "Du lägger försiktigt ner liket." ELSE PRINT "Ok."
7054 GOTO 12210
7070 PRINT "När du lägger en natriumpollett på marken oxideras den häftigt och försvinner."
7071 Flagga(16)=Flagga(16)-1
7072 IF Flagga(16)>0 THEN GOTO 12210
7073 Pryl(33)=4 : Flagga(16)=2 : Flagga(1)=Flagga(1)-1
7074 PRINT "Där rök din sista pollett!" : GOTO 12210
7075 Flagga(28)=Flagga(28)+1 : Flagga(1)=Flagga(1)-1 : Pryl(25)=Z
7077 IF Flagga(28)=2 THEN GOTO 27100
7078 IF Flagga(28)/3=INT(Flagga(28)/3) AND RND(1)>0.5 THEN GOTO 7080 ELSE IF Z=100 AND Flagga(27)=1 AND Jack(100)=1 THEN Telefon : ON PLAY(1) Itelefon : PLA
7079 PRINT "Du kopplar in telefonen." : GOTO 12210
7080 PRINT "Just som du ska koppla in telefonen kommer en man klädd i en röd"
7081 PRINT "dräkt som det står 'TELE' på, ";
7082 IF Jack(Z)=1 THEN PRINT "skruvar bort telefonjacken" : Jack(Z)=2 : GOTO 7084
7083 IF Jack(Z)<>1 THEN PRINT "tar bort förlängningssladden" : Flagga(44)=0 : Pryl(30)=0
7084 PRINT "och sluddrar fram:"
7085 PRINT "- Abonnemangsavgiften är inte betald." : PRINT
7087 X1=1 : GOSUB 27050
7088 GOTO 12999
7090 IF Objekt=19 THEN PRINT "Gubben tar snabbt vattenflaskan när du släpper den." : GOTO 7094
7092 PRINT "Gubben sätter din lagerkrans på sitt huvud och ser genast gladare ut."
7094 Pryl(Objekt)=0 : Flagga(1)=Flagga(1)-1
7096 GOTO 12210
7100 I=0'SLÄPP ALLT
7102 IF Z=4 THEN GOTO 7043
7105 FOR I1=1 TO Pryl(0)
7107   Flagga(2)=I1
7110   IF Pryl(I1)<>1 THEN GOTO 7135
7115   IF I1=22 AND Z=63 THEN Flagga(52)=Flagga(50) : Poang=Poang+25
7116   IF I1=25 AND Z=100 AND Flagga(27)=1 AND Jack(100)=1 THEN Telefon : ON PLAY(1) Itelefon : PLAY ON
7117   IF I1=30 AND Jack(Z)=1 THEN Flagga(44)=-1 ELSE IF I1=30 THEN Flagga(44)=0
7119   IF I1=33 THEN GOTO 7135
7120   Pryl(I1)=Z : Flagga(1)=Flagga(1)-1
7125   IF I=0 THEN PRINT "Du släpper "; ELSE PRINT " och ";
7130   PRINT SAK$(I1,0); : I=I+1
7132   IF I1=15 AND Varde(15)>0 THEN PRINT " som spricker"; : Varde(15)=0 : Flagga(3)=1 : Flagga(41)=1
7135 NEXT I1
7137 IF I>0 THEN PRINT "."
7138 IF Pryl(33)=1 THEN GOTO 7070
7140 IF I=0 THEN PRINT "Du bär inte på någonting!"
7145 GOTO 12210
8600 Flagga(36)=1 'XXX VÄDERSTRECKSSUBRUTIN XXXXX
8602 Flagga(50)=Flagga(50)+1
8604 IF Lika(1,A$,"SYDOST","SYDÖST") OR A$="SO" OR A$="SÖ" THEN X=8 : GOTO 11000
8605 IF Lika(1,A$,"VÄSTER","4") OR A$="V" THEN X=1 : GOTO 11000
8606 IF Lika(1,A$,"ÖSTER","3") OR Lika(0,A$,"OST","") OR A$="Ö" THEN X=2 : GOTO 11000
8607 IF Lika(1,A$,"NORR","NORD") OR A$="N" THEN X=3 : GOTO 11000
8608 IF Lika(1,A$,"SÖDER","SYD") OR A$="S" THEN X=4 : GOTO 11000
8609 IF Lika(1,A$,"NORDVÄST","") OR A$="NV" THEN X=5 : GOTO 11000
8610 IF Lika(1,A$,"SYDVÄST","") OR A$="SV" THEN X=6 : GOTO 11000
8611 IF Lika(1,A$,"NORDÖST","NORDOST") OR A$="NO" OR A$="NÖ" THEN X=9 : GOTO 11000
8612 GOTO 12025
8613 'XXXX INVENTERING XXX
8614 IF Flagga(1)=0 THEN PRINT "Du bär ingenting." : GOTO 12210
8615 PRINT "Du bär på"
8617 IF Pryl(1)=1 THEN PRINT "en gnistrande diamant"
8618 IF Pryl(31)=1 THEN PRINT "en stor kofot"
8619 IF Pryl(16)=1 THEN PRINT "en ny cykelpump"
8620 IF Pryl(2)=1 THEN PRINT "en illaluktande gurka"' uääääääääääääääää
8621 IF Pryl(3)=1 THEN PRINT "en snygg silvertacka"
8622 IF Pryl(17)=1 THEN PRINT "en lång stege"
8623 IF Pryl(18)<>1 THEN GOTO 8626
8624 IF Flagga(31)=0 THEN PRINT "en full brännvinsflaska"
8625 IF Flagga(31)=1 THEN PRINT "en tom brännvinsflaska"
8626 IF Pryl(19)<>1 THEN GOTO 8629
8627 IF Flagga(32)=0 THEN PRINT "en full vattenflaska"
8628 IF Flagga(32)=1 THEN PRINT "en tom vattenflaska"
8629 IF Pryl(20)<>1 THEN GOTO 8632
8630 IF Flagga(33)=0 THEN PRINT "en pumpad boll"
8631 IF Flagga(33)=1 THEN PRINT "en opumpad boll"
8632 IF Pryl(4)=1 THEN PRINT "en sylvass hillebard"
8633 IF Pryl(21)=1 THEN PRINT "en jordig spade"
8634 IF Pryl(5)=1 THEN PRINT "en urgammal döskalle"
8635 IF Pryl(6)=1 THEN PRINT "en tickande väckarklocka"
8637 IF Pryl(11)=1 THEN PRINT "ett glittrande pärlhalsband"
8638 IF Pryl(22)=1 THEN PRINT "ett äckligt lik"
8640 IF Pryl(12)=1 THEN PRINT "en ful faunsko"
8641 IF Pryl(7)=1 THEN PRINT "en massa guldmynt"
8642 IF Pryl(25)=1 THEN PRINT "en modern telefon"
8645 IF Pryl(26)=1 THEN PRINT "några gamla nycklar"
8646 IF Pryl(27)=1 THEN PRINT "en vass sax"
8647 IF Pryl(28)=1 THEN PRINT "en tung slägga"
8650 IF Pryl(8)=1 THEN PRINT "en illustrerad familjebibel"
8651 IF Pryl(24)=1 THEN PRINT "en lampa"
8652 IF Pryl(9)=1 THEN PRINT "ett skärt kontrakt"
8653 IF Pryl(10)=1 THEN PRINT "en grön lagerkrans"
8654 IF Pryl(23)=1 THEN PRINT "en tunn telefonkatalog"
8656 IF Pryl(30)=1 THEN PRINT "en förlängningssladd till telefonen"
8657 IF Pryl(13)=1 AND Varde(13)=0 THEN PRINT "en medalj av mörk choklad"
8658 IF Pryl(13)=1 AND Varde(13)>0 THEN PRINT "en ärorik medalj"
8659 IF Pryl(14)=1 THEN PRINT "gröna alger av släktet Cyanophyceae"
8660 IF Pryl(15)=1 AND Varde(15)=0 THEN PRINT "en sönderslagen spegel"
8661 IF Pryl(15)=1 AND Varde(15)>0 THEN PRINT "en mycket skör spegel"
8662 IF Pryl(32)=1 AND Pryl(14)=0 THEN PRINT "en sömnig, algklädd sengångare"
8663 IF Pryl(32)=1 AND Pryl(14)>0 THEN PRINT "en mycket sömnig, pälsklädd sengångare"
8664 IF Pryl(33)=1 AND Flagga(16)=1 THEN PRINT "en sketen pollett"
8665 IF Pryl(33)=1 AND Flagga(16)>1 THEN PRINT Flagga(16);"oansenliga polletter"
8669 GOTO 12210
9950 IF M2%=1 THEN CLOSE 2 : M2%=0'&&&&& Stäng ev. loggfil
9951 PRINT : IF Jasvar("Är du säker på att du vill sluta nu? ") THEN GOTO 32500
9957 PRINT "Ok. Du har";Poang;"poäng!"
9958 GOTO 12210
11000 IF Flagga(21)>0 THEN Sov "haltar",5
11005 Flagga(2)=0
11010 GOTO 12999
12000 Flagga(36)=0'XXXXX KOMMANDOAVKODARE XXXXX
12002 Flagga(50)=Flagga(50)+1
12010 IF Lika(1,A$,"UPPÅT","3") OR A$="U" THEN X=1 : GOTO 11000
12012 IF Lika(1,A$,"NERÅT","3") OR Lika(0,A$,"NEDÅT","3") OR A$="N" THEN X=2 : GOTO 11000
12014 IF Lika(1,A$,"VÄNSTER","") OR A$="V" THEN X=3 : GOTO 11000
12016 IF Lika(1,A$,"HÖGER","") OR A$="H" THEN X=4 : GOTO 11000
12018 IF Lika(1,A$,"FRAMÅT","") OR A$="F" THEN X=5 : GOTO 11000
12020 IF Lika(1,A$,"BAKÅT","") OR A$="B" THEN X=6 : GOTO 11000
12025 IF Lika(1,A$,"HJÄLP","") THEN X=7 : GOTO 12999
12030 E=INSTR(1,A$," ") : C$=MID$(A$,E)
12032 IF E=0 THEN C$="" : GOTO 12040''
12034 IF LEFT$(C$,1)=" " THEN C$=MID$(C$,2) : GOTO 12032
12040 IF Objekt>0 THEN Flagga(2)=Objekt
12042 Objekt=Testasak(C$)''
12051 IF Lika(0,A$,"KLAPPA","SMEK") THEN GOTO 13000
12052 IF Lika(0,A$,"VÄNTA","STANNA") THEN GOTO 12570
12053 IF Lika(0,A$,"ÄT","BIT") OR Lika(0,A$,"SMAKA","") THEN GOTO 13030
12054 IF Lika(0,A$,"KLÄTTRA","") THEN GOTO 12330
12055 IF Lika(1,A$,"HELVETE","") THEN PRINT "Åt vilket håll är det?" : GOTO 12210
12056 IF Lika(0,A$,"HOPPA","") THEN GOTO 12130
12057 IF Lika(0,A$,"VRICKA","") THEN GOTO 12584
12058 IF Lika(1,A$,"KNACKA","") THEN PRINT "Ingenting händer." : GOTO 12210
12059 IF Lika(0,A$,"SKIT","4") THEN GOTO 12590
12060 IF Lika(0,A$,"SKRIK","") THEN GOTO 12550
12061 IF Lika(1,A$,"FAN","JÄVLAR") OR Lika(1,A$,"DJÄVLAR","SATANS") THEN GOTO 12555
12062 IF Lika(0,A$,"TITTA","SE") THEN X1=1 : Flagga(50)=Flagga(50)-1
12063 IF A$="ALEA JACTA EST" THEN GOTO 12220
12064 IF Lika(0,A$,"SLUTA","") THEN GOTO 9950
12065 IF Lika(0,A$,"GÅ","") THEN GOTO 12255
12066 IF Lika(0,A$,"BLÄNDA","") THEN GOTO 12850
12067 IF Lika(0,A$,"GE","GIV") THEN GOTO 28090
12069 IF Lika(0,A$,"VÄCK","4") AND Flagga(6)=3 THEN GOTO 28000
12070 IF Lika(0,A$,"INVENTERA","INVENTERING") THEN GOTO 8613
12071 IF Lika(0,A$,"TAG","2") THEN GOTO 6300
12072 IF Lika(0,A$,"SLÄPP","") THEN GOTO 7000
12073 IF Lika(0,A$,"FYLL","") THEN GOTO 12340
12074 IF Lika(0,A$,"DRICK","") THEN GOTO 12270
12075 IF Lika(0,A$,"RING","") THEN GOTO 12800
12076 IF Lika(1,A$,"POÄNG","") THEN PRINT "Du har";Poang;"poäng." : GOTO 12210
12077 IF Lika(0,A$,"GRÄV","") THEN GOTO 12230
12078 IF Lika(0,A$,"SPARKA","") THEN GOTO 12580
12079 IF Lika(0,A$,"DÖDA","") THEN GOTO 12240
12080 IF A$="?" THEN X1=1 : Flagga(50)=Flagga(50)-1 : Skrivrad 1, 132, 150
12081 IF Lika(1,A$,"UT","") THEN GOTO 12420
12082 IF Lika(0,A$,"ÖPPNA","") THEN GOTO 12440
12083 IF Lika(0,A$,"STÄNG","") THEN GOTO 12470
12084 IF Lika(0,A$,"LÄS","") THEN GOTO 12650
12085 IF Lika(0,A$,"PUMPA","") THEN GOTO 12900
12086 IF Lika(0,A$,"SIMMA","DUSCHA") OR Lika(0,A$,"BADA","") THEN GOTO 12890
12087 IF Lika(0,A$,"FÖLJ","") THEN GOTO 12910
12088 IF Lika(1,A$,"IN","IGENOM") THEN GOTO 12400
12089 IF Lika(0,A$,"KASTA","") THEN PRINT "Tyvärr har kastarmen gått ur led." : GOTO 12210
12090 IF Lika(0,A$,"INFORMATION","4") THEN GOTO 12770
12092 IF Lika(0,A$,"BEGRAV","") THEN GOTO 12510
12093 IF Lika(1,A$,"KOPPLA IN","9") THEN GOTO 12140
12094 IF Lika(1,A$,"KOPPLA UR","9") THEN GOTO 12160
12095 IF Lika(0,A$,"SPARA","") THEN GOTO 31000'           &&&&&
12096 IF Lika(0,A$,"ÅTERSKAPA","") THEN GOTO 31200'   &&&&&
12097 IF Lika(0,A$,"LOGGA","4") THEN GOTO 12950'    &&&&&
12099 IF Objekt=15 OR Lika(1,A$,"SPEGLA","") THEN GOTO 12560
12102 IF E>1 THEN C$=LEFT$(A$,E-1) ELSE C$=A$
12103 Objekt=Testasak(C$)
12104 IF Objekt>0 THEN GOTO 12600 ELSE GOTO 12999
12120 IF Flagga(33)=1 THEN PRINT"Bollen är inte pumpad." : GOTO 12210
12122 IF Z=39 AND Flagga(48)<1 THEN X=13 : GOTO 12999
12123 PRINT "Du sparkar bollen så hårt att den försvinner."
12124 IF Pryl(20)=1 THEN Flagga(1)=Flagga(1)-1
12125 Pryl(20)=Z-1
12126 GOTO 12210
12130 IF Lika(1,C$,"VATTEN","VATTNET") THEN GOTO 12890
12132 IF Flagga(36)<>0 THEN GOTO 12057
12134 IF Lika(0,C$,"UPPÅT","3") THEN X=1 ELSE X=2
12136 GOTO 12999
12140 IF Jack(Z)<>1 AND Flagga(44)<>Z THEN PRINT "Det finns ingen jack här." : GOTO 12210'KOPPLA IN
12142 IF Objekt=0 THEN Objekt=Testasak(LASIN$("Vad vill du koppla in? "))
12144 IF Objekt=25 OR Objekt=30 THEN GOTO 7030'Telefon eller förlängningssladd
12148 PRINT "Det kan man inte koppla in." : GOTO 12210
12160 IF (Objekt=30 AND Flagga(44)=Z) OR (Objekt>0 AND Jack(Z)=1) THEN GOTO 6410
12170 IF Flagga(44)=Z THEN Objekt=30 : GOTO 6410
12172 IF Jack(Z)<>1 THEN PRINT "Det finns ingen jack här." : GOTO 12210
12174 IF Pryl(25)=Z THEN Objekt=25 : GOTO 6410
12176 IF Pryl(30)=Z THEN Objekt=30 : GOTO 6410
12178 PRINT "Ingenting är inkopplat i jacken." : GOTO 12210
12210 PRINT'XXXX INMATNINGSRUTIN XXXX
12211 IF Flagga(49)=0 AND Flagga(30)=Z AND Z<>96 THEN X1=1 : GOTO 30000
12212 IF Flagga(1)<9 AND Z=60 THEN GOTO 12999
12213 A$=LASIN$("")
12214 PRINT
12215 GOTO 1
12220 IF Pryl(10)=5 OR Pryl(10)=53 THEN PRINT "Ingenting händer." : GOTO 12210
12223 IF Pryl(10)=1 THEN GOTO 12226
12224 IF Z=53 THEN PRINT "Nu hänger lagerkransen på väggen." ELSE PRINT "Ok."
12225 Pryl(10)=53 : GOTO 12210
12226 Flagga(1)=Flagga(1)-1 : Pryl(10)=53
12227 IF Z=53 THEN GOTO 12224
12228 PRINT "Lagerkransen försvinner." : GOTO 12210
12230 IF Pryl(21)<>1 THEN PRINT "Du har ingenting du kan gräva med." : GOTO 12210
12232 IF Z<>77 THEN PRINT "Marken är för hård." : GOTO 12210
12234 IF Flagga(20)=1 THEN PRINT "Platsen är redan helt utgrävd." : GOTO 12210
12236 X=13 : GOTO 12999
12240 IF Lika(0,C$,"GUBBEN","") OR (C$="" AND Flagga(30)=Z) THEN GOTO 30050
12242 IF Lika(0,C$,"VAKTEN","4") OR (C$="" AND (Pryl(29)=Z OR Pryl(29)=1)) THEN GOTO 28010
12248 PRINT "Det finns inget du kan döda här." : GOTO 12210
12255 IF C$<>"" THEN A$=C$ : GOTO 1
12260 PRINT "Åt vilket håll?"
12261 A$=LASIN$("")
12262 GOTO 1
12270 IF Objekt=18 THEN GOTO 12280'Brännvinsflaska''
12272 IF Objekt=19 OR Lika(0,C$,"VATTEN","VATTNET") THEN GOTO 12282
12274 IF C$<>"" THEN PRINT "Det går inte att dricka!" : GOTO 12210''
12276 C$=LASIN$("Drick vad ? ")
12278 GOTO 12270
12280 IF Pryl(18)=1 AND Flagga(31)=0 THEN GOTO 12310 ELSE GOTO 12306
12282 IF Pryl(19)=1 AND Flagga(32)=0 THEN GOTO 12292
12284 IF Z=25 OR Z=33 OR Z=49 OR Z=50 OR Z=66 OR Z=70 THEN GOTO 12296
12286 IF Z=72 OR Z=74 OR Z=78 OR Z=79 OR Z=83 OR Z=87 OR Z=88 THEN GOTO 12296
12288 IF Z=91 THEN PRINT "Du dricker ur vattenfallet." : GOTO 12300
12290 PRINT "Jag ser inget VATTEN här." : GOTO 12210
12292 PRINT "Du dricker ur vattenflaskan."
12294 Flagga(32)=1 : GOTO 12300
12296 PRINT "Du dricker ur sjön."
12300 PRINT "Klunk...klunk...klunk......AHHHH!"
12302 GOTO 12210
12304 IF Pryl(18)=1 AND Flagga(31)=0 THEN GOTO 12348
12306 PRINT "Jag ser inget BRÄNNVIN här." : GOTO 12210
12310 Skrivrad 1, 13, 14 : BEEP
12314 FOR I=1 TO 9
12316   Sov "Hickar", 3
12318   PRINT TAB(INT(RND(1)*66)+1);"HICK!" : BEEP
12320 NEXT I
12322 PRINT : Flagga(31)=1
12324 PRINT "Nu hoppas jag att vi har nyktrat till så pass att vi kan fortsätta!"
12326 PRINT : GOTO 12210
12330 IF Lika(0,C$,"NER","NED") AND Flagga(36)=0 THEN X=2 : GOTO 12999
12332 IF Z=13 OR Z=15 OR Z=39 OR Z=11 THEN X=1 : GOTO 12999
12333 IF Z>70 AND Z<88 AND Z<>80 AND Z<>78 AND Z<>81 THEN GOTO 12337
12335 IF Flagga(17)=1 OR Flagga(17)=Z THEN PRINT "Du klättrar upp på stegen och ner igen." : GOTO 12210
12336 PRINT "Det finns inget du kan klättra på här." : GOTO 12210
12337 Skrivrad 1, 16, 18 : GOTO 12584
12340 IF Lika(0,C$,"GUBBEN","") OR (C$="" AND Flagga(30)=Z) THEN GOTO 30010
12341 IF Lika(0,C$,"VAKTEN","4") OR (C$="" AND (Pryl(29)=Z OR Pryl(29)=1)) THEN GOTO 12360
12342 IF Objekt=18 THEN GOTO 12304
12344 IF Objekt=19 THEN GOTO 12380'Vattenflaska''
12346 IF C$="" OR Lika(1,C$,"FLASKAN","") THEN GOTO 12350
12348 PRINT "Det finns inget du kan fylla här!" : GOTO 12210
12350 IF Pryl(18)=1 OR Pryl(19)=1 THEN C$=LASIN$("Fyll vad? ") : Objekt=Testasak(C$) : GOTO 12340
12354 PRINT "Du har ju ingen flaska!" : GOTO 12210
12360 IF Pryl(29)<>Z AND Pryl(29)<>1 THEN Objekt=29 : GOTO 6430
12361 IF Flagga(6)=2 THEN PRINT "Vakten är ju död." : GOTO 12210
12362 IF Flagga(6)=3 THEN PRINT "Han sover för djupt." : GOTO 12210
12364 IF Pryl(18)<>1 THEN PRINT "Du har inget att fylla honom med." : GOTO 12210
12366 IF Flagga(31)=1 THEN PRINT "Din brännvinsflaska är tom." : GOTO 12210
12368 Flagga(31)=1 : Flagga(51)=Flagga(50)
12370 PRINT "Vakten dricker upp ditt brännvin i en enda klunk."
12372 IF Flagga(6)=0 THEN Flagga(6)=1 : GOTO 12210
12374 PRINT "Den nu redlöst fulle vakten ramlar ihop i en hög på golvet och somnar."
12376 Flagga(6)=3 : Pryl(29)=Z : GOTO 12210
12380 IF Pryl(19)<>1 THEN PRINT "Du bär ingen vattenflaska som du kan fylla." : GOTO 12210
12382 IF Flagga(32)=0 THEN PRINT "Din vattenflaska är så full den kan bli." : GOTO 12210
12384 IF Z=25 OR Z=33 OR Z=49 OR Z=50 OR Z=66 OR Z=70 OR Z=72 THEN GOTO 12390
12386 IF Z=74 OR Z=78 OR Z=79 OR Z=83 OR Z=87 OR Z=88 OR Z=91 THEN GOTO 12390
12388 GOTO 12290
12390 PRINT "Du fyller vattenflaskan med vatten från ";
12392 IF Z=91 THEN PRINT "vattenfallet." ELSE PRINT "sjön."
12394 Flagga(32)=0 : GOTO 12210
12400 IF Lika(0,A$,"KOPPLA","") THEN GOTO 12140'*** In
12401 IF Z=81 THEN X=3
12402 IF Z=50 THEN X=4
12404 IF Z=99 OR Z=55 OR Z=62 THEN X=5
12406 IF Z=30 THEN Flagga(23)=1 : PRINT "I garderoben hittar du ett kassaskåp." : GOTO 12210
12408 IF X>0 THEN GOTO 12999 ELSE GOTO 12089
12420 IF Lika(0,A$,"KOPPLA","") THEN GOTO 12160'*** Ut
12422 IF Z=8 OR Z=51 OR Z=100 THEN X=6
12424 IF Z=31 THEN Z=30 : GOTO 12999
12428 IF X>0 THEN GOTO 12999 ELSE GOTO 12082
12440 IF Z=81 OR Z=8 THEN GOTO 12450
12442 IF Z=30 THEN GOTO 12456
12444 IF Z=62 AND Flagga(7)=0 THEN PRINT "Porten är låst." : GOTO 12210
12448 GOTO 12083
12450 IF Flagga(19)=1 THEN PRINT "Dörren är ju redan öppen!" : GOTO 12210
12452 PRINT "Dörren öppnas med ett gnäll." : Flagga(19)=1
12453 PLAY "T250L64MLO1CBP64B-AP64A-GG-P64FEE-DD-CO6L4G#L64AMN"
12454 GOTO 12210
12456 IF Lika(1,A$,"GARDEROBEN","") OR (C$="" AND Flagga(23)=0) THEN GOTO 12466
12458 IF Lika(1,A$,"KASSASKÅPET","")=0 AND C$<>"" THEN GOTO 12083
12460 IF Z=31 THEN PRINT "Kassaskåpet är redan öppet." : GOTO 12210
12464 PRINT "Dörren är utan handtag och lås." : GOTO 12210
12466 IF Flagga(23)=1 THEN PRINT "Garderoben är redan öppen." : GOTO 12210
12468 PRINT "Du öppnar garderoben och hittar ett litet kassaskåp där."
12469 Flagga(23)=1 : GOTO 12210
12470 IF Z=30 THEN GOTO 12480
12472 IF Z=31 THEN GOTO 12486
12474 IF (Z=81 OR Z=8) AND Flagga(19)=0 THEN PRINT "Dörren är ju redan stängd." : GOTO 12210
12475 IF Z=81 OR Z=8 THEN PRINT "Dörren stängs med ett gnäll." : Flagga(19)=0 : GOTO 12453
12478 GOTO 12084
12480 IF Flagga(23)=0 THEN PRINT "Garderoben är redan stängd." : GOTO 12210
12482 PRINT "Ok." : Flagga(23)=0 : GOTO 12210
12486 IF Lika(1,A$,"GARDEROBEN","") OR (C$="" AND Flagga(23)=0) THEN GOTO 12494
12488 IF Lika(1,A$,"KASSASKÅPET","")=0 AND C$<>"" THEN GOTO 12084
12490 PRINT "Kassaskåpet stängs sakta."
12492 Z=30 : GOTO 12999
12494 PRINT "Kassaskåpet och garderoben stängs."
12496 Flagga(23)=0 : GOTO 12492
12510 IF Z<>63 AND Z<>61 THEN PRINT "Marken är för hård!" : GOTO 12210
12512 IF Objekt<>22 AND Objekt<>29 AND (C$<>"" OR Pryl(22)<>1) THEN GOTO 12526
12514 IF Pryl(29)=1 OR Pryl(29)=Z THEN PRINT "Du kan inte begrava en levande!" : GOTO 12210
12516 IF Pryl(22)<>1 AND Pryl(22)<>Z THEN PRINT "Du har inget sånt att begrava!" : GOTO 12210
12518 IF Pryl(22)=63 THEN PRINT "Liket är redan begravt!" : GOTO 12210
12520 IF Pryl(22)=1 THEN Flagga(1)=Flagga(1)-1
12522 Pryl(22)=63 : Poang=Poang+25 : Flagga(52)=Flagga(50)
12524 PRINT "Ok." : GOTO 12210
12526 PRINT "Man kan bara begrava lik!" : GOTO 12210
12550 I=INSTR(A$," ")'================== SKRIK
12551 IF I=0 OR I=LEN(A$) THEN PRINT "AAAAAAARRRRRRRRRRGHHHHHH!" ELSE PRINT "Ok. ";MID$(A$,I+1)
12553 GOTO 12210
12555 '====================================================SVÄRORD============
12557 PRINT "Vilket språk!"
12559 GOTO 12210
12560 IF Pryl(15)<>1 AND Pryl(15)<>Z THEN Objekt=15 : GOTO 6430
12562 IF Varde(15)=0 THEN PRINT "Spegeln är i bitar!" : GOTO 12210
12564 PRINT "Du tittar i spegeln, men det är ingen vacker syn:" : Thorvald 1
12566 GOTO 12210
12570 '==========================================VÄNTA===========STANNA=======
12572 PRINT"Ok."; : Sov "väntar",10
12574 IF Z=37 THEN GOTO 12999 ELSE PRINT "Så där ja!"
12576 GOTO 12210
12580 IF Flagga(21)>0 THEN PRINT"Du kan inte sparka något med vrickade fötter!" : GOTO 12210
12582 IF (Objekt=20 OR C$="") AND (Pryl(20)=Z OR Pryl(20)=1) THEN GOTO 12120
12584 PRINT"AJ! Du vrickar dina fötter."
12586 Flagga(21)=Flagga(50)
12588 GOTO 12210
12590 IF Z<>23 THEN PRINT "Ok." : GOTO 12210
12592 PRINT "Oj! Du ramlar själv ner i toaletten." : A$="SPOLA"
12594 GOTO 12999
12600 PRINT "Vad vill du göra med ";SAK$(I,0);"?"
12601 A1$=LASIN$("") : A$=A1$+" "+SAK$(I,1)
12610 GOTO 12214
12650 IF Objekt>0 THEN IF Pryl(Objekt)<>1 AND Pryl(Objekt)<>Z THEN GOTO 6430
12651 IF Objekt=23 THEN Skrivrad 1, 504,514 : GOTO 12210 'Katalog
12652 IF Objekt=9 THEN GOTO 12700'Kontrakt''
12654 IF Objekt=8 THEN GOTO 12670
12655 IF Lika(1,A$,"LOGGFILEN","")>0 THEN GOTO 12975'&&&&&
12656 IF Objekt=6 THEN PRINT : PRINT TIME$ : PRINT : GOTO 12210'Klockan
12657 IF Lika(1,A$,"GRAVEN","4") OR Lika(1,A$,"STENEN","4") OR Lika(1,A$,"GRAVSTENEN","") THEN GOTO 12750
12658 IF C$<>"" THEN PRINT "Det går inte att läsa ";C$;"." : GOTO 12210
12660 A$=LASIN$("Vad vill du läsa? ") : C$=A$ : Objekt=Testasak(A$) : GOTO 12650
12670 D=INT(RND*3)+1
12675 IF D=1 THEN Skrivrad 1,33,41
12676 IF D=2 THEN Skrivrad 1,406, 414
12678 IF D=3 THEN Skrivrad 1,418, 426
12679 GOTO 12210
12700 PRINT "Tyvärr är kontraktet skrivet med Kermits oläsliga handstil."
12704 PRINT : GOTO 12210
12750 IF Z<>61 THEN PRINT "Jag ser ingen GRAVSTEN här." : GOTO 12210
12752 IF Namn$="" THEN PRINT "Gravstenen är tom." : GOTO 12210
12754 PRINT "Här vilar sej ";Namn$;"." : PRINT Namn$;
12756 PRINT " försvann in i ett okänt hus och sågs aldrig mer." : PRINT : GOTO 12210
12770 Skrivrad 1, 44,62 : GOTO 12210
12800 IF Pryl(25)=Z THEN GOTO 12806
12802 IF Pryl(25)=1 AND (Jack(Z)=1 OR Flagga(44)=Z) THEN PRINT "Du HÅLLER ju i telefonsladden." : GOTO 12210
12804 PRINT "Jag ser ingen RING här." : GOTO 12210
12806 IF Jack(Z)<>1 AND Flagga(44)<>Z THEN PRINT "Jag ser ingen telefonjack här." : GOTO 12210
12808 IF C$="" THEN C$=LASIN$("Ring vart:")
12809 IF Lika(1,C$,"100","") THEN GOTO 27600
12810 PLAY "T120O2L4G#P8G#.P2P4G#P8G#.P16"
12812 IF Lika(1,C$,"000","") THEN GOTO 27250
12814 IF Lika(1,C$,"307","323") THEN GOTO 27620
12816 IF Lika(1,C$,"405","") THEN GOTO 27200
12818 IF Lika(1,C$,"481","") THEN GOTO 27630
12821 IF Lika(1,C$,"900","") THEN GOTO 27400
12822 IF Lika(1,C$,"HEM","") THEN GOTO 12835
12826 PRINT "Du hör en röst säja:"
12828 PRINT "- Ingen abonnent på det numret."
12830 GOTO 12210
12835 IF Namn$="" THEN PRINT "Du hör din egen röst:  - Jag är inte hemma än!" : GOTO 12210
12836 PRINT "Du hör en röst säja:"
12837 PRINT "- Detta är ";Namn$;"s telefonsvarare. ";Namn$;" är inte hemma."
12838 GOTO 12210
12840 IF Flagga(30)<>Z THEN PRINT "Du kan inte ge något till någon här." : GOTO 12210
12842 IF Objekt=10 OR Objekt=19 THEN GOTO 7030
12844 IF Lika(1,C$,"VATTEN","VATTNET") THEN GOTO 30010
12846 PRINT "Gubben tar inte emot det." : GOTO 12210
12850 IF Pryl(24)<>1 THEN PRINT "Du har inget att blända med." : GOTO 12210
12851 IF Lika(0,C$,"GUBBEN","") OR (C$="" AND Flagga(30)=Z) THEN GOTO 12860
12852 IF Lika(0,C$,"VAKTEN","4") OR (C$="" AND (Pryl(29)=Z OR Pryl(29)=1)) THEN GOTO 12880
12854 PRINT "Du kan inte blända någon här." : GOTO 12210
12860 IF Flagga(30)<>Z THEN GOTO 30060
12864 IF Pryl(19)=0 THEN PRINT "Du försöker blända gubben men han håller vattenflaskan som skydd." : GOTO 12210
12868 IF Flagga(49)=0 OR Flagga(49)=2 OR Flagga(30)=96 THEN GOTO 30002
12870 Flagga(49)=2 : PRINT "Du bländar den stackars gubben med lampan." : GOTO 12210
12880 IF (Pryl(29)=Z OR Pryl(29)=1) AND Flagga(6)<>2 THEN PRINT "Du klarar inte av att blända vakten." : GOTO 12210
12881 IF Flagga(6)=2 THEN PRINT "Vakten är död." ELSE Objekt=29 : GOTO 6430
12882 GOTO 12210
12890 IF Z=25 OR Z=33 OR Z=49 OR Z=50 OR Z=66 OR Z=70 THEN X=10 : GOTO 12999
12892 IF Z=72 OR Z=74 OR Z=78 OR Z=79 OR Z=83 OR Z=87 OR Z=88 THEN X=10 : GOTO 12999
12893 IF Z=91 THEN PRINT "Vattenfallets vatten är alldeles för kallt." : GOTO 12210
12894 PRINT "Här finns det inget vatten." : GOTO 12210
12900 IF Pryl(20)<>1 THEN PRINT "Du har inget att pumpa."
12902 IF Pryl(16)<>1 THEN PRINT "Du har inget att pumpa med."
12904 IF Pryl(20)=1 AND Flagga(33)=0 THEN PRINT "Din boll är redan pumpad."
12906 IF Pryl(16)=1 AND Pryl(20)=1 AND Flagga(33)=1 THEN PRINT "Ok." : Flagga(33)=0
12908 GOTO 12210
12910 IF Lika(1,A$,"RÖVAREN","") THEN PRINT "Jag ser ingen RÖVARE här." : GOTO 12210
12912 IF Lika(1,A$,"GUBBEN","") THEN GOTO 12920
12914 IF Lika(1,A$,"EFTER","") THEN PRINT "Jag ser ingen du kan följa efter." : GOTO 12210
12916 IF (Lika(1,A$,"RÅDET","3") OR Lika(1,A$,"THORVALD","")) AND Z=59 THEN GOTO 12924
12918 GOTO 12088
12920 IF Flagga(30)=Z THEN PRINT "Gubben sitter ju här!" : GOTO 12210
12922 GOTO 30060
12924 Skrivrad 1, 64, 66 : GOTO 12210
12950 IF M2%=1 THEN GOTO 12970'&&&&&
12951 M2$=LASIN$("Vad heter loggfilen? ")'&&&&&
12952 ON ERROR GOTO 12964'&&&&&
12954 OPEN M2$ FOR OUTPUT AS #2'&&&&&
12956 ON ERROR GOSUB 32450'&&&&&
12958 PRINT "Nu loggas alla kommandon på filen ";M2$'&&&&&
12960 M2%=1 : W$=CHR$(3) : GOTO 12210'&&&&&
12964 ON ERROR GOSUB 32450'&&&&&
12966 PRINT "? Jag kan inte öppna ";M2$'&&&&&
12968 X1=1 : GOTO 32600
12970 M2%=0 : CLOSE 2'&&&&&
12972 PRINT "Loggningen på ";M2$;" avslutad."'&&&&&
12974 GOTO 12210'&&&&&
12975 M3$=LASIN$("Vad heter loggfilen? ")'&&&&&
12976 IF M3$="" THEN GOTO 12210 ELSE IF M3%=1 THEN CLOSE 3 : M3%=0'&&&&&
12977 ON ERROR GOTO 12985'&&&&&
12979 OPEN M3$ FOR INPUT AS #3'&&&&&
12981 ON ERROR GOSUB 32450'&&&&&
12983 M3%=1 : GOTO 12210'&&&&&
12985 PRINT "? Jag kan inte öppna ";M3$'&&&&&
12988 ON ERROR GOSUB 32450'&&&&&
12989 X1=1 : GOTO 32600
12999 IF Flagga(30)=Z THEN GOTO 30000 ELSE GOTO 32600
13000 IF Objekt=0 THEN GOTO 13020
13002 IF Pryl(Objekt)<>1 AND Pryl(Objekt)<>Z THEN GOTO 6430
13004 IF Objekt<>32 THEN GOTO 13010
13005 IF Pryl(32)=Z THEN PRINT "Var snäll och ta upp den i famnen först." : GOTO 12210
13006 PRINT "När du klappar sengångaren kurar den ihop sej och somnar i din famn."
13008 GOTO 12210
13010 IF Objekt=29 THEN PRINT "Vakten bara grymtar när du klappar honom." : GOTO 12210
13012 PRINT "Du klappar ";SAK$(Objekt,0);"." : GOTO 12210
13020 IF C$="" THEN C$=LASIN$("Klappa vadå? ") : Objekt=Testasak(C$) : GOTO 13000
13022 PRINT "Jag vill inte klappa ";C$ : GOTO 12210
13030 IF Objekt=0 THEN GOTO 13080
13034 IF Pryl(Objekt)<>1 THEN GOTO 6430
13036 IF Objekt=2 OR Objekt=22 OR Objekt=5 THEN PRINT "En kraftig stank gör att du inte kan." : GOTO 12210
13038 IF Objekt=8 OR Objekt=9 THEN PRINT "Du får bara damm i munnen." : GOTO 12210
13040 IF Objekt=10 THEN PRINT "Du mumsar i dej ett lagerblad. Det smakar så starkt att du inte vill äta mer." : GOTO 12210
13042 IF Objekt<>13 THEN GOTO 13060
13044 IF Varde(13)=0 THEN GOTO 13050
13046 Skrivrad 1, 516,517 : Varde(13)=0 : GOTO 12210
13050 PRINT "Du mumsar i dej ";SAK$(Objekt,0);"."; : Sov "äter",5
13052 PRINT : PRINT "Tack, det var gott!"
13055 Flagga(1)=Flagga(1)-1 : Pryl(Objekt)=0 : GOTO 12210
13060 IF Objekt=23 OR Objekt=30 THEN GOTO 13050
13062 IF Objekt=14 THEN PRINT "Du tar och äter en nypa alger och känner dej genast tio år yngre." : GOTO 12210
13064 IF Objekt=32 OR Objekt=29 THEN PRINT "Kannibal!" : GOTO 12210
13066 IF Objekt<>33 THEN GOTO 13075
13068 PRINT "Du stoppar en natriumpollett i munnen och den börjar fräsa."
13069 IF RND(1)<0.4 THEN PRINT "Mmmm, apelsinsmak!" ELSE IF RND(1)<0.5 THEN PRINT "Aaah, citronsmak!" ELSE PRINT "Bläh! Den smakar ricinolja!"
13070 GOTO 7071
13075 PRINT "Du biter i ";SAK$(Objekt,0);" som håller till skillnad från din tand." : GOTO 12210
13080 IF C$="" THEN C$=LASIN$("Ät vadå? ") : Objekt=Testasak(C$) : GOTO 13030
13082 PRINT "Jag vill inte äta ";C$;"." : GOTO 12210
25045 IF A$="ÖPPNA" THEN A$=LASIN$("Öppna vad? ")
27050 REM XXX TELEVERKET - subrutin för jackmontering XXX
27060 D=INT(RND(1)*Jack(0))+1
27075 IF Jack(D)<2 THEN GOTO 27050
27080 Jack(D)=1
27085 GOTO 32600
27100 Skrivrad 1, 71, 73
27122 IF Jack(Z)<>1 THEN GOTO 27140 ELSE Jack(Z)=2
27130 PRINT "Med en sur min skruvar han bort telefonjacken ur väggen och går."
27140 Poang=Poang+5
27150 Pryl(23)=Z : GOTO 12210
27200 REM XXXXX RING PERSONALKÖK XXXX
27204 PRINT : PRINT "TUUT ------ TUUT ----- TUUT ------ <klick>"
27206 IF Namn$="" THEN Namn$=LASIN$("Hej, vem där? ")'Namninläsning
27212 Skrivrad 1, 75, 77 : PRINT Namn$; : Skrivrad 1, 78, 79
27222 Flagga(29)=1 : PRINT : X1=1 : GOTO 12999
27250 REM XXX Ring Televerket XXXXX
27254 PRINT
27256 PRINT "- Stugans televerk."
27258 IF Jasvar("Har ni klagomål på er linje? ") THEN GOTO 27300
27264 A$=LASIN$("Vilket nummer gäller det? ")
27268 PRINT "Ok. Vänta ett tag så ska jag kolla upp det."
27270 Vanta "TUUT",20
27272 IF Lika(1,A$,"481","") THEN GOTO 27280
27274 IF Lika(1,A$,"999","") THEN GOTO 27290
27275 IF Lika(1,A$,"100","") AND Jack(100)=0 THEN PRINT "Abonnemanget har upphört.<klick>" : GOTO 12210
27276 PRINT "Det är inget fel på den linjen."
27278 PRINT "<klick>" : Flagga(28)=2 : GOTO 12210
27280 IF Flagga(6)>0 THEN GOTO 27284
27282 PRINT "Linjen fungerar utmärkt. (För en gångs skull...)" : GOTO 12210
27284 Skrivrad 1, 81, 83 : GOTO 12210
27290 PRINT "Nummerändring. Nya numret är 900." : GOTO 12210
27300 PRINT "Jag ska skicka någon för att fixa det."
27302 PRINT "<klick>" : PRINT
27304 PRINT "Ur skuggorna kommer plötsligt en man klädd i en röd"
27306 GOTO 7081
27400 REM XXX Ring Larmcentralen. XXX
27404 PRINT : PRINT "Larmcentralen, var god dröj."
27406 Vanta " Var god dröj  ",20
27408 PRINT "LARMCENTRALEN. Vi fixar allt - snabbt!"
27410 PRINT "Vad vill du ha hjälp med";
27412 A$=LASIN$("? ") : PRINT "Det går inte."
27416 IF Lika(1,A$,"RÖVAREN","") THEN GOTO 27428
27418 IF Lika(1,A$,"TRAPPAN","TRAPPORNA") THEN GOTO 27434
27420 IF Lika(1,A$,"HISSEN","4") THEN GOTO 27440
27424 PRINT "<klick>" : GOTO 12210
27428 Skrivrad 1, 85, 86 : Flagga(3)=-1 : GOTO 27424
27434 Skrivrad 1, 88, 89 : Flagga(15)=0 : Flagga(17)=0 : Flagga(18)=0 : GOTO 27424
27440 PRINT "Vänta, var det hissen du sa? Jag får väl se över den då."
27442 Flagga(40)=4 : Flagga(41)=0 : GOTO 27424
27600 IF Jack(100)<>1 THEN GOTO 12826 ELSE Sov "ringer",2
27602 IF Z=100 THEN I=1 ELSE I=5
27604 PLAY "T120O2L4G#P8G#.P2P4G#P8G#.P2P4G#P8G#.P2P4G#P8G#.P2P4"
27606 PRINT "Ingen svarar."
27608 GOTO 12210
27610 PLAY "T120O2L4G#.P4G#.P4G#.P4G#.P4G#.P4G#.P4G#.P4G#.P4G#.P4G#.P4G#.P4G#.P4G#.P4"
27612 PRINT "Det är visst upptaget."
27614 GOTO 12210
27616 GOTO 12210
27620 Skrivrad 1, 91, 95 : GOTO 12210
27630 ON Flagga(6)+1 GOTO 27632,27640,27650,27650
27632 IF Pryl(29)<>58 THEN GOTO 27652
27633 IF Z=58 THEN PRINT "Du hör en signal. Vakten går bort ett ögonblick."
27636 PRINT " - Stör mej inte! Jag vaktar!"
27638 GOTO 27424
27640 IF (Pryl(29)=58 OR Pryl(29)=1) AND Z=58 THEN PRINT "Du hör en signal. Vakten kravlar iväg."
27641 IF Pryl(29)<>58 THEN GOTO 27652
27644 PRINT " - Hick, HELAN GÅÅÅÅÅÅÅÅR... HICK!"
27646 GOTO 27424
27650 IF Z=58 THEN PRINT "Du hör en signal."
27652 I=4 : GOTO 27604
28000 REM XXX VAKT XXXXX
28002 IF Pryl(29)=Z OR Pryl(29)=1 THEN PRINT "Vakten sover för djupt." : GOTO 12210
28010 IF Pryl(29)<>Z AND Pryl(29)<>1 THEN Objekt=29 : GOTO 6430
28012 IF Flagga(6)=2 THEN PRINT "Vakten är redan död! Ser du inte blodfläckarna!" : GOTO 12210
28014 IF Pryl(4)<>1 THEN PRINT "Du har inget du kan döda honom med." : GOTO 12210
28016 IF Flagga(6)=3 THEN GOTO 28030
28018 IF Flagga(6)=1 THEN GOTO 28026
28020 PRINT "Du kastar hillebarden mot vakten, men han duckar."
28022 Pryl(4)=Z : Flagga(1)=Flagga(1)-1
28024 GOTO 12210
28026 Skrivrad 1, 97, 98 : Pryl(4)=2 : Flagga(1)=Flagga(1)-1
28029 GOTO 12210
28030 Skrivrad 1, 519,520  : Flagga(6)=2 : Pryl(22)=Z : Pryl(29)=Z : Flagga(51)=0
28039 IF Z=63 THEN Flagga(52)=Flagga(50) : Poang=Poang+25
28040 IF Pryl(31)=2 THEN Pryl(31)=Z
28042 IF Pryl(25)=2 THEN Pryl(25)=Z
28044 IF Pryl(26)=2 THEN Pryl(26)=Z
28046 X1=1 : GOTO 12999
28090 IF C$="" THEN GOTO 12999
28092 IF Lika(0,C$,"UPP","") THEN GOTO 9950
28100 Objekt=Testasak(C$)
28105 IF (Pryl(29)<>1 AND Pryl(29)<>Z) OR Flagga(6)>1 THEN GOTO 12840
28106 IF Lika(1,C$,"GUBBEN","") OR Flagga(30)=Z THEN GOTO 12840
28107 IF Objekt>0 THEN GOTO 28110
28108 PRINT "Det kan du inte ge till vakten." : GOTO 12210
28110 IF Pryl(Objekt)<>1 THEN GOTO 7034
28112 IF Objekt=13 THEN PRINT "- Behåll den du. Jag vill inte ha den!" : GOTO 12210
28114 IF Objekt=18 THEN GOTO 12360
28116 PRINT "Vakten tar emot ";SAK$(Objekt,0);" med ett snett leende."
28118 Flagga(1)=Flagga(1)-1 : Pryl(Objekt)=2 : GOTO 12210
28130 IF Flagga(6)=3 THEN GOTO 28150
28132 IF Objekt=13 THEN Poang=Poang-8 : Skrivrad 1, 580, 601 : GOTO 28144
28134 IF Flagga(6)=1 THEN GOTO 28140
28136 PRINT "Vakten hindrar dej." : GOTO 12210
28140 PRINT "Vakten släpper motvilligt ";SAK$(Objekt,0);"."
28144 Flagga(1)=Flagga(1)+1 : Pryl(Objekt)=1
28146 GOTO 12210
28150 PRINT "Har du hjärta att ta någonting från en sovande vakt?!?"
28152 A$=LASIN$("")
28154 IF A$<>"JA" THEN GOTO 12214
28156 PRINT "Har du inget hjärta i kroppen?!! Jag vägrar!" : GOTO 12214
28160 IF Pryl(22)<>63 THEN GOTO 6430
28164 Flagga(52)=0 : Poang=Poang-30
28166 GOTO 6420
30000 REM XXX GUBBE XXXX
30001 IF Flagga(30)=96 OR Flagga(49)=1 THEN GOTO 32600
30002 Flagga(30)=INT(RND(1)*92)+9 : Flagga(49)=0 : X1=2
30004 IF Flagga(30)=Z OR Flagga(30)=51 OR Flagga(30)=60 THEN GOTO 30002
30006 Skrivrad 1, 102, 104 : GOTO 32600
30010 IF Flagga(30)<>Z THEN GOTO 30060
30012 IF Pryl(19)=0 THEN PRINT "Gubben har ju vattenflaskan." : GOTO 12210
30014 IF Pryl(19)<>1 OR Flagga(32)>0 THEN PRINT "Du har ju ingen full vattenflaska." : GOTO 12210
30016 PRINT "Gubben dricker ur vattenflaskan och ser genast gladare ut."
30018 Flagga(49)=1 : Flagga(32)=1 : GOTO 12210
30020 IF Objekt=11 AND Flagga(30)=Z AND Flagga(49)=2 THEN GOTO 30030
30022 IF Objekt=19 AND Pryl(10)=0 THEN GOTO 30038 ELSE GOTO 6430
30030 PRINT "Du tar pärlhalsbandet från den bländade gubben."
30032 Flagga(49)=0 : Pryl(11)=1 : X1=1 : Flagga(1)=Flagga(1)+1
30034 GOTO 30002
30038 PRINT "Du tar vattenflaskan från gubben."
30040 Flagga(1)=Flagga(1)+1 : X1=1 : Pryl(19)=1
30042 GOTO 30002
30050 IF Flagga(30)<>Z THEN GOTO 30060
30052 PRINT "Gubben ser din hotande blick och smiter iväg."
30054 Flagga(30)=INT(RND(1)*92)+9 : Flagga(49)=0
30056 IF Flagga(30)=Z OR Flagga(30)=51 OR Flagga(30)=60 THEN GOTO 30054
30058 GOTO 12210
30060 PRINT "Jag ser ingen gubbe här." : GOTO 12210
31000 REM *** SPARA *** &&&&& DEC-10 SPECIELL KOD PÅ 31000-31565
31002 R$=LASIN$("Vilken skivenhet ska jag använda? (A:, B: eller C:) ")
31003 IF R$<>"" AND RIGHT$(R$,1)<>":" THEN R$=R$+":"
31004 R$=R$+"STUGA.STA"
31005 ON ERROR GOTO 31500'&&&&&
31100 OPEN R$ FOR OUTPUT AS #1'&&&&&
31102 ON ERROR GOSUB 32450'&&&&&
31105 KOD=0'&&&&&
31107 PRINT #1,Version
31110 FOR I=0 TO Pryl(0)'&&&&&
31112   PRINT #1,Pryl(I)'&&&&&
31113   IF I<=Varde(0) THEN PRINT #1,Varde(I) : KOD=KOD+Varde(I)/(PI+2)'&&&&&
31115   KOD=KOD+Pryl(I)/(PI)'&&&&&
31117 NEXT I'&&&&&
31125 FOR I=0 TO Flagga(0)'&&&&&
31126   PRINT #1,Flagga(I)'&&&&&
31127   KOD=KOD+Flagga(I)/(PI-1)'&&&&&
31128 NEXT I'&&&&&
31131 PRINT #1,STR$(G*2);",";STR$(Poang*9);",";STR$(Z*7);",";STR$(KOD+Poang+G+Z)'&&&&&
31135 IF Namn$="" THEN PRINT #1,"-----" ELSE PRINT #1,Namn$'&&&&&
31140 FOR I=0 TO Jack(0)'&&&&&
31142   PRINT #1,Jack(I)'&&&&&
31144 NEXT I'&&&&&
31150 CLOSE 1'&&&&&
31155 PRINT "Det nuvarande läget är sparat på filen ";R$;"."'&&&&&
31160 GOTO 12210'&&&&&
31200 REM *** ÅTERSKAPA ***'&&&&&
31202 R$=LASIN$("Vilken skivenhet ska jag använda? (A:, B: eller C:) ")
31203 IF R$<>"" AND RIGHT$(R$,1)<>":" THEN R$=R$+":"
31204 R$=R$+"STUGA.STA"
31205 ON ERROR GOTO 31500
31207 OPEN R$ FOR INPUT AS #1'&&&&&
31210 ON ERROR GOTO 31520'&&&&&
31300 KOD=0'&&&&&
31303 INPUT #1,A1%
31305 INPUT #1,Pryl(0)'&&&&&
31306 INPUT #1,Varde(0)
31307 KOD=Pryl(0)/(PI)+Varde(0)/(PI+2)
31310 FOR I=1 TO Pryl(0)'&&&&&
31312   INPUT #1,Pryl(I)'&&&&&
31313   IF I<=Varde(0) THEN INPUT #1,Varde(I) : KOD=KOD+Varde(I)/(PI+2)'&&&&&
31315   KOD=KOD+Pryl(I)/(PI)'&&&&&
31317 NEXT I'&&&&&
31318 INPUT #1,Flagga(0)'&&&&&
31319 KOD=KOD+Flagga(0)/(PI-1)'&&&&&
31320 FOR I=1 TO Flagga(0)'&&&&&
31322   INPUT #1,Flagga(I)'&&&&&
31325   KOD=KOD+Flagga(I)/(PI-1)'&&&&&
31327 NEXT I'&&&&&
31329 INPUT #1,G,Poang,Z,KOD2'&&&&&
31331 LINE INPUT #1,Namn$'&&&&&
31332 IF RIGHT$(Namn$,5)="-----" THEN Namn$=""'&&&&&
31336 INPUT #1,Jack(0)'&&&&&
31337 FOR I=1 TO Jack(0)'&&&&&
31338   INPUT #1,Jack(I)'&&&&&
31339 NEXT I'&&&&&
31340 G=G/2 : Z=Z/7 : Poang=Poang/9'&&&&&
31341 KOD=KOD+G+Poang+Z'&&&&&
31342 CLOSE 1'&&&&&
31343 ON ERROR GOSUB 32450'&&&&&
31345 IF ABS(KOD-KOD2)>0.03 THEN PRINT "Fel på ";R$ : STOP'&&&&&
31350 IF Z<>4 AND (Z<8 OR Z>100) THEN PRINT "Fel i ";R$ : STOP'&&&&&
31355 X=99 : GOTO 32600
31500 PRINT "? Kan inte öppna ";R$;"."'&&&&&
31510 ON ERROR GOSUB 32450'&&&&&
31515 X1=1 : GOTO 32600
31520 PRINT "Fel inuti ";R$'&&&&&
31525 CLOSE 1'&&&&&
31530 STOP'&&&&&
32450 BEEP : PRINT "Thorvald kommer och ber om ursäkt för fel nr";ERR;"."
32455 PRINT "Han gör en magisk gest och försvinner i ett gult moln."
32460 RETURN
32500 X=100
32600 RESULT=X
END FUNCTION

PROCEDURE: Thorvald
EXTERNAL: Skrivrad
700 REM ********************** THORVALD: ******************************
701 IF Stora=0 AND RND(1)<0.8 THEN GOTO 702 ELSE GOTO 705
702 Skrivrad 1, 126, 131
703 GOTO 710
705 Skrivrad 1, 107, 125
710 REM
END PROCEDURE

PROCEDURE: Felmeddelande
EXTERNAL: Flagga(),A$,X,D,X1,Z,Lika
11000 REM VIOLS SUBFELMEDDELANDERUTIN 3
11001 IF X1>0 THEN GOTO 11100 ELSE Flagga(50)=Flagga(50)-1
11002 IF INSTR(1,A$,"HJÄLP")>0 THEN PRINT "Du kan inte få någon hjälp här." : GOTO 11100
11003 IF INSTR(1,"*N*V*U*NERÅT*NEDÅT*UPPÅT","*"+A$)>0 THEN PRINT "Du kan inte gå ditåt." : GOTO 11100
11004 IF INSTR(1,"*NORR*SÖDER*VÄSTER*ÖSTER*NV*NÖ*NO*SV*SÖ*SO","*"+A$)>0 THEN GOTO 11200
11005 IF INSTR(1,"*NORDVÄST*NORDÖST*NORDOST*SYDVÄST*SYDÖST*SYDOST","*"+A$)>0 THEN GOTO 11200
11006 IF INSTR(1,"*UPPÅT*NEDÅT*NERÅT*VÄNSTER*HÖGER*FRAMÅT*BAKÅT","*"+A$)>0 THEN GOTO 11220
11007 IF Lika(1,A$,"SESAM","")=0 AND Lika(1,A$,"KORKSKRUV","9")=0 THEN GOTO 11011
11008 PRINT "Ingenting händer."
11009 GOTO 11100
11011 IF Lika(1,A$,"STÄNG","") THEN PRINT "Det finns inget du kan stänga här!" : GOTO 11100
11012 IF (Lika(1,A$,"TRYCK","") OR A$="HIT") AND Z>20 AND Z<30 THEN PRINT "Gå fram till hissdörren först!" : GOTO 11100
11013 IF Lika(1,A$,"KROSSA","") THEN PRINT "Det finns inget du kan krossa här!" : GOTO 11100
11014 IF Lika(1,A$,"SKÄR","") THEN PRINT "Det finns inget du kan skära här!" : GOTO 11100
11015 IF Lika(0,A$,"SE","TITTA") THEN GOTO 11250
11017 IF Lika(0,A$,"HOPPA","") AND X=1 THEN PRINT "Du kommer ingenstans uppåt." : GOTO 11100
11018 IF Lika(0,A$,"HOPPA","") THEN PRINT "Det finns inget hål att hoppa ner genom." : GOTO 11100
11020 IF Lika(0,A$,"ÖPPNA","") THEN PRINT "Det finns inget du kan öppna här." : GOTO 11100
11022 IF Lika(0,A$,"STÄNG","") THEN PRINT "Det finns inget du kan stänga här." : GOTO 11100
11080 D=INT(RND(1)*5)+1
11081 IF D=1 THEN PRINT "Va??"
11082 IF D=2 THEN PRINT "Jag förstår inte."
11083 IF D=3 THEN PRINT "Det förstår jag inte alls."
11084 IF D=4 THEN PRINT "Det vet jag inte vad det betyder."
11085 IF D=5 THEN PRINT "Uttryck Dej klarare."
11099 PRINT
11100 GOTO 11250
11200 IF Flagga(36)=0 THEN PRINT "Inomhus ska du ange riktningar, inte väderstreck."
11202 IF Flagga(36)<>0 THEN PRINT "Du kan inte gå ditåt."
11205 GOTO 11100
11220 IF Flagga(36)=1 THEN PRINT "Utomhus ska du ange väderstreck, inte riktningar."
11225 IF Flagga(36)<>1 THEN PRINT "Du kan inte gå ditåt."
11230 GOTO 11100
11250 REM Slut på subfelmeddelanderutinen
END PROCEDURE

PROCEDURE: Underbyggnad
EXTERNAL: Skrivrad
730 REM******************OLLES SUBRUTIN****************************
731 Skrivrad 1, 151, 156
END PROCEDURE

PROCEDURE: Rumsbeskrivning
EXTERNAL: Flagga(),Pryl(),Jack(),I,Z,A1,B,Sov
EXTERNAL: Varde(),SAK$(),Skrivrad
6000 REM Rumsbeskrivningsrutin
6003 IF Z=70 AND A1=1 THEN PRINT "På marken ligger en enorm rubin."
6005 IF Pryl(1)=Z THEN PRINT "Det finns en diamant här."
6006 IF Pryl(31)=Z THEN PRINT "Det ligger en kofot här."
6007 IF Pryl(16)=Z THEN PRINT "Det står en cykelpump här."
6008 IF Pryl(2)=Z THEN PRINT "Det finns en illaluktande gurka här."
6009 IF Pryl(3)=Z THEN PRINT "Det finns en silvertacka här."
6010 IF Pryl(17)=Z AND Z<>11 THEN PRINT "Det står en stege här."
6011 IF Pryl(18)<>Z THEN GOTO 6014
6012 IF Flagga(31)=0 THEN PRINT "Det finns en fylld brännvinsflaska här."
6013 IF Flagga(31)=1 THEN PRINT "Det finns en tom brännvinsflaska här."
6014 IF Pryl(19)<>Z THEN GOTO 6017
6015 IF Flagga(32)=0 THEN PRINT "Det finns en fylld vattenflaska här."
6016 IF Flagga(32)=1 THEN PRINT "Det finns en tom vattenflaska här."
6017 IF Pryl(20)<>Z THEN GOTO 6020
6018 IF Flagga(33)=0 THEN PRINT "Det finns en pumpad boll här."
6019 IF Flagga(33)=1 THEN PRINT "Det finns en opumpad boll här."
6020 IF Pryl(4)=Z THEN PRINT "Det står en hillebard här."
6021 IF Pryl(21)=Z THEN PRINT "Det står en spade här."
6022 IF Pryl(5)=Z THEN PRINT "Det ligger en döskalle här."
6023 IF Pryl(6)=Z THEN PRINT "Det finns en väckarklocka här."
6024 IF Pryl(23)=Z THEN PRINT "Det ligger en tunn telefonkatalog här."
6025 IF Pryl(12)=Z THEN PRINT "Det ligger en faunsko här."
6026 IF Pryl(7)=Z THEN PRINT "Det finns en massa guldmynt här."
6027 IF Jack(Z)=1 AND Pryl(25)=Z THEN PRINT "En telefon är inkopplad i en jack i väggen."
6028 IF Jack(Z)=1 AND Pryl(25)<>Z AND Pryl(30)<>Z THEN PRINT "Det finns en telefonjack i väggen."
6029 IF Jack(Z)<>1 AND Pryl(25)=Z AND Flagga(44)<>Z THEN PRINT "Det finns en telefon här."
6030 IF Pryl(26)=Z THEN PRINT "Det finns några nycklar här."
6031 IF Pryl(27)=Z THEN PRINT "Det hänger en sax här."
6032 IF Pryl(28)=Z THEN PRINT "Det hänger en slägga här."
6033 IF Pryl(22)=Z THEN PRINT "Det ligger ett äckligt lik här."
6034 IF Pryl(11)=Z THEN PRINT "Det finns ett glittrande pärlhalsband här."
6035 IF Pryl(8)=Z THEN PRINT "Det ligger en bibel här."
6036 IF Pryl(24)=Z THEN PRINT "Det står en lampa här."
6037 IF Pryl(9)=Z THEN PRINT "Det ligger ett kontrakt här."
6038 IF Pryl(10)=Z THEN PRINT "Det hänger en lagerkrans här."
6039 IF Flagga(30)=Z THEN GOTO 6200
6040 IF Pryl(13)=Z AND Varde(13)=0 THEN PRINT "Det finns en medalj av mörk choklad här."
6041 IF Pryl(13)=Z AND Varde(13)>0 THEN PRINT "Det finns en ärorik medalj här."
6042 IF Pryl(14)=Z THEN PRINT "Det finns gröna alger av släktet Cyanophyceae här."
6043 IF Pryl(15)=Z AND Varde(15)=0 THEN PRINT "Bitarna av en sönderslagen spegel ligger här."
6044 IF Pryl(15)=Z AND Varde(15)>0 THEN PRINT "På väggen hänger en mycket skör spegel."
6045 IF Pryl(32)<>Z THEN GOTO 6050
6046 IF Z>65 AND Z<88 AND Z<>78 THEN PRINT "I ett träd hänger"; ELSE PRINT "Här ligger";
6047 PRINT " en sengångare och slumrar."
6048 IF Pryl(14)=0 THEN PRINT "Den har sovit så länge att det börjat växa alger i dess päls."
6050 IF Pryl(33)=Z THEN PRINT "Här ligger";Flagga(16);"oansenliga natriumpolletter."
6054 IF Jack(Z)=1 AND Pryl(30)=Z THEN PRINT "En förlängningssladd är inkopplad i en jack i väggen."
6055 IF Jack(Z)<>1 AND Pryl(30)=Z THEN PRINT "Det ligger en förlängningssladd här."
6056 IF Jack(Z)<>1 AND Flagga(44)=Z AND Pryl(25)=Z THEN PRINT "En telefon är inkopplad i en förlängningssladd."
6057 IF Flagga(44)=Z AND (Pryl(25)<>Z OR Jack(Z)=1) THEN PRINT "En förlängningssladd sticker in hit."
6058 IF Flagga(44)=-1 AND Z<>Pryl(30) THEN Flagga(44)=Z : PRINT "Förlängningssladden räcker precis hit."
6059 IF Flagga(3)>0 THEN GOTO 29000
6060 IF INT(RND(1)*20)<>1 OR Flagga(50)<50 THEN GOTO 6069
6061 FOR I=1 TO Varde(0)
6062   IF Pryl(I)=1 AND Varde(I)>0 THEN Flagga(3)=Flagga(3)+1
6063 NEXT I
6064 IF Flagga(3)>0 THEN Flagga(4)=INT(RND(1)*6)+INT(Flagga(3)/2+0.5)
6069 IF INT(RND(1)*40)<>1 THEN GOTO 6080
6070 IF Flagga(48)>0 THEN PRINT "En glasmästare springer förbi dej." : Flagga(48)=0
6072 IF Flagga(15)<>1 AND Flagga(17)<>1 AND Flagga(18)<>1 THEN GOTO 6076
6074 PRINT "En verkmästare från Stugans gatukontor lunkar förbi dej." : Flagga(15)=Flagga(17)=Flagga(18)=0
6076 IF Flagga(41)=1 THEN PRINT "En hissreparatör går förbi dej." : Flagga(41)=0 : Flagga(40)=INT(RND(1)*9)+1
6080 IF Flagga(50)-Flagga(24)=10 AND Pryl(32)=1 THEN PRINT "Du känner dej plötsligt väldigt trött."
6082 IF Flagga(24)=0 OR Flagga(50)-Flagga(24)<15 OR INT(Flagga(50)/3)*3<>Flagga(50) OR Pryl(32)<>1 THEN GOTO 6098
6083 PRINT "Tröttheten överväldigar dej och du somnar.";
6084 Sov "sover",10 : PRINT : PRINT "Du vaknar och gnuggar ögonen."
6085 IF Flagga(21)>0 THEN GOTO 6130
6098 IF Flagga(50)-Flagga(21)>25 AND Flagga(21)>0 THEN GOTO 6130
6100 IF Pryl(29)<>Z THEN GOTO 6120
6102 IF Flagga(50)-Flagga(51)>30 AND Flagga(51)>0 THEN GOTO 6124
6104 IF Flagga(6)=0 THEN PRINT "Här står en vakt."
6106 IF Flagga(6)=1 THEN PRINT "En full vakt raglar omkring här."
6108 IF Flagga(6)=2 THEN PRINT "Det finns blodfläckar på golvet." : GOTO 32000
6110 IF Flagga(6)=3 THEN PRINT "En vakt sover här."
6112 FOR I=1 TO Pryl(0)
6113   IF I=4 AND Pryl(4)=2 THEN PRINT "Han håller i en juvelprydd hillebard." : GOTO 6118
6114   IF I=13 AND Pryl(13)=2 THEN PRINT "På bröstet bär han en medalj." : GOTO 6118
6116   IF Pryl(I)=2 THEN PRINT "Han bär på ";SAK$(I,0);"."
6118 NEXT I
6119 GOTO 32000
6120 IF Pryl(29)<>1 THEN GOTO 32000
6122 IF Flagga(50)-Flagga(51)<31 AND Flagga(51)>0 THEN PRINT "Du följs av en full vakt." : GOTO 6112
6123 Flagga(1)=Flagga(1)-1
6124 PRINT "Vakten har nyktrat till nu."
6126 IF Flagga(6)=3 THEN PRINT "Han vaknar och sträcker på sej."
6128 Pryl(29)=Z : Flagga(6)=0 : Flagga(51)=0
6129 GOTO 32000
6130 Flagga(21)=0
6131 PRINT"Dina fötter är läkta nu."
6132 GOTO 6100
6200 IF Pryl(19)>0 AND Pryl(11)>0 THEN PRINT "Här sitter en gubbe." : GOTO 6210
6202 PRINT "Här sitter en gubbe med ett pärlhalsband runt halsen."
6204 IF Pryl(19)=0 THEN PRINT "I armarna har han en vattenflaska."
6206 IF Pryl(19)=0 AND Pryl(10)=0 THEN PRINT "På huvudet bär han din lagerkrans."
6208 IF Pryl(19)=0 AND Pryl(10)=1 THEN PRINT "Han stirrar på din lagerkrans."
6210 PRINT  : GOTO 6040
29000 REM XXX RÖVARE XXXX
29005 Flagga(4)=Flagga(4)+1
29010 IF Flagga(4)>8 THEN GOTO 29050
29015 IF RND(1)<0.2 THEN Flagga(4)=Flagga(3)=0 : GOTO 6069
29020 IF RND(1)<0.7 THEN PRINT "Du hör tunga fotsteg i närheten."
29025 GOTO 6069
29050 IF Z=80 THEN GOTO 6069
29055 B=0
29060 IF INT(RND(1)*4)=3 THEN Flagga(3)=0 : Flagga(41)=1 : GOTO 6069
29065 FOR I=1 TO Varde(0)
29070   IF (Pryl(I)<>1 AND Pryl(I)<>Z) OR Varde(I)=0 THEN GOTO 29080
29075   IF Pryl(I)=1 THEN Flagga(1)=Flagga(1)-1
29077   Pryl(I)=80 : B=B+1
29080 NEXT I
29085 IF B=0 THEN GOTO 6069
29090 IF Flagga(20)=1 THEN Flagga(20)=0
29095 PRINT
29100 Skrivrad 1, 158, 161 : Flagga(4)=-7 : Flagga(3)=-1 : GOTO 6069
32000 REM Slut på rumsbeskrivningsrutinen
END PROCEDURE

PROCEDURE: Telefon
10 PLAY "MBMLT180O3L64CACACACACACACACACACAP4CACACACACACACACACACACACAP1"
20 PLAY "CACACACACACACACACACAP4CACACACACACACACACACACACAP1MN"
END PROCEDURE

INTERRUPT PROCEDURE: Itelefon
10 PLAY "MLT180O3L64CACACACACACACACACACAP4CACACACACACACACACACACACAP1"
20 PLAY "CACACACACACACACACACAP4CACACACACACACACACACACACAP1MN"
END PROCEDURE

PROCEDURE: Telefonsignal
10 PLAY "T120O2L4G#P8G#.P2P4G#P8G#."
END PROCEDURE

INTERRUPT PROCEDURE: Poangsiffra
EXTERNAL: Poang
10 PRINT CHR$(8);" "
20 PRINT "Just nu har du";Poang;"poäng." : PRINT
END PROCEDURE

PROCEDURE: Faunsang
10 PLAY "MNMBT65O2L4B.L16B-L24AA-GL8G-.L16A-AB-L8B>L7C#L6F#L3D#"
END PROCEDURE

PROCEDURE: Piratsang
10 PLAY "MFT170O2L8GL4GL8GGFEL4>C<L8GL4GL8GL4AL8AL4>C<L8AL2GP8"
20 PLAY "L8EL4FL8FL4GL8FEFGAB>C<L4EL8EL4DL8DL4C."
END PROCEDURE

PROCEDURE: Skrivrad
INTEGER: I
STRING: Rad[80],C$[?]
EXTERNAL: Kryptera
5 IF Borja=Sluta THEN GOTO 60
10 FOR I=Borja TO Sluta
30   READ RECORD 4,I,Rad
35   IF I>500 THEN PRINT Kryptera(Rad)
40   IF I<500 THEN PRINT Rad
50 NEXT I
55 GOTO 100
60 READ RECORD 4,Borja,Rad
65 IF I>500 THEN PRINT Kryptera(Rad)
70 IF I<500 THEN PRINT Rad
100 'SLUT PÅ skrivradsproceduren
END PROCEDURE

STRING FUNCTION: Kryptera
BYTE: I
STRING: Utstr[80]
10 Utstr=SPACE$(LEN(Rad$))
20 FOR I=1 TO LEN(Rad$)
30   MID$(Utstr,I,1)=CHR$(ASC(MID$(Rad$,I,1))-100)
40 NEXT I
50 RESULT=Utstr
END FUNCTION

INTEGER FUNCTION: Jasvar
EXTERNAL: Lasin$,A1$
10 A1$=Lasin$(Ledtext$)
20 IF A1$="JA" THEN RESULT=1 : GOTO 50
30 IF A1$<>"NEJ" THEN PRINT "Svara JA eller NEJ!" : PRINT : GOTO 10
40 RESULT=0
50 REM 0=nej, 1=ja
END FUNCTION

INTEGER FUNCTION: Lika
STRING: B$[80],E$[16]
INTEGER: C,D,A
REAL: I
5 IF A$="" THEN RESULT=0 : GOTO 1010
10 D=0 : IF LEN(Ord1$)>16 THEN E$=LEFT$(Ord1$,16) ELSE E$=Ord1$
20 IF Ord2$<>"" THEN IF ASC(Ord2$)>48 AND ASC(Ord2$)<58 THEN D=VAL(Ord2$)
30 IF Inne THEN GOTO 500
35 A=INSTR(A$," ")
40 IF A>0 THEN B$=LEFT$(A$,A-1) ELSE B$=A$
50 IF B$="" THEN GOTO 900
60 IF B$=E$ THEN GOTO 1000
70 A=LEN(B$)
100 IF (A<5 AND D<=1) OR A>LEN(E$) THEN GOTO 900
110 IF D>1 AND A<D THEN GOTO 900
120 IF B$=LEFT$(E$,A) THEN GOTO 1000 ELSE GOTO 900
500 IF D>1 THEN A=D+1 ELSE IF LEN(E$)>5 THEN A=6 ELSE A=LEN(E$)+1
510 B$=" "+LEFT$(A$,78)+" " : E$=" "+E$
520 FOR I=LEN(E$) TO A STEP -1
530   IF INSTR(B$,LEFT$(E$,I)+" ")>0 THEN EXIT
540 NEXT I
550 IF I>=A THEN RESULT=INSTR(B$,LEFT$(E$,I)+" ") : GOTO 1010
900 IF D>0 THEN RESULT=0 : GOTO 1010
910 D=1 : IF LEN(Ord2$)>16 THEN E$=LEFT$(Ord2$,16) ELSE E$=Ord2$
920 IF Inne THEN GOTO 500 ELSE GOTO 60
1000 RESULT=1
1010 REM 0=inte lika, >0=lika
END FUNCTION

'MAIN Program:


1 'Version: 2.01/PC
2 Version=2
3 GOTO 32000
4 '-------- IBM-version påbörjad 860508 ---------
5 'Midsommarversion (2.01) skickad 86-06-15
9 '************************  S T U G A  *****************************
10 '******* STUGA är skrivet av Viggo Eriksson, Kimmo Eriksson *******
15 '******* och Olle Johansson. Adressen till programmakarna   *******
20 '******* är Solängsvägen 170, 191 54 SOLLENTUNA.            *******
25 '******* Denna fil är hemlig och får inte spridas ut utan   *******
30 '******* författarnas tillstånd.                  810321    *******
35 '******************************************************************
55 'Rader märkta med %%%%% får bara finnas med på Oden och Nadja
60 'Rader märkta med &&&&& använder DEC-10-BASIC-filhantering
1499 Felmeddelande
1500 Z=53 : Flagga(25)=Flagga(25)+1'XXXXX VIGGOS ATELJE XXXXX
1503 IF Flagga(25)>2 AND Flagga(25)<8 THEN PRINT "Du är i Ateljen." : GOTO 1511
1504 Skrivrad 1, 163, 164
1511 IF Flagga(25)>8 THEN Flagga(25)=4
1512 GOSUB 12200
1517 IF X1=1 THEN GOTO 1504
1518 IF X=0 THEN GOTO 1499
1520 ON X GOTO 1499,1499,15000,15050,1499,1540,1530
1530 IF Pryl(10)=53 THEN GOTO 1538
1532 PRINT "TIPS!! Prova orden och gå tillbaka hit." : Poang=Poang-10 : GOTO 1500
1538 PRINT "TIPS!! Ta lagerkransen och skriv HJÄLP igen."
1539 Poang=Poang-10 : GOTO 1500
1540 IF Pryl(1)<>1 THEN GOTO 9991
1542 PRINT "Dörren har gått i baklås så du kommer inte ut åt det hållet!"
1544 IF Pryl(26)=1 OR Pryl(26)=Z THEN PRINT "Dina nycklar passar inte i nyckelhålet."
1548 GOTO 1500
1908 Felmeddelande
1909 PRINT "Du är i en stor svängande labyrint."
1910 Z=34
1911 GOSUB 12200
1913 IF X=0 OR X>6 THEN GOTO 1908
1914 ON X GOTO 1970,1919,1939,8095,1950,1929
1918 Felmeddelande
1919 PRINT "Du är i en svängig stor labyrint."
1921 Z=92 : GOSUB 12200'XX KIVIS LABYRINTRUM 3 XXXX Z=92 XXX
1922 IF X=0 OR X>6 THEN GOTO 1918
1923 ON X GOTO 1929,1944,1960,1950,1909,8300
1928 Felmeddelande
1929 PRINT "Du är i en svängande stor labyrint."
1931 Z=89 : GOSUB 12200'XX KIVIS LABYRINTRUM 2 XXXX Z=89 XXX
1932 IF X=0 OR X>6 THEN GOTO 1928
1933 ON X GOTO 1960,1950,1939,1909,1970,1919
1939 ON (INT(RND(1)*2)+1) GOTO 16500,15432
1944 Skrivrad 1, 166, 170
1948 IF G=1 THEN PRINT "Han håller din halvruttna tomat i handen."
1949 Piratsang
1950 PRINT "Du är i en stor svängig labyrint."
1952 Z=93 : GOSUB 12200'XX KIVIS LABYRINTRUM 4 XXXX Z=93 XXX
1953 IF X=0 OR X>6 THEN GOTO 1956
1954 ON X GOTO 8000,1929,1960,1970,1944,1919
1956 Felmeddelande : GOTO 1950
1959 Felmeddelande
1960 PRINT "Du är i en stor labyrint som också är svängig."
1962 Z=94 : GOSUB 12200'XX KIVIS LABYRINTRUM 5 XXXX Z=94 XXX
1963 IF X=0 OR X>6 THEN GOTO 1959
1964 ON X GOTO 1919,8035,1970,1980,1929,1950
1969 Felmeddelande
1970 PRINT "Du är i en svängig labyrint som också är stor."
1972 Z=95 : GOSUB 12200'XX KIVIS LABYRINTRUM 6 XXXX Z=95 XXX
1973 IF X=0 OR X>6 THEN GOTO 1969
1974 ON X GOTO 1960,1980,8071,1950,1909,1929
1980 IF G=1 THEN GOTO 1970 ELSE PRINT "Du har en halvrutten tomat i handen."
1981 PRINT "HAR HAR HAR! ropar en pirat som springer mot dej."
1982 PRINT "Piraten tar din tomat."
1983 Piratsang : G=1 : GOTO 1970
2008 PRINT "Du är i Högra pannrummet."
2009 Flagga(42)=Flagga(42)+1
2010 IF Flagga(42)>1 THEN PRINT "Ett hål finns i väggen." : GOTO 2012
2011 PRINT "En panna sprängs och gör ett hål i väggen."
2012 IF Jasvar("Vill du gå in i hålet? ") THEN GOTO 2019
2015 PRINT "Ok."
2016 GOTO 14100
2018 Felmeddelande
2019 Z=69 'XXXX GROTTRUM 1 XXXXX Z=69 XXX
2020 PRINT "Du är i en grotta som sträcker sej utom synhåll åt vänster och höger."
2021 IF Flagga(42)>0 THEN PRINT "Bakom dej finns ett uppsprängt hål."
2023 GOSUB 20500
2024 IF X<3 THEN GOTO 2018 ELSE ON X-2 GOTO 2044,2032,2018,2025,2018
2025 IF Flagga(42)>0 THEN GOTO 14100 ELSE GOTO 2018
2032 PRINT "Du gick just genom ett vattenfall."
2033 Z=91'XXXXX GROTTRUM 6 XXX Z=91 XXX
2034 PRINT "Du är vid ett vattenfall i en skog."
2035 GOSUB 20500
2036 IF X=0 OR X>6 THEN GOTO 2038
2037 ON X GOTO 2019,2066,2115,2150,2066,2019
2038 Felmeddelande
2039 GOTO 2034
2043 Felmeddelande
2044 Z=19'XXXX GROTTRUM 2 XXX Z=19 XXX
2045 IF Pryl(12)<>0 THEN GOTO 2051 ELSE Faunsang
2046 Skrivrad 1, 172, 173
2049 Pryl(12)=19
2051 PRINT "Du är i Schweiziska klockrummet."
2052 GOSUB 20500
2054 IF X=0 THEN GOTO 2043
2056 ON X GOTO 2043,2043,2075,2150,2145,2019,2043
2065 Felmeddelande
2066 Z=33'XXXXX GROTTRUM 3 XXXXX
2067 PRINT "Du är på stranden till en underjordisk sjö."
2068 GOSUB 20500
2071 IF X=10 THEN GOTO 2107
2073 IF X<>0 THEN ON X GOTO 2101,2104,2075,20270,2107,2033,2065 ELSE GOTO 2065
2074 Felmeddelande
2075 Z=25 'XXX GROTTRUM 5 XXX Z=25 XXX
2076 PRINT "Du är på stranden till en underjordisk sjö bredvid en enorm spelautomat."
2077 PRINT "På den står det: 'DRA I SPAKEN OM DU HAR EN FAUNSKO ATT SATSA'"
2078 GOSUB 20500
2079 IF Lika(0,A$,"DRAG","3")=0 THEN GOTO 2089
2080 IF Pryl(12)=1 THEN GOTO 2083
2081 PRINT "FUSKARE! Du har ingen faunsko!"
2082 GOTO 2087
2083 D=INT(RND(1)*10)+1
2084 IF D>7 THEN GOTO 2094
2085 PRINT "Grattis! Du vann en massa guldmynt." : BEEP
2086 Pryl(12)=2 : Pryl(7)=25 : Flagga(1)=Flagga(1)-1
2087 PRINT "Du är vid slutet av stranden."
2088 GOSUB 20500
2089 IF X>0 AND X<7 THEN ON X GOTO 2074,2074,20255,2066,2107,2044
2090 IF X=10 THEN GOTO 2107 ELSE GOTO 2074
2094 PRINT "Tyvärr, du förlorade!"
2095 Flagga(1)=Flagga(1)-1 : Pryl(12)=0
2096 GOTO 2087
2101 PRINT "Du fastnade i en jättesugkopp och kan inte komma loss."; : SOV "sover",15
2102 PRINT "Hoppsan, nu svalt du ihjäl."
2103 GOTO 9461
2104 PRINT "Du sjunker...S J U N K E R!!"
2105 GOTO 9075
2107 PRINT "Vattnet är lugnt, du simmar fort." : PRINT SPACE$(40)
2108 GOTO 2138
2109 PRINT "Järndörrar slår ner omkring dej. Du kan bara gå uppåt."
2111 A$=LASIN$("")
2112 IF Lika(1,A$,"UPPÅT","3") OR A$="U" THEN GOTO 2135
2113 PRINT "Så kan du väl inte gå!"
2114 GOTO 2111
2115 Z=20 'XXX GROTTRUM 4 XXX Z=20 XXX
2116 PRINT "Du är fortfarande i skogen men åt ett håll skymtar man en stuga."
2118 GOSUB 20500
2119 IF X<>0 THEN ON X GOTO 2120,2120,20155,2120,2127,2033,2120
2120 Felmeddelande
2121 GOTO 2115
2123 PRINT "Du hoppar ner i brunnen och ramlar till slut ner på marken."
2124 GOTO 14000
2126 Felmeddelande
2127 Z=99 'XXXXX GROTTRUM 7 XXXX Z=99 XXXX
2128 PRINT "Du står utanför stugan vid en brunn."
2129 GOSUB 20500
2133 IF X=0 OR X>6 THEN GOTO 2126
2134 ON X GOTO 2126,2123,2126,2126,26000,2115
2135 PRINT "Du har kommit upp ur en brunn. Här finns en stuga."
2136 Z=99 : GOTO 2129
2138 D=INT(RND(1)*4)+1
2139 IF D=2 THEN GOTO 2109
2140 IF D>2 THEN GOTO 21600 ELSE PRINT "Du dras ner. Nu är du";
2143 GOTO 9450
2145 D=INT(RND(1)*5)+1
2146 IF D<4 THEN GOTO 2115
2147 PRINT "En hord fauner kommer framrusande. Nnnnnu är du en våt fläck."
2148 GOTO 9461
2149 Felmeddelande
2150 Z=98'XXX GROTTRUM 8 XXXX Z=98 XXXXX
2151 PRINT "Du har en halvrutten tomat i handen men den försvinner."
2152 GOSUB 20500
2153 IF X=0 OR X>6 THEN GOTO 2149
2154 ON X GOTO 2157,21600,2180,2168,2161,2044
2157 IF Flagga(31)<>0 OR Pryl(18)<>1 THEN GOTO 2164
2158 Flagga(31)=1
2159 PRINT "Du har blivit nervös och tar fram brännvinsflaskan och dricker ur den."
2160 GOTO 2150
2161 Skrivrad 1, 175, 176 : GOTO 2104
2164 IF Flagga(32)<>0 THEN GOTO 2168
2165 D=INT(RND(1)*5)+1
2166 IF Pryl(19)=1 AND Flagga(32)=0 THEN GOTO 2173
2167 Flagga(46)=Flagga(46)+1
2168 PRINT "Du står bakom ett draperi."
2169 GOTO 2150
2173 Flagga(32)=1
2174 PRINT "Du råkar hälla ut vattnet på en faun som springer ylande iväg."
2175 GOTO 2150
2180 IF RND(1)*10<8 THEN GOTO 2033
2183 PRINT "Plötsligt känner du en trasa framför näsan och du säckar ihop."
2184 PRINT "När du vaknar märker du att ";
2185 GOTO 2168
2199 Felmeddelande
2200 Z=50'XXX SÖDRA STRANDEN XXXXX Z=50 XXXX
2201 Skrivrad 1, 178, 180 : GOSUB 12300
2211 IF X>0 THEN ON X GOTO 2218,2218,2216,2241,2216,2218,2199,2218,2216,2107
2212 IF Lika(1,A$,"BÅTEN","3") OR Lika(0,A$,"RO","") THEN GOTO 9390 ELSE GOTO 2199
2216 PRINT "Kan du gå på vattnet?"
2217 GOTO 2200
2218 PRINT "Stängslet är för högt för att du ska kunna komma förbi det."
2219 GOTO 2200
2241 Z=51'XXX STRANDHUSET XXXXX Z=51 XXXX
2242 Skrivrad 1, 522, 527 : PRINT TAB(10);"Om du släpper:";TAB(35);"Så får du:"
2248 PRINT : PRINT TAB(10);"EN TAVLA";TAB(35);" 5 poäng"
2250 FOR I=1 TO Varde(0)
2251   IF Pryl(I)<>1 OR Varde(I)=0 THEN GOTO 2257
2252   PRINT TAB(10);
2253   IF Genus(I)=0 THEN PRINT "EN "; ELSE IF Genus(I)=1 THEN PRINT "ETT "; ELSE PRINT "MASSOR AV ";
2254   PRINT SAK$(I,1);TAB(34);
2255   IF Varde(I)<10 THEN PRINT " ";
2256   PRINT Varde(I);"poäng"
2257 NEXT I
2272 GOSUB 12200
2274 IF X=15 THEN GOTO 2290
2275 IF X=6 THEN GOTO 2201
2276 IF X1=1 THEN GOTO 2242 ELSE Felmeddelande
2277 PRINT "Du är vid apparaten och kan bara gå bakåt."
2278 GOTO 2272
2290 Pryl(I)=5 : Flagga(1)=Flagga(1)-1
2292 Poang=Poang+Varde(I)
2300 PRINT "Maskinen slukar ";SAK$(I,0);"."
2302 GOTO 2277
7500 FOR I=1 TO Pryl(0)'XXXXX  Släpper allt man bär i rummet
7501   IF Pryl(I)=1 THEN Pryl(I)=Z
7502 NEXT I
7503 Flagga(1)=0
7504 RETURN
7556 Z=35'XXXXX HISSENS MASKINRUM XXX Z=35 XXX
7558 Skrivrad 1, 189, 190
7560 IF Pryl(1)<>1 THEN PRINT "Det finns en lucka i golvet."
7562 GOSUB 12200
7564 IF X1=1 THEN GOTO 7556
7566 IF X=6 THEN GOTO 26000
7568 IF X=2 AND Pryl(1)<>1 THEN GOTO 7570 ELSE Felmeddelande : GOTO 7556
7569 Felmeddelande
7570 PRINT "Du är i ett litet rum utan fönster."
7573 Z=32 : GOSUB 12200
7579 IF X1=1 THEN GOTO 7588
7585 IF X=1 THEN GOTO 7556
7586 IF X=0 OR X=7 THEN GOTO 7569
7587 IF X=5 THEN GOTO 13000
7588 PRINT "Du kan gå framåt och uppåt." : GOTO 7570
7999 GOSUB 8290
8000 Z=43'XXXXX LABYRINTRUM 8 XXX Z=43 XXX
8001 PRINT "Du är i en gångande vindel."
8002 GOSUB 12200
8003 IF X=0 OR X>7 THEN GOTO 7999
8007 IF Flagga(45)=1 THEN ON X GOTO 1929,8330,1944,8300,8420,8300,8011
8008 PRINT "Det hänger en tavla här."
8009 Thorvald 0
8010 ON X GOTO 8330,7999,8400,17000,8020,16000,8011
8011 GOTO 8017
8017 Poang=Poang-2
8018 PRINT "Gå inte åt höger eller neråt!" : GOTO 8000
8019 GOSUB 8290
8020 Z=44'XXXXX LEBYRINTRUM 9 XXX Z=44 XXX
8021 PRINT "Du är i en gång med vindlar överallt."
8022 GOSUB 12200
8025 IF X=4 THEN Flagga(45)=2
8026 IF X=0 OR X>6 THEN GOTO 8019
8027 ON Flagga(45) GOTO 8250,8263,8430
8034 GOSUB 8290
8035 Z=45'XXXXX LABYRINTRUM 10 XXXXX Z=45 XXX
8036 PRINT "Du är i en vindlande gång med hål överallt."
8037 GOSUB 12200
8038 IF X=0 OR X>6 THEN GOTO 8034
8040 IF Flagga(45)=1 THEN Flagga(45)=3
8041 IF Flagga(45)=3 THEN ON X GOTO 8000,1919,8300,8020,17000,8365
8042 ON X GOTO 1919,8071,8020,8365,8300,8330
8054 PRINT "Den här hjälpen kostar inget."
8056 Flagga(3)=1 : Flagga(41)=1
8057 GOTO 8035
8067 ON Flagga(45) GOTO 8252,8258,8256
8070 GOSUB 8290
8071 Z=38'XXXXX LABYRINTRUM 3 XXXX Z=38 XXX
8072 PRINT "Du är i ett rum som vindlar."
8073 GOSUB 12200
8074 IF X=0 OR X>6 THEN GOTO 8070
8075 IF Flagga(45)=3 THEN ON X GOTO 8095,8071,1919,8365,8330,8020
8076 ON Flagga(45) GOTO 8261,8253
8093 Felmeddelande
8095 Z=39'XXXXX LABYRINTRUM 4 XXXXX
8096 PRINT "Du är i ett rum med hål överallt."
8097 IF Flagga(48)>0 THEN PRINT "Det finns ett krossat fönster här." : GOTO 8102
8100 PRINT "Högt uppe i taket finns ett skört fönster."
8101 PRINT "Någon knackar på det!!!"
8102 GOSUB 12200
8103 IF X1=1 THEN GOTO 8095
8104 IF X=13 THEN GOTO 8131 ELSE IF X=0 THEN GOTO 8093
8105 IF X<>7 THEN GOTO 8112
8106 IF Pryl(20)=1 AND Flagga(33)=0 THEN PRINT "Sparka din pumpade boll!" : Poang=Poang-5 : GOTO 8095
8108 PRINT "Inget du bär kan hjälpa dej att komma upp till fönstret." : Poang=Poang-2
8110 GOTO 8095
8112 IF X=1 THEN PRINT "Du kan inte på något sätt själv nå upp till fönstret." : GOTO 8095
8114 ON Flagga(45) GOTO 8250,8254,8253
8131 PRINT "Du sparkar din boll mot fönstret."
8132 PRINT "   PANG!"
8133 PRINT "Fönsterrutan gick sönder." : IF Flagga(48)=-1 THEN Poang=Poang+10 : Flagga(48)=0
8134 Flagga(48)=Flagga(48)+1
8135 PRINT "Ett rep ramlar ner genom fönstret och nån viskar: -Kom fort!"
8137 IF Jasvar("Klättrar du upp på repet? ") THEN GOTO 8144
8142 PRINT "TYST!! Han hörde dej och drog upp repet!!!"
8143 GOTO 8095
8144 Skrivrad 1, 192, 194 : Pryl(20)=4 : Flagga(1)=Flagga(1)-1
8148 IF Flagga(48)<>-1 THEN PRINT "Du är i en mörk gång." ELSE Underbyggnad : GOTO 17000
8149 Z=97 'XXXXX MÖRK GÅNG ÖVER LAB.4 XXXX
8150 GOSUB 12200
8152 IF X>0 THEN ON X GOTO 17000,25000,18000,10020,8300,8155,8153
8153 Felmeddelande
8154 GOTO 8149
8155 IF Flagga(47)=1 THEN GOTO 9510 ELSE GOTO 2168
8250 ON X GOTO 8365,8300,8000,8330,8020,8095,8011
8252 ON X GOTO 8420,8000,8365,8300,17000,8095,8054
8253 ON X GOTO 8330,8020,8330,8330,8095,8420,8054
8254 ON X GOTO 8381,8000,1500,1919,8330,8095,8054
8256 ON X GOTO 8095,8000,8420,8095,8300,8330,8054
8258 ON X GOTO 8300,8020,8095,8071,8330,14100,8054
8261 ON X GOTO 8381,8365,8300,8095,17000,8071,8054
8263 ON X GOTO 8095,8095,8095,8365,8095,1919,8054
8290 PRINT "Du kan gå åt vänster,höger,framåt,bakåt,uppåt och neråt!"
8291 RETURN
8300 Z=36'XXXXX LABYRINTRUM 1 XXXXX
8302 PRINT "Du är i en vindlande liten gång med hål."
8304 GOSUB 12200
8306 IF X=1 THEN GOTO 8095 ELSE IF X=0 THEN GOTO 8310
8307 IF Flagga(45)=1 THEN ON X GOTO 8320,17000,8000,8322,8365,8330,8700
8308 IF Flagga(45)=2 THEN ON X GOTO 9075,8320,8365,8300,8330,8381,8700
8309 IF Flagga(45)=3 THEN ON X GOTO 1500,8330,8320,8365,8322,8381,8323
8310 GOSUB 8290
8311 GOTO 8304
8320 PRINT "Återvändsgränd!"
8321 GOTO 8300
8322 Flagga(45)=2 : GOTO 8420
8323 PRINT "TIPS!! Skriv framåt!"
8324 Poang=Poang-4
8325 GOTO 8300
8329 GOSUB 8290
8330 Z=37'XXXXX LABYRINTRUM 2 XXX Z=37 XXXX
8331 IF Flagga(38)=0 THEN PRINT "Du är i en kolsvart gång." : GOTO 8335
8332 Skrivrad 1, 529, 530
8334 Rumsbeskrivning
8335 GOSUB 12215
8336 IF Lika(0,A$,"VÄNTA","STANNA") THEN GOTO 8345
8338 IF X=13 THEN GOTO 8350
8339 IF X=7 AND Flagga(38)=0 THEN GOTO 8343
8340 IF X=0 OR X>6 THEN GOTO 8329
8341 IF Flagga(38)=1 THEN Pryl(24)=31 : Flagga(38)=2 : Flagga(1)=Flagga(1)-1 : PRINT "Lampan försvinner!"
8342 ON X GOTO 8300,9075,8320,8365,8420,1919
8343 PRINT "HJÄLP: Det finns en sak här i mörkret."
8344 Poang=Poang-5 : GOTO 8330
8345 IF Pryl(24)<>1 OR Flagga(38)<>1 THEN GOTO 8349
8347 PRINT "Lampan och ";
8348 Flagga(38)=2
8349 PRINT "Du lyfts uppåt." : GOTO 8148
8350 IF Flagga(1)=9 THEN PRINT "Du kan inte bära fler saker." : GOTO 8330
8351 Skrivrad 1, 532, 534 : Flagga(1)=Flagga(1)+1 : Pryl(24)=1 : Flagga(38)=1
8354 GOTO 8334
8363 GOSUB 8290
8365 Z=40'XXXXX LABYRINTRUM 5 XXXXX
8366 PRINT "Du är i en gång med hål."
8367 GOSUB 12200
8368 IF X=0 THEN GOTO 8363
8369 IF Flagga(45)=1 THEN ON X GOTO 8420,9075,8381,14100,8330,8376,8373
8370 IF Flagga(45)=2 THEN ON X GOTO 8376,8300,8400,8320,8381,8420,8700
8371 IF Flagga(45)=3 THEN ON X GOTO 8381,9075,8320,8330,8376,8420,8700
8373 PRINT "TIPS! Gå åt höger."
8374 Poang=Poang-4
8375 GOTO 8365
8376 IF Pryl(8)<>1 THEN GOTO 8330
8377 PRINT "Thorvald kommer förbi, utklädd till präst, och snor din bibel."
8378 Pryl(8)=31 : Flagga(1)=Flagga(1)-1
8379 GOTO 8365
8380 GOSUB 8290
8381 Z=41'XXXXX LABYRINTRUM 6 XXXXX
8382 PRINT "Du vindlar i en liten gång."
8384 GOSUB 12200
8386 IF X=0 THEN GOTO 8380
8387 IF Flagga(45)=1 THEN ON X GOTO 8365,8000,8300,8400,8420,8320,8391
8388 IF Flagga(45)=2 THEN ON X GOTO 8000,8300,8400,8365,8420,8330,8392
8389 IF Flagga(45)=3 THEN ON X GOTO 8420,8365,8000,8400,8300,8330,8392
8391 Flagga(45)=3
8392 PRINT "TIPS!!  Gå uppåt eller bakåt!" : Flagga(45)=3
8393 Poang=Poang-4
8394 GOTO 8380
8400 Z=42'XXXXX LABYRINTRUM 7 XXXXX
8401 Skrivrad 1, 202, 203
8403 GOSUB 12200
8406 IF X=0 THEN GOTO 8400
8407 IF Flagga(45)=1 THEN ON X GOTO 8420,9075,8381,8420,8300,8330,8414
8408 Flagga(45)=3
8409 ON X GOTO 8420,9075,8330,8420,8381,8300,8414
8414 PRINT "TIPS!! Chansa på uppåt eller neråt!"
8415 Poang=Poang-4 : GOTO 8403
8419 GOSUB 8290
8420 Z=52'XXXXX LABYRINTRUM 11 XXXXX
8421 PRINT "Lilla du vindlar."
8422 IF Flagga(43)=0 THEN GOTO 8800
8423 GOSUB 12200
8425 IF X=0 OR X>6 THEN GOTO 8419
8426 IF Flagga(45)=1 THEN ON X GOTO 8071,1929,8381,8000,8300,1500
8428 IF Flagga(45)=2 THEN ON X GOTO 8000,8365,1950,8381,1500,8300
8430 IF Flagga(45)=3 THEN ON X GOTO 8300,8000,8365,1970,8381,1500
8700 PRINT "Du kan inte få nå'n hjälp så som du ser ut!" : D=INT(RND(1)*5)+1
8701 ON D GOTO 8000,8300,8095,8035,8420
8800 REM ******* MUPPARNA *********
8805 Skrivrad 1, 9,11 : GOTO 8095
8999 Felmeddelande
9000 Z=21 : R$="första"'XXXX HISSRUM 1 XXX Z=21 XXX
9002 GOSUB 9250
9004 GOSUB 12200
9008 IF X=4 THEN GOTO 16000
9010 IF X<>3 THEN GOTO 8999
9012 GOSUB 9260
9014 ON X% GOTO 9008,9300,9000
9019 Felmeddelande
9020 Z=28 : R$="åttonde"'XXXX HISSRUM 8 XXX Z=28 XXX
9022 GOSUB 9250
9024 GOSUB 12200
9026 IF X=4 THEN GOTO 22000
9028 IF X<>3 THEN GOTO 9019
9030 GOSUB 9260
9032 ON X% GOTO 9026,9300,9020
9035 Z=22'XXXX HISSRUM 2 XXX Z=22 XXX
9037 PRINT "Du befinner dej i andra våningens hissrum. Till höger ser man"
9039 IF Flagga(40)=2 THEN PRINT "en öppen"; ELSE PRINT "en stängd";
9041 PRINT " hissdörr. I den vänstra väggen finns"
9043 PRINT "ett hål till ett trapprum."
9045 GOSUB 12200
9047 IF X=3 THEN GOTO 15370
9048 IF X1=1 THEN GOTO 9035
9051 IF X<>4 THEN GOTO 9056
9053 GOSUB 9260
9055 ON X% GOTO 9047,9300,9035
9056 Felmeddelande
9058 PRINT "Du är i andra våningens hissrum."
9060 GOTO 9045
9065 Z=23'XXXX HISSRUM 3 XXX Z=23 XXX
9066 Skrivrad 1, 205, 207
9069 GOSUB 12200
9070 IF Lika(1,A$,"SPOLA","") THEN GOTO 9075
9071 IF X1=1 THEN PRINT "Som sagt, "; : GOTO 9065
9072 IF X=3 THEN GOTO 8300
9073 Felmeddelande
9074 PRINT "Du är i toaletten." : GOTO 9069
9075 Skrivrad 1, 209, 212 : GOTO 9450
9099 Felmeddelande
9100 Z=27 : R$="sjunde"'XXXX HISSRUM 7 XXX Z=27 XXX
9102 GOSUB 9250
9103 GOSUB 12200
9104 IF X=4 THEN GOTO 1919
9105 IF X<>3 THEN GOTO 9099
9106 GOSUB 9260
9107 ON X% GOTO 9104,9300,9100
9144 Felmeddelande
9145 Z=24 : R$="fjärde"'XXXX HISSRUM 4 XXX Z=24 XXX
9146 GOSUB 9250
9147 PRINT "Bakom dej anar man en öppning."
9148 GOSUB 12200
9150 IF X=4 THEN GOTO 15432
9152 IF X=6 THEN GOTO 15300
9154 IF X<>3 THEN GOTO 9144
9156 GOSUB 9260
9158 ON X% GOTO 9150,9300,9145
9174 Felmeddelande
9175 Z=26 : R$="sjätte"'XXXX HISSRUM 6 XXX Z=26 XXX
9177 GOSUB 9250
9179 GOSUB 12200
9181 IF X=4 THEN GOTO 8381
9183 IF X<>3 THEN GOTO 9174
9185 GOSUB 9260
9187 ON X% GOTO 9181,9300,9175
9189 Felmeddelande
9190 Z=29 : R$="nionde"'XXXX HISSRUM 9 XXX Z=29 XXX
9192 GOSUB 9250
9194 GOSUB 12200
9196 IF X=4 THEN GOTO 26000
9198 IF X<>3 THEN GOTO 9189
9200 GOSUB 9260
9202 ON X% GOTO 9196,9300,9190
9210 A=A*-1 : GOSUB 9282 'Åk nedåt i hissen ********************
9213 FOR I=1 TO A
9214   C$=Insov$(2,1)
9215   IF C$<>"" THEN EXIT
9216   X=X-1 : GOSUB 9282
9219 NEXT I
9220 PRINT : BEEP : IF RND(1)<0.2 THEN Flagga(41)=1
9221 IF I>A THEN GOTO 9359
9222 REM ****************** NÖDSTOPP ***************************
9223 IF M2%=1 THEN W$=STR$(X)
9224 Flagga(40)=X
9225 PRINT "Hissen stannar och du kastas ur!"
9226 GOTO 9360
9228 GOSUB 9282 'Åk uppåt i hissen *****************************
9230 FOR I=1 TO A
9231   C$=Insov$(2,1)
9232   IF C$<>"" THEN EXIT
9233   X=X+1 : GOSUB 9282
9236 NEXT I
9237 GOTO 9220
9250 PRINT "Du är i ";R$;" våningens hissrum. Till vänster finns en"
9252 IF Flagga(40)=Z-20 THEN PRINT "öppen"; ELSE PRINT "stängd";
9254 PRINT " hissdörr och till höger en annan dörr.": RETURN
9260 X%=2 : Flagga(39)=Z-20
9261 IF Flagga(40)=Flagga(39) THEN Flagga(40)=0 : GOTO 9280
9262 PRINT "Du står framför en stängd hissdörr. På en knapp bredvid"
9263 PRINT "dörren står det ";: COLOR 0,7,0 : PRINT "HIT";: COLOR 7,0,0:PRINT
9264 GOSUB 12200
9265 IF X1=1 THEN GOTO 9262
9266 IF Lika(1,A$,"TRYCK","HIT")=0 THEN X%=1 : GOTO 9280
9268 IF Flagga(40)=10 THEN PRINT "Hissen kommer inte. Den måste vara trasig!" : X%=3 : GOTO 9280
28105 IF (Pryl(29)<>1 AND Pryl(29)<>Z) OR Flagga(6)>1 THEN GOTO 12840
28106 IF Lika(1,C$,"GUBBEN","") OR Flagga(30)=Z THEN GOTO 12840
28107 IF Objekt>0 THEN GOTO 28110
28108 PRINT "Det kan du inte ge till vakten." : GOTO 12210
28110 IF Pryl(Objekt)<>1 THEN GOTO 7034
28112 IF Objekt=13 THEN PRINT "- Behåll den du. Jag vill inte ha den!" : GOTO 12210
28114 IF Objekt=18 THEN GOTO 12360
28116 PRINT "Vakten tar emot ";SAK$(Objekt,0);" med ett snett leende."
28118 Flagga(1)=Flagga(1)-1 : Pryl(Objekt)=2 : GOTO 12210
28130 IF Flagga(6)=3 THEN GOTO 28150
28132 IF Objekt=13 THEN Poang=Poang-8 : Skrivrad 1, 580, 601 : GOTO 28144
28134 IF Flagga(6)=1 THEN GOTO 28140
28136 PRINT "Vakten hindrar dej." : GOTO 12210
28140 PRINT "Vakten släpper motvilligt ";SAK$(Objekt,0);"."
28144 Flagga(1)=Flagga(1)+1 : Pryl(Objekt)=1
28146 GOTO 12210
28150 PRINT "Har du hjärta att ta någonting från en sovande vakt?!?"
28152 A$=LASIN$("")
28154 IF A$<>"JA" THEN GOTO 12214
28156 PRINT "Har du inget hjärta i kroppen?!! Jag vägrar!" : GOTO 12214
28160 IF Pryl(22)<>63 THEN GOTO 6430
28164 Flagga(52)=0 : Poang=Poang-30
28166 GOTO 6420
30000 REM XXX GUBBE XXXX
30001 IF Flagga(30)=96 OR Flagga(49)=1 THEN GOTO 32600
30002 Flagga(30)=INT(RND(1)*92)+9 : Flagga(49)=0 : X1=2
30004 IF Flagga(30)=Z OR Flagga(30)=51 OR Flagga(30)=60 THEN GOTO 30002
30006 Skrivrad 1, 102, 104 : GOTO 32600
30010 IF Flagga(30)<>Z THEN GOTO 30060
30012 IF Pryl(19)=0 THEN PRINT "Gubben har ju vattenflaskan." : GOTO 12210
30014 IF Pryl(19)<>1 OR Flagga(32)>0 THEN PRINT "Du har ju ingen full vattenflaska." : GOTO 12210
30016 PRINT "Gubben dricker ur vattenflaskan och ser genast gladare ut."
30018 Flagga(49)=1 : Flagga(32)=1 : GOTO 12210
30020 IF Objekt=11 AND Flagga(30)=Z AND Flagga(49)=2 THEN GOTO 30030
30022 IF Objekt=19 AND Pryl(10)=0 THEN GOTO 30038 ELSE GOTO 6430
30030 PRINT "Du tar pärlhalsbandet från den bländade gubben."
30032 Flagga(49)=0 : Pryl(11)=1 : X1=1 : Flagga(1)=Flagga(1)+1
30034 GOTO 30002
30038 PRINT "Du tar vattenflaskan från gubben."
30040 Flagga(1)=Flagga(1)+1 : X1=1 : Pryl(19)=1
30042 GOTO 30002
30050 IF Flagga(30)<>Z THEN GOTO 30060
30052 PRINT "Gubben ser din hotande blick och smiter iväg."
30054 Flagga(30)=INT(RND(1)*92)+9 : Flagga(49)=0
30056 IF Flagga(30)=Z OR Flagga(30)=51 OR Flagga(30)=60 THEN GOTO 30054
30058 GOTO 12210
30060 PRINT "Jag ser ingen gubbe här." : GOTO 12210
31000 REM *** SPARA *** &&&&& DEC-10 SPECIELL KOD PÅ 31000-31565
31002 R$=LASIN$("Vilken skivenhet ska jag använda? (A:, B: eller C:) ")
31003 IF R$<>"" AND RIGHT$(R$,1)<>":" THEN R$=R$+":"
31004 R$=R$+"STUGA.STA"
31005 ON ERROR GOTO 31500'&&&&&
31100 OPEN R$ FOR OUTPUT AS #1'&&&&&
31102 ON ERROR GOSUB 32450'&&&&&
31105 KOD=0'&&&&&
31107 PRINT #1,Version
31110 FOR I=0 TO Pryl(0)'&&&&&
31112   PRINT #1,Pryl(I)'&&&&&
31113   IF I<=Varde(0) THEN PRINT #1,Varde(I) : KOD=KOD+Varde(I)/(PI+2)'&&&&&
31115   KOD=KOD+Pryl(I)/(PI)'&&&&&
31117 NEXT I'&&&&&
31125 FOR I=0 TO Flagga(0)'&&&&&
31126   PRINT #1,Flagga(I)'&&&&&
31127   KOD=KOD+Flagga(I)/(PI-1)'&&&&&
31128 NEXT I'&&&&&
31131 PRINT #1,STR$(G*2);",";STR$(Poang*9);",";STR$(Z*7);",";STR$(KOD+Poang+G+Z)'&&&&&
31135 IF Namn$="" THEN PRINT #1,"-----" ELSE PRINT #1,Namn$'&&&&&
31140 FOR I=0 TO Jack(0)'&&&&&
31142   PRINT #1,Jack(I)'&&&&&
31144 NEXT I'&&&&&
31150 CLOSE 1'&&&&&
31155 PRINT "Det nuvarande läget är sparat på filen ";R$;"."'&&&&&
31160 GOTO 12210'&&&&&
31200 REM *** ÅTERSKAPA ***'&&&&&
31202 R$=LASIN$("Vilken skivenhet ska jag använda? (A:, B: eller C:) ")
31203 IF R$<>"" AND RIGHT$(R$,1)<>":" THEN R$=R$+":"
31204 R$=R$+"STUGA.STA"
31205 ON ERROR GOTO 31500
31207 OPEN R$ FOR INPUT AS #1'&&&&&
31210 ON ERROR GOTO 31520'&&&&&
31300 KOD=0'&&&&&
31303 INPUT #1,A1%
31305 INPUT #1,Pryl(0)'&&&&&
31306 INPUT #1,Varde(0)
31307 KOD=Pryl(0)/(PI)+Varde(0)/(PI+2)
31310 FOR I=1 TO Pryl(0)'&&&&&
31312   INPUT #1,Pryl(I)'&&&&&
31313   IF I<=Varde(0) THEN INPUT #1,Varde(I) : KOD=KOD+Varde(I)/(PI+2)'&&&&&
31315   KOD=KOD+Pryl(I)/(PI)'&&&&&
31317 NEXT I'&&&&&
31318 INPUT #1,Flagga(0)'&&&&&
31319 KOD=KOD+Flagga(0)/(PI-1)'&&&&&
31320 FOR I=1 TO Flagga(0)'&&&&&
31322   INPUT #1,Flagga(I)'&&&&&
31325   KOD=KOD+Flagga(I)/(PI-1)'&&&&&
31327 NEXT I'&&&&&
31329 INPUT #1,G,Poang,Z,KOD2'&&&&&
31331 LINE INPUT #1,Namn$'&&&&&
31332 IF RIGHT$(Namn$,5)="-----" THEN Namn$=""'&&&&&
31336 INPUT #1,Jack(0)'&&&&&
31337 FOR I=1 TO Jack(0)'&&&&&
31338   INPUT #1,Jack(I)'&&&&&
31339 NEXT I'&&&&&
31340 G=G/2 : Z=Z/7 : Poang=Poang/9'&&&&&
31341 KOD=KOD+G+Poang+Z'&&&&&
31342 CLOSE 1'&&&&&
31343 ON ERROR GOSUB 32450'&&&&&
31345 IF ABS(KOD-KOD2)>0.03 THEN PRINT "Fel på ";R$ : STOP'&&&&&
31350 IF Z<>4 AND (Z<8 OR Z>100) THEN PRINT "Fel i ";R$ : STOP'&&&&&
31355 X=99 : GOTO 32600
31500 PRINT "? Kan inte öppna ";R$;"."'&&&&&
31510 ON ERROR GOSUB 32450'&&&&&
31515 X1=1 : GOTO 32600
31520 PRINT "Fel inuti ";R$'&&&&&
31525 CLOSE 1'&&&&&
31530 STOP'&&&&&
32450 BEEP : PRINT "Thorvald kommer och ber om ursäkt för fel nr";ERR;"."
32455 PRINT "Han gör en magisk gest och försvinner i ett gult moln."
32460 RETURN
32500 X=100
32600 RESULT=X
END FUNCTION

PROCEDURE: Thorvald
EXTERNAL: Skrivrad
700 REM ********************** THORVALD: ******************************
701 IF Stora=0 AND RND(1)<0.8 THEN GOTO 702 ELSE GOTO 705
702 Skrivrad 1, 126, 131
703 GOTO 710
705 Skrivrad 1, 107, 125
710 REM
END PROCEDURE

PROCEDURE: Felmeddelande
EXTERNAL: Flagga(),A$,X,D,X1,Z,Lika
11000 REM VIOLS SUBFELMEDDELANDERUTIN 3
11001 IF X1>0 THEN GOTO 11100 ELSE Flagga(50)=Flagga(50)-1
11002 IF INSTR(1,A$,"HJÄLP")>0 THEN PRINT "Du kan inte få någon hjälp här." : GOTO 11100
11003 IF INSTR(1,"*N*V*U*NERÅT*NEDÅT*UPPÅT","*"+A$)>0 THEN PRINT "Du kan inte gå ditåt." : GOTO 11100
11004 IF INSTR(1,"*NORR*SÖDER*VÄSTER*ÖSTER*NV*NÖ*NO*SV*SÖ*SO","*"+A$)>0 THEN GOTO 11200
9269 I1=1
9270 I=INSTR(I1,A$," ")
9271 IF I<1 OR I>=LEN(A$) THEN GOTO 9276
9272 D=ASC(MID$(A$,I+1,1))
9273 IF D>47 AND D<58 THEN Flagga(40)=D-48 : GOTO 9278
9274 I1=I+1
9275 IF I1<LEN(A$) THEN GOTO 9270
9276 Flagga(40)=-1
9277 W$="TRYCK 0"
9278 PRINT "Hissen kommer och du stiger in." : A$=Insov$(3,1)
9280 RETURN
9282 'Blink i hissen
9284 LOCATE 4, -1+(8*X)
9286 COLOR 30,0,0 : PRINT CHR$(X+48); : COLOR 7, 0, 0
9290 IF Old>0 THEN LOCATE 4, -1+(8*Old) : PRINT CHR$(Old+48);
9298 Old=X:LOCATE 22,1 : RETURN
9300 Z=18 : CLS : Skrivrad 1,434,449 : Flagga(22)=Flagga(22)+1 'XXX HISSEN XXX
9301 Old=0 : IF Flagga(40)>0 THEN GOTO 9335
9304 X=Flagga(39) : GOSUB 9282: LOCATE 15,1
9305 PRINT "Du är i hissen. Här finns tio knappar. Dom nio första är"
9306 PRINT "numrerade 1-9. På den sista står det NÖDSTOPP."
9307 Rumsbeskrivning
9308 PRINT "Vilken knapp trycker du på? ";
9309 E=INT(RND(1)*9)+1 : E1=INT(RND(1)*5)
9310 IF Flagga(40)=0 OR M3%=1 THEN GOTO 9315 ELSE GOTO 9325
9311 PRINT : PRINT "Dörrarna går igen och hissen startar."
9312 IF M2%=1 THEN PRINT #2,W$ : W$=STR$(E)'&&&&&
9313 Flagga(40)=E : PRINT "Hissen går till";Flagga(40);"an."
9314 GOTO 9356
9315 PRINT : A$=LASIN$("") : PRINT
9316 IF ASC(A$)>48 AND ASC(A$)<58 THEN Flagga(40)=VAL(LEFT$(A$,1)) : GOTO 9335
9318 IF Lika(1,A$,"NÖDSTOPP","")=0 THEN GOSUB 12250: GOTO 9330'Nödstopp
9322 PRINT "Skrik inte på hjälp innan det hemska börjar!"
9323 GOTO 9308
9325 A$=Insov$(5+E1,2)
9327 IF A$="" THEN GOTO 9311
9328 IF M2%=1 THEN PRINT #2,W$ : W$=A$
9329 GOTO 9316
9330 IF ASC(A$)>48 AND ASC(A$)<58 THEN GOTO 9316
9331 IF X1=1 THEN GOTO 9305
9332 Felmeddelande : GOTO 9300
9335 IF Flagga(41)<>1 THEN GOTO 9355
9337 SOV "åker hiss",5
9338 PRINT "Hissen faller!!" : BEEP
9339 PRINT "Hissen krossas mot hisschaktets botten."
9340 Flagga(40)=10 : GOTO 9461
9355 PRINT "Hissen startar." : Vanta "",3
9356 A=Flagga(40)-Flagga(39)
9357 IF A=0 THEN GOTO 9360
9358 IF A<0 THEN GOTO 9210 ELSE GOTO 9228
9359 PRINT "Hissen är framme. Du går ur..."
9360 ON Flagga(40) GOTO 9000,9035,9065,9145,9075,9175,9100,9020,9190
9361 Z=49'XXX ÖSTRA STRANDEN XXX Z=49 XXXXX
9362 Skrivrad 1, 214, 215
9366 GOSUB 12300
9368 IF Lika(1,A$,"BÅTEN","3") OR Lika(1,A$,"RO","") THEN GOTO 9390
9370 IF X<>0 THEN ON X GOTO 20000,9424,20070,9374,20200,9374,9372,9374,20085,2107
9372 Felmeddelande : GOTO 9361
9374 PRINT "Du kan väl inte gå på vattnet?" : GOTO 9361
9390 Z=78'XXXX I BÅTEN XXXXX Z=78 XXXXX
9391 PRINT "Du sitter i båten, mitt i sjön."
9392 GOSUB 12300
9393 IF X>0 AND X<5 THEN ON X GOTO 9410,9416,9361,2200
9397 IF X=10 THEN GOTO 2107
9399 IF X=0 THEN Felmeddelande
9400 PRINT "Du kan ro åt söder, norr, öster eller väster."
9401 GOTO 9390
9410 Skrivrad 1, 217, 220 : GOTO 9450
9416 Skrivrad 1, 222, 224 : SOV "sugs ner", 3 : GOTO 9075
9424 Z=88 : Skrivrad 1, 226, 228
9428 GOSUB 12300
9429 IF X1=1 THEN GOTO 9424
9430 IF X=0 THEN GOTO 9436
9431 ON X GOTO 9361,9437,20085,20330,20070,9432,9437,9437,9437,2107
9432 PRINT "Kan du gå på vattnet?" : GOTO 9424
9436 IF Lika(1,A$,"BADHYTTEN","IN") OR Lika(0,A$,"ÖPPNA","") THEN GOTO 9439
9437 Felmeddelande
9438 GOTO 9424
9439 PRINT "Du går in i badhytten men golvet ger vika och du faller..."
9440 GOTO 25000
9450 PRINT " under bryggan."
9451 PRINT "Du ser ett hål rakt fram, men kan inte komma dit."
9452 IF Flagga(1)=0 THEN GOTO 9455
9453 Z=4 : GOSUB 7500'Släpper allt i garderoben
9454 PRINT "OJ! Du tappar allt du bär på. Nu ligger det på botten."
9455 IF Poang<10 THEN GOTO 9459
9456 IF Jasvar("Vill du vara kvar här? ")=0 THEN GOTO 20005
9459 PRINT
9460 PRINT "Din luft är slut och du kvävs. Ditt lik flyter upp."
9461 Flagga(46)=Flagga(46)+1
9462 IF Jasvar("Vill du att jag ska återuppliva dej? ")=0 THEN GOTO 9484
9470 PRINT "OK, men skyll inte på mej om något går fe..."
9471 IF Flagga(46)=1 OR Flagga(46)=6 THEN GOTO 9479
9473 D=INT(RND(1)*10)+1
9474 IF D>3 THEN GOTO 9479
9475 Skrivrad 1, 230, 232 : GOTO 32500
9479 Skrivrad 1, 234, 235 : Poang=Poang-5 : GOTO 21650
9484 Skrivrad 1, 237, 239 : PRINT "Men jag ska inte bråka. Du får som du vill."
9488 GOTO 32500
9490 Z=54'XXXXX VIGGOS HEMLIGA RUM 1 XXXXX
9491 PRINT "Du är i ett dunkelt, dammtäckt rum."
9492 PRINT "Dörrar går bakåt, åt höger och framåt."
9493 IF Flagga(47)>0 THEN PRINT "Det finns ett hål till vänster." : GOTO 9496
9494 PRINT "Bakom ett draperi till vänster kan man ana ett hål."
9496 GOSUB 12200
9497 IF X1=1 THEN GOTO 9490
9500 IF X>0 THEN ON X GOTO 9501,9501,9510,9558,9545,14000,9501
9501 Felmeddelande
9502 PRINT "Du är i ett dunkelt, dammtäckt rum." : GOTO 9496
9510 Z=55'XXXXX VIGGOS HEMLIGA RUM 2 XXXXX
9511 IF Flagga(47)>0 THEN PRINT "Det finns ett hål i väggen." ELSE PRINT "Du är vid draperiet."
9512 GOSUB 12200
9515 IF X=5 THEN GOTO 9525
9516 IF X=6 THEN GOTO 9492
9517 IF Lika(0,A$,"KLIPP","") AND Flagga(47)=0 THEN GOTO 9520
9518 IF (Lika(0,A$,"ÖPPNA","") OR Lika(0,A$,"DRAG","3")) AND Flagga(47)=0 THEN GOTO 9523
9519 Felmeddelande : GOTO 9510
9520 IF Pryl(27)=1 THEN GOTO 9535
9521 PRINT "Du har inget att klippa med!"
9522 GOTO 9510
9523 PRINT "Du är för svag för att kunna rubba draperiet."
9524 GOTO 9510
9525 IF Flagga(47)=1 THEN GOTO 8148
9526 PRINT "Draperiet är i vägen."
9527 GOTO 9510
9535 Flagga(47)=1 : Poang=Poang+20
9536 PRINT "Du klipper sönder draperiet. Draperiet försvinner."
9537 GOTO 9510
9545 Z=56'XXXXX VIGGOS HEMLIGA RUM 3 XXXXX
9546 PRINT "Du är i en återvändsgränd."
9547 IF Pryl(28)=0 THEN PRINT "Det ligger en låst låda här som du inte orkar bära."
9548 GOSUB 12200
9550 IF X=6 THEN GOTO 9490
9551 IF Lika(0,A$,"LÅS","") THEN GOTO 9553
9552 Felmeddelande : GOTO 9545
9553 IF Pryl(26)<>1 OR Pryl(28)<>0 THEN PRINT "Det kan du inte." : GOTO 9545
9554 PRINT "Du låser upp lådan och hittar en slägga. Lådan försvinner."
9555 Pryl(28)=56 : GOTO 9545
9556 Felmeddelande
9557 PRINT "Du är i höger kammare." : GOTO 9562
9558 Z=57'XXXXX VIGGOS HEMLIGA RUM 4 XXXXX
9559 Skrivrad 1, 240, 242
9562 IF Pryl(27)=0 THEN PRINT "Fastskruvad i väggen sitter en glasask med en sax i."
9563 GOSUB 12200
9564 IF Lika(0,A$,"SLÅ","KROSSA") THEN GOTO 9568
9565 IF X=0 OR X>6 THEN GOTO 9556
9566 ON X GOTO 9556,9556,9556,9556,9575,9490
9568 IF Pryl(27)<>0 THEN GOTO 9556
9569 IF Pryl(28)<>1 THEN PRINT "Du har inget att slå med." : GOTO 9558
9570 PRINT "Du krossar glaset. Saxen ramlar ur och asken försvinner i ett moln."
9571 Pryl(27)=57 : GOTO 9557
9575 D=INT(RND(1)*10)
9576 IF D<4 THEN PRINT "Gången mynnar ut i ett hus." : GOTO 2241
9577 PRINT "Du trampar på en sprängladdning och sprängs i luften!"
9578 GOTO 9461
9696 Old=X
9991 Z=8 : Flagga(8)=Flagga(8)+1'XXXXX HALLEN XXXXX
9993 IF Flagga(8)=1 THEN Poang=Poang+5
9995 IF Flagga(8)<3 OR Flagga(8)>7 THEN GOTO 10009 ELSE GOTO 10000
9997 Felmeddelande
10000 PRINT "Du är i hallen."
10001 GOSUB 12200
10003 IF X1=1 THEN GOTO 10009
10006 IF X>2 AND X<7 THEN ON X-2 GOTO 15350,1500,15425,10015
10008 GOTO 9997
10009 Skrivrad 1, 244, 246
10012 IF Flagga(19)=1 THEN PRINT "Ytterporten är öppen."
10013 IF Flagga(8)>7 THEN Flagga(8)=3
10014 GOTO 10001
10015 IF Flagga(19)=1 THEN PRINT "Porten stängs bakom dej." : Flagga(19)=0 : GOTO 20200
10016 PRINT "Porten är stängd!"
10017 GOTO 10000
10020 Z=16'XXXXX SKUMGUMMIRUMMET XXXXX
10022 Skrivrad 1, 249, 251
10024 IF Pryl(31)=0 THEN PRINT "Det finns ett mystiskt, mörkt fönster i väggen."
10025 IF Pryl(31)=0 THEN PRINT "Bakom fönstret anar man ett föremål."
10030 GOSUB 12200
10032 IF X1=1 THEN GOTO 10020
10034 IF X=0 THEN GOTO 10040
10036 ON X GOTO 10050,25130,25100,10050,21100,25000,10090
10040 IF Pryl(31)>0 THEN GOTO 10048
10042 IF Lika(0,A$,"SLÅ","KROSSA") THEN GOTO 10084
10044 IF Lika(0,A$,"SKÄR","") THEN GOTO 10060
10046 IF Lika(0,A$,"ÖPPNA","") THEN PRINT "Du kan inte öppna fönstret." : GOTO 10050
10048 Felmeddelande
10050 PRINT "Du är i Skumgummirummet."
10052 GOTO 10030
10055 Skrivrad 1, 538, 541 : Pryl(31)=16 : GOTO 9461
10060 IF Lika(1,A$,"DIAMANTEN","") THEN GOTO 10072
10062 IF Lika(1,A$,"TUNGAN","") THEN GOTO 10055
10064 A$=LASIN$("Vad ska du skära med? Din vassa tunga ? ")
10066 IF A$="JA" OR Lika(1,A$,"TUNGAN","") THEN GOTO 10055
10068 IF A$="NEJ" THEN PRINT "Det var ju skönt!" : GOTO 10050
10070 IF Lika(1,A$,"DIAMANTEN","")=0 THEN PRINT "Det går inte!" : GOTO 10050
10072 IF Pryl(1)<>1 THEN PRINT "Du bär väl ingen DIAMANT!" : GOTO 10050
10074 Skrivrad 1, 543, 545 : SOV "sover", 10
10079 Poang=Poang+10 : PRINT : PRINT "När du vaknar är du fortfarande i Skumgummirummet."
10080 Pryl(31)=16 : GOTO 10030
10084 IF Pryl(28)<>1 THEN PRINT "Du har inget att slå med." : GOTO 10050
10086 Skrivrad 1, 547, 550 : Pryl(31)=5 : GOTO 10020
10090 IF Pryl(31)>0 THEN GOTO 10048
10092 IF Pryl(1)<>1 THEN PRINT "Försök att hitta något du kan skära upp fönstret med."
10094 IF Pryl(1)=1 THEN PRINT "Skär upp fönstret med din diamant!"
10096 Poang=Poang-5 : GOTO 10050
12000 X=Kommando(1)'XXXXXXX Riktningar XXXXXXX
12010 IF X<99 THEN RETURN
12015 EXIT GOSUB
12017 EXIT GOSUB
12020 IF X=99 THEN GOTO 31350
12030 GOTO 32500
12100 X=Kommando(2)'XXXXXXX Väderstreck XXXXXXX
12110 GOTO 12010
12200 REM'XXXXX ALLMÄN INOMHUSSUBRUTIN XXXXXX
12205 IF Poang=Maxpoang AND RND(1)>0.1 THEN PRINT "Plötsligt kommer Thorvald fram och säjer:" : GOTO 32660
12210 Rumsbeskrivning
12215 PRINT
12220 A$=LASIN$("") : PRINT
12250 GOSUB 12000
12255 RETURN
12300 REM XXXXXXXXXXX	ALLMÄN UTOMHUSSUBRUTIT XXXXXXXXXXXX
12305 IF Poang=Maxpoang AND RND(1)>0.1 THEN PRINT "Plötsligt kommer Thorvald fram och säjer:" : GOTO 32660
12310 Rumsbeskrivning
12320 PRINT
12325 A$=LASIN$("") : PRINT
12350 GOSUB 12100
12355 RETURN
13000 Z=58'XXX FARSTUN XXXXX
13002 Skrivrad 1, 267, 268
13004 GOSUB 12200
13005 IF X1=1 THEN GOTO 13000
13006 IF X>4 THEN ON X-4 GOTO 13173,7570,13010
13008 Felmeddelande
13009 GOTO 13000
13010 IF Flagga(6)>0 OR Pryl(29)<>Z THEN GOTO 13008
13012 PRINT "Det finns en sak som kan påverka vakten."
13014 Poang=Poang-10
13016 GOTO 13000
13172 Felmeddelande
13173 Z=62'XXX PORTEN XXX
13175 PRINT "Du står vid en jättelik, utsmyckad port."
13176 GOSUB 12200
13177 IF X>5 THEN ON X-5 GOTO 13000,13195
13178 IF X=5 AND Flagga(7)=1 THEN GOTO 13220
13179 IF X=5 THEN PRINT "Porten är låst." : GOTO 13173
13180 IF X1=1 THEN GOTO 13173
13184 IF Lika(1,A$,"LÅS UPP","") THEN GOTO 13200
13186 IF Lika(0,A$,"LÅS","")=0 THEN GOTO 13172
13188 IF Flagga(7)=0 THEN PRINT "Porten är redan låst." : GOTO 13173
13190 PRINT "Det går inte utan nycklar." : GOTO 13173
13195 IF Flagga(7)<>0 OR Flagga(26)=1 THEN GOTO 13172
13196 IF Pryl(26)<>2 THEN PRINT "Vakten orkar låsa upp porten, men han har inga nycklar."
13197 IF Pryl(26)=2 THEN PRINT "Ta hit vakten och lås upp porten."
13198 Poang=Poang-15 : GOTO 13173
13200 IF Flagga(7)=1 THEN PRINT "Porten är redan upplåst." : GOTO 13173
13202 IF Pryl(26)=1 THEN PRINT "Du orkar inte vrida om nyckeln själv." : GOTO 13173
13204 IF Pryl(29)<>1 OR Pryl(26)<>2 THEN PRINT "Det går inte." : GOTO 13173
13206 PRINT "Vakten låser upp porten."
13208 PRINT "Han tittar på nycklarna ett slag, innan han äter upp dom."
13218 Flagga(7)=1 : Pryl(26)=0 : Poang=Poang+10 : GOTO 13173
13220 IF Flagga(26)=1 THEN GOTO 13235 ELSE Flagga(26)=1
13222 Skrivrad 1, 552, 553 : Thorvald 1 : Skrivrad 1, 555, 562
13235 Z=59 : PRINT "Du är i husets matrum. Väggarna är målade i rött och guld."
13237 IF Flagga(15)=0 THEN PRINT "En trappa leder uppåt."
13238 IF Flagga(15)=1 THEN PRINT "En trappa har gått uppåt, men är nu obrukbar."
13241 GOSUB 12200
13244 IF X<>0 THEN ON X GOTO 13247,13245,13245,13245,13245,13173,13245
13245 Felmeddelande
13246 GOTO 13235
13247 IF Flagga(15)=0 AND NOT (Pryl(1)=1) THEN GOTO 21700
13248 IF Flagga(15)=1 THEN GOTO 13252
13249 PRINT "Trappan rasar ihop."
13250 Flagga(15)=1
13251 GOTO 13235
13252 PRINT "Trappan är avspärrad av Stugans gatukontor."
13253 GOTO 13235
14000 Z=64'XXXXX MÖRKA GÅNGEN XXXXX
14004 Skrivrad 1, 282, 284
14012 GOSUB 12200
14018 IF X1=1 THEN GOTO 14000
14020 IF X<>0 THEN ON X GOTO 9490,1909,14030,14100,8000,9490,14022
14022 Felmeddelande
14024 PRINT "Du är i en mörk gång." : GOTO 14012
14030 PRINT "Du går genom en grind som går i lås bakom dej."
14032 GOTO 20040
14034 Skrivrad 1, 286, 287 : BEEP : GOTO 14000
14099 Felmeddelande
14100 Z=65'XXXXX PANNRUMMET XXXXX
14106 Skrivrad 1, 289, 290
14112 GOSUB 12200
14121 IF X=0 THEN GOTO 14099
14124 ON X GOTO 14139,14099,15050,2008,14139,14099,14099
14139 IF Pryl(1)<>1 THEN GOTO 14000
14142 PRINT "Nå'nting du bär på tar emot. Skriv INVENT och släpp det."
14145 GOTO 14100
14998 Felmeddelande
15000 Z=10 : Flagga(10)=Flagga(10)+1'XXXXX KÄLLAREN XXXXX
15004 IF Flagga(10)<3 OR Flagga(10)>7 THEN GOTO 15018
15006 PRINT "Du är i källaren."
15008 GOSUB 12200
15012 IF X1=1 THEN GOTO 15018
15013 IF X=0 THEN GOTO 14998
15014 ON X GOTO 16000,14998,15300,16500,1500,14998,14998
15018 Skrivrad 1, 292, 293
15024 IF Flagga(10)>8 THEN Flagga(10)=4
15026 GOTO 15008
15050 Z=9 : Flagga(9)=Flagga(9)+1 : Flagga(45)=2'XXXXX ÅP-RUMMET XXXXX
15056 IF Flagga(9)<3 OR Flagga(9)>7 THEN GOTO 15066
15058 PRINT "Du är i ÅP-rummet."
15060 GOSUB 12200
15062 IF X<>0 THEN ON X GOTO 15076,15064,15386,14100,1500,15078,15064
15064 Felmeddelande
15066 IF Flagga(9)>8 THEN Flagga(9)=4
15068 Skrivrad 1, 295, 297 : GOTO 15060
15076 PRINT "Tror du att du kan flyga?" : GOTO 15058
15078 PRINT "Du kan inte gå bakåt!" : GOTO 15058
15299 Felmeddelande
15300 Z=11'XXX Hilbertrummet XXX
15302 PRINT "Du är i Hilbertrummet, ett rum med fyra dörrar och hål i taket och golvet."
15304 IF Pryl(17)=11 THEN PRINT "En stege är uppställd mot hålet i taket."
15306 GOSUB 12200
15308 IF X=0 OR X>6 THEN GOTO 15299
15310 ON X GOTO 15312,17000,16000,15000,9145,16500
15312 IF Pryl(17)=11 THEN GOTO 25000
15314 PRINT "Du når inte upp till hålet." : GOTO 15300
15349 Felmeddelande
15350 Z=46 'XXX TRAPPRUM 1 XXXXXZ=46 XXX
15351 PRINT "Du är i ett rum med två rulltrappor."
15352 PRINT "Det finns en dörr bakom dej."
15354 IF Flagga(17)=1 THEN PRINT "Den nedåtgående rulltrappan är avspärrad av Stugans gatukontor."
15355 IF Flagga(18)=1 THEN PRINT "Den uppåtgående rulltrappan är avspärrad av Stugans gatukontor."
15356 GOSUB 12200
15357 IF X=6 THEN GOTO 9991
15358 IF X=1 AND Flagga(18)=0 THEN GOTO 15370
15359 IF X=2 AND Flagga(17)=0 THEN GOTO 15386 ELSE GOTO 15349
15369 Felmeddelande
15370 Z=47'XXX TRAPPRUM 2 XXX
15372 PRINT "Du är i ett rum med en nedåtgående rulltrappa och en dörr åt höger."
15373 IF Flagga(18)=1 THEN PRINT "Rulltrappan är avspärrad av Stugans gatukontor."
15374 GOSUB 12200
15375 IF X=4 THEN GOTO 9035
15376 IF Pryl(1)=1 AND X=2 AND Flagga(18)=0 THEN GOTO 15382
15377 IF X=2 AND Flagga(18)=0 THEN GOTO 15350 ELSE GOTO 15369
15382 Skrivrad 1, 289, 300 : Flagga(18)=1 : GOTO 15370
15385 Felmeddelande
15386 Z=48'XXX TRAPPRUM 3 XXXXX
15388 PRINT "Du är i ett rum med en uppåtgående rulltrappa och en dörr framåt."
15390 IF Flagga(17)=1 THEN PRINT "Rulltrappan är avspärrad av Stugans gatukontor."
15392 GOSUB 12200
15394 IF X=5 THEN GOTO 15050
15396 IF X<>1 OR Flagga(17)<>0 THEN GOTO 15385 ELSE IF Pryl(1)<>1 THEN GOTO 15350
15398 Skrivrad 1, 302, 303 : Flagga(17)=1 : GOTO 15386
15425 Z=90'XXXXX TRAPPRUM 4 XXXXX
15427 PRINT "Dörren öppnar sej och du går in i ett rum"
15428 PRINT "med två trappor och en dörr bakåt."
15429 GOSUB 12200
15430 IF X>0 THEN ON X GOTO 16000,9145,15431,15431,15431,15434,15438
15431 Felmeddelande
15432 PRINT "Du är i trapprummet." : Z=90
15433 GOTO 15429
15434 IF Pryl(1)<>1 THEN GOTO 9991
15435 PRINT "Dörren har gått i baklås."
15436 IF Pryl(26)=1 OR Pryl(26)=90 THEN PRINT "Din nyckel passar inte i låset!"
15437 GOTO 15432
15438 IF Pryl(1)=1 THEN PRINT "TIPS!! Något du bär hindrar dej att gå bakåt!" : GOTO 15432
15439 GOTO 15431
16000 Z=12 : Flagga(12)=Flagga(12)+1'XXXXX VINDEN XXXXX
16012 IF Flagga(12)>2 AND Flagga(12)<8 THEN PRINT "Du är på vinden." : GOTO 16055
16020 Skrivrad 1, 305, 307
16045 IF Flagga(12)>8 THEN Flagga(12)=4
16055 GOSUB 12200
16057 IF X1=1 THEN GOTO 16020
16058 IF X>0 THEN ON X GOTO 15000,15432,9000,15300,17000,16500,16060
16060 Felmeddelande
16065 PRINT "Du är på vinden." : GOTO 16055
16500 Z=13 : Flagga(13)=Flagga(13)+1'XXXXX TOMMA RUMMET XXXXX
16512 IF Flagga(13)>2 AND Flagga(13)<8 THEN PRINT "Du är i Tomma rummet." : GOTO 16530
16515 Skrivrad 1, 309, 310
16525 IF Flagga(13)>8 THEN Flagga(13)=4
16530 GOSUB 12200
16535 IF X1=1 THEN GOTO 16515
16540 IF X>0 THEN ON X GOTO 16000,16545,15000,15300,16545,16545,16545
16545 Felmeddelande
16550 PRINT "Du är i Tomma rummet." : GOTO 16530
17000 Z=14 : Flagga(14)=Flagga(14)+1'XXXXX UNDERLIGA RUMMET XXXXX
17005 IF Flagga(14)<3 OR Flagga(14)>8 THEN GOTO 17100
17010 PRINT "Du är i Underliga rummet."
17020 GOSUB 12200
17025 IF X<7 AND X>0 THEN ON X GOTO 17150,17180,17185,17195,17220,17240
17031 IF X1=1 THEN GOTO 17100
17032 Felmeddelande
17033 GOTO 17010
17100 Skrivrad 1, 312, 313
17110 IF Flagga(14)>8 THEN Flagga(14)=4
17120 GOTO 17020
17150 PRINT "Jag är ledsen, men det tar lång tid att komma fram här."
17155 SOV "tar mej fram",15
17160 D=INT(RND(1)*6)+1
17165 IF D=1 THEN GOTO 20040
17172 IF D=3 THEN GOTO 15050
17175 GOTO 17182
17180 D=INT(RND(1)*4)+1
17181 IF D=4 THEN GOTO 21700
17182 PRINT "Du har vindlat runt i en trång gång och kommer tillbaka."
17183 GOTO 17010
17185 D=INT(RND(1)*6)+1
17186 IF D<3 THEN PRINT "Du har en rutten tomat i handen, men den försvinner."
17190 IF D=5 THEN IF Pryl(1)<>1 THEN GOTO 9991 ELSE GOTO 14100
17191 IF D=6 THEN GOTO 14100
17192 GOTO 17182
17195 D=INT(RND(1)*10)+1
17197 IF D>5 AND Poang>50 THEN GOTO 18000
17205 IF D=2 THEN GOTO 14100
17210 IF D=3 THEN GOTO 8000
17215 GOTO 17182
17220 IF Flagga(3)>0 AND Flagga(41)=1 THEN GOTO 9035
17230 IF Flagga(40)=4 THEN GOTO 1500
17235 GOTO 9190
17240 D=INT(RND(1)*10)
17245 IF D=2 THEN GOTO 15370
17250 GOTO 17182
18000 PRINT "Du är i ZZZZ-rummet. Ett stort schackbräde är ritat på golvet."
18020 IF RND(1)<0.3 OR Pryl(18)<>1 OR Flagga(31)<>1 THEN GOTO 18120
18030 Skrivrad 1, 564, 571 : Flagga(31)=0
18110 ON INT(RND(1)*3)+1 GOTO 21700,1960,1960
18120 PRINT "Du trampas på tårna av en faun, så du springer ut igen."
18125 GOTO 18110
20000 IF Poang>50 THEN GOTO 20005'XXX BRYGGAN XXXXX
20001 Skrivrad 1, 324, 326 : GOTO 20006'PLAY "MBO2T120MNL2AAAAAAAAAAAA"
20005 PRINT "Du är på bryggan och ser ett hus rakt fram."
20006 Z=70
20007 GOSUB 12300
20008 IF X1=1 THEN GOTO 20000
20009 IF Lika(0,A$,"HOPPA","DYK") THEN GOTO 20015
20010 IF X<>0 THEN ON X GOTO 20030,9361,20200,20013,20020,20013,20011,20013,20070,2107
20011 Felmeddelande
20012 GOTO 20005
20013 PRINT "Du kan väl inte gå på vattnet?"
20014 GOTO 20005
20015 PRINT "Du gör ett vackert svanhopp från bryggan i den härliga sjön."
20017 GOTO 9075
20020 Z=71'XXXXX SKOG 1 XXXXX
20021 PRINT "Du är i skogen."
20024 GOSUB 12300
20025 IF X<>0 THEN ON X GOTO 20040,20200,20028,20030,20028,20055,20026,20005,20028
20026 Felmeddelande
20027 GOTO 20020
20028 PRINT "Ett staket hindrar dej att gå ditåt."
20029 GOTO 20020
20030 Z=72'XXXX STRAND 1 XXX Z=72 XXXXX
20031 PRINT "Du är på stranden väster om bryggan."
20032 IF Flagga(53)=1 THEN GOTO 20350 ELSE Flagga(53)=Flagga(53)+1
20033 GOSUB 12300
20034 IF X<>0 THEN ON X GOTO 20055,20000,20020,20037,20040,20037,20035,20037,20200,2107
20035 Felmeddelande
20036 GOTO 20030
20037 PRINT "Du kan väl inte gå på vattnet?"
20038 GOTO 20030
20040 Z=73'XXXXX SKOG MED GRIND XXXXX
20041 PRINT "Du är i skogen, framför en låst grind."
20042 IF Pryl(26)=1 THEN PRINT "Dina nycklar passar inte i grinden."
20043 GOSUB 12300
20044 IF Lika(1,A$,"IN","") THEN PRINT "Grinden är ju låst!" : GOTO 20040
20045 IF Lika(1,A$,"LÅS UPP","") THEN IF Pryl(26)=1 THEN GOTO 20042 ELSE PRINT "Det går inte!" : GOTO 20040
20046 IF Lika(1,A$,"SESAM","") THEN GOTO 14034
20047 IF X<>0 THEN ON X GOTO 20050,20020,20050,20055,20050,20050,20048,20030,20050
20048 Felmeddelande
20049 GOTO 20040
20050 PRINT "Ett staket hindrar dej att gå ditåt!"
20051 GOTO 20040
20054 Felmeddelande
20055 Z=74'XXX STRAND 2 XXXX Z=74 XXXXX
20056 PRINT "Du är på stranden nordväst om sjön."
20057 GOSUB 12300
20058 IF X1=1 THEN GOTO 20061
20059 IF X=0 THEN GOTO 20054
20060 ON X GOTO 20054,20030,20040,20155,20054,20054,20054,20065,20020,2107
20061 Skrivrad 1, 328, 330 : GOTO 20055
20065 PRINT "Du kan väl inte gå på vattnet?" : GOTO 20055
20070 Z=75'XXXXX SKOG 2 XXXXX
20071 PRINT "Du är i skogen. Åt väster ser du ett hus."
20073 GOSUB 12300
20074 IF X<>0 THEN ON X GOTO 20200,20085,20077,9361,20077,20000,20075,9424,20077
20075 Felmeddelande
20076 GOTO 20070
20077 PRINT "Ett staket hindrar dej att gå ditåt."
20078 GOTO 20070
20085 Z=76'XXXXX SKOG 3 XXXXX
20086 PRINT "Du är i skogen."
20088 GOSUB 12300
20089 IF X<>0 THEN ON X GOTO 20070,20092,20094,9424,20092,9361,20090,20105,20092
20090 Felmeddelande
20091 GOTO 20085
20092 PRINT "Ett elektriskt stängsel hindrar dej att gå ditåt."
20093 GOTO 20085
20094 Skrivrad 1, 332,333 : PRINT : PRINT : PRINT "Plötsligt hittar du ";
20098 D=INT(RND(1)*5)
20099 IF D<4 THEN GOTO 20102
20100 PRINT "en stig som du följer tillbaka."
20101 GOTO 20085
20102 PRINT "ett hål som du hoppar ner genom."
20103 PRINT : GOTO 8000
20105 PRINT "Du kryper igenom ett hål i staketet."
20107 Z=77'XXX OVANFÖR RÖVARGÖMSTÄLLET XXX
20108 PRINT "Du är i skogen."
20110 IF Flagga(20)<1 THEN PRINT "Det ser ut som något har grävt här tidigare."
20112 IF Flagga(20)=1 THEN PRINT "Det finns en grop här."
20114 GOSUB 20500
20116 IF X=2 AND Flagga(20)=1 THEN GOTO 20143
20117 IF X1=1 THEN GOTO 20107
20118 IF X<13 THEN GOTO 20085
20120 IF Flagga(20)<0 THEN Poang=Poang+10
20122 Flagga(20)=1
20124 PRINT "Du gräver och gräver..."; : SOV "gräver",10 : PRINT
20143 Z=80'XXXX RÖVARGÖMSTÄLLET XXXX
20145 PRINT "Du är längst ner i en grop och kan bara gå uppåt."
20147 GOSUB 12200
20149 IF X=1 THEN Flagga(3)=1 : GOTO 20107
20151 Felmeddelande : GOTO 20143
20155 Z=79 'XXXXX SKOG 4 XXXXX Z=79 XXX
20156 PRINT "Du är i skogen, väster om sjön."
20158 GOSUB 12300
20161 IF X<>0 THEN ON X GOTO 20197,2115,20055,20165,20197,20197,20162,20180,20195,2107
20162 Felmeddelande
20163 GOTO 20155
20165 Z=82 'XXXXX SKOG 5 XXXXX Z=82 XXX
20166 PRINT "Du är i skogen, sydväst om sjön."
20168 GOSUB 12300
20172 IF X<>0 THEN ON X GOTO 20178,20180,20155,20240,20178,20178,20173,20255,20176
20173 Felmeddelande
20174 GOTO 20165
20176 PRINT "Kan du gå på vattnet?"
20177 GOTO 20165
20178 PRINT "Ett staket hindrar dej att gå ditåt."
20179 GOTO 20165
20180 Z=66 'XXXXX SKOG 6 XXXXX Z=66 XXX
20181 Skrivrad 1, 335, 336
20183 GOSUB 12300
20187 IF X<>0 THEN ON X GOTO 20165,20190,20191,20255,20155,20240,20188,2075,20191,2107
20188 Felmeddelande
20189 GOTO 20180
20190 PRINT "Stängslet är för högt för att du ska kunna komma förbi det." : GOTO 20180
20191 PRINT "Kan du gå på vattnet?"
20192 GOTO 20180
20195 PRINT "Kan du gå på vattnet?"
20196 GOTO 20155
20197 PRINT "Ett staket hindrar dej att gå ditåt."
20198 GOTO 20155
20199 Felmeddelande
20200 Z=81'XXXXX FRAMFÖR HUSET XXXX Z=81 XXX
20202 PRINT "Du står framför husets väldiga port."
20204 GOSUB 12300
20206 IF X1=1 THEN GOTO 20232
20210 IF X=0 THEN GOTO 20199
20212 ON X GOTO 20020,20070,20225,20230,20214,20030,20199,9361,20214
20214 PRINT "Huset är i vägen." : GOTO 20200
20225 IF Flagga(19)=1 THEN PRINT "Porten stängs bakom dej." : Flagga(19)=0 : GOTO 9991
20226 PRINT "Porten är stängd!"
20227 GOTO 20200
20230 PRINT "En avskyvärd stank driver dej tillbaka!"
20231 GOTO 20200
20232 Skrivrad 1, 338, 341 : GOTO 20204
20240 Z=67 'XXXXX SKOG 7 XXXXX Z=67 XXX
20241 PRINT "Du är i skogen."
20243 GOSUB 12300
20247 IF X<>0 THEN ON X GOTO 20251,20255,20165,20251,20251,20251,20248,20251,20180
20248 Felmeddelande
20249 GOTO 20240
20251 PRINT "Ett staket hindrar dej att gå ditåt."
20252 GOTO 20240
20255 Z=68 'XXXXX SKOG 8 XXXXX Z=68 XXX
20256 PRINT "Du är i skogen. I bergväggen åt öster finns en grottöppning."
20258 GOSUB 12300
20262 IF X<>0 THEN ON X GOTO 20240,2075,20180,20266,20165,20266,20263,20266,2075
20263 Felmeddelande
20264 GOTO 20255
20266 PRINT "Ett staket hindrar dej att gå ditåt."
20267 GOTO 20256
20269 PRINT "Stängslet är för högt för att du ska kunna komma förbi det."
20270 Z=83 'XXXXX SKOG 9 XXXXX Z=83 XXX
20271 Skrivrad 1, 343, 344
20273 GOSUB 12300
20277 IF X<>0 THEN ON X GOTO 20269,20285,20281,20300,20281,2066,20278,20315,20330,2107
20278 Felmeddelande
20279 GOTO 20270
20281 PRINT "Kan du gå på vattnet?"
20282 GOTO 20270
20285 Z=84 'XXXXX SKOG 10 XXXX Z=84 XXX
20286 PRINT "Du är i skogen, sydost om sjön."
20288 GOSUB 12300
20292 IF X<>0 THEN ON X GOTO 20270,20297,20330,20315,20295,20300,20293,20297,20297
20293 Felmeddelande
20294 GOTO 20285
20295 PRINT "Kan du gå på vattnet?"
20296 GOTO 20285
20297 PRINT "Ett staket hindrar dej att gå ditåt."
20298 GOTO 20285
20300 Z=85 'XXXXX SKOG 11 XXXX Z=85 XXX
20301 PRINT "Du är i skogen. I bergväggen åt väster finns en grottöppning."
20303 GOSUB 12300
20307 IF X<>0 THEN ON X GOTO 2066,20315,20270,20312,2066,20312,20309,20312,20285
20309 Felmeddelande
20310 GOTO 20300
20312 PRINT "Ett staket hindrar dej att gå ditåt."
20313 GOTO 20300
20315 Z=86 'XXXXX SKOG 12 XXXX Z=86 XXX
20316 PRINT "Du är i skogen."
20318 GOSUB 12300
20322 IF X<>0 THEN ON X GOTO 20300,20327,20285,20327,20270,20327,20324,20327,20327
20324 Felmeddelande
20325 GOTO 20315
20327 PRINT "Ett staket hindrar dej att gå ditåt."
20328 GOTO 20315
20330 Z=87 'XXXXX SKOG 13 XXXX Z=87 XXX
20331 PRINT "Du är i skogen, öster om sjön."
20333 GOSUB 12300
20336 IF X<>0 THEN ON X GOTO 20340,20342,9424,20285,20340,20270,20338,20342,20342,2107
20338 Felmeddelande
20339 GOTO 20330
20340 PRINT "Kan du gå på vattnet?"
20341 GOTO 20330
20342 PRINT "Ett staket hindrar dej att gå ditåt."
20343 GOTO 20330
20350 Flagga(53)=2
20352 BEEP: Skrivrad 1, 346, 350 : GOTO 20033
20500 REM XXXXXXX ALLMÄN SUBRUTIN FÖR RIKTNINGAR OCH VÄDERSTRECK XXXXXX
20505 Rumsbeskrivning
20510 PRINT
20515 A$=LASIN$("") : PRINT
20520 X=0
20525 GOSUB 12100
20530 IF X1=1 THEN GOTO 20540
20535 IF X>0 AND X<5 THEN X=X+2 ELSE GOSUB 12250
20540 Flagga(36)=2 : RETURN
21100 Z=30'XXXXX DIMMIGT BERGSRUM XXXXX
21120 PRINT "Du är i ett dimmigt bergsrum. Kall rå luft blåser dej i"
21122 PRINT "ansiktet. Här finns";
21130 IF Flagga(23)=0 THEN PRINT " en garderob."
21140 IF Flagga(23)=1 THEN PRINT " ett kassaskåp i en garderob."
21155 PRINT "En gång leder uppåt och nedåt men du kan också gå framåt och bakåt."
21160 GOSUB 12200
21180 IF X1=1 THEN GOTO 21120
21190 IF X>0 AND X<7 THEN ON X GOTO 25000,25130,21230,21230,21600,10020
21220 IF Lika(1,A$,"KORKSKRUV","9") THEN GOTO 21300
21230 Felmeddelande
21240 PRINT "Du är i ett dimmigt bergsrum."
21250 GOTO 21160
21300 IF Flagga(23)=1 THEN GOTO 21330
21310 GOTO 21230
21330 PRINT "Kassaskåpet öppnas."
21340 Z=31
21350 GOSUB 12200
21380 IF X>0 AND X<7 THEN GOTO 21500
21390 IF Flagga(23)=0 OR Z=30 THEN GOTO 21240
21410 Felmeddelande
21415 PRINT "Kassaskåpet är öppet."
21420 GOTO 21350
21500 PRINT "Kassaskåpet stängs."
21510 Z=30 : GOTO 21180
21600 Z=61'XXXXX KYRKOGÅRD XXXXX
21605 PLAY "MF" : Skrivrad 1,351,352
21615 GOSUB 12200
21620 IF X1=1 THEN GOTO 21600
21625 IF X>0 AND X<7 THEN ON X GOTO 21635,21650,21635,21635,2150,21100
21635 Felmeddelande
21640 PRINT "Du är på kyrkogården."
21645 GOTO 21615
21650 Z=63'XXXXX GRAVEN XXXXX
21652 PLAY "MNT70P4MBO1L4AL8A.L16AL4AL8>C.<L32BB>C<L8B.L16AL8A.L16AL4A."'Sorgmarsch
21655 Skrivrad 1, 354, 355
21665 Thorvald 0
21667 IF Flagga(50)-Flagga(52)>30 AND Flagga(52)>0 THEN Flagga(52)=0 : Pryl(22)=2 : Pryl(5)=63
21670 GOSUB 12200
21675 IF X1=1 THEN GOTO 21690
21680 IF X=1 THEN GOTO 21600
21685 Felmeddelande
21690 PRINT "Du är i en grav."
21695 GOTO 21670
21700 Z=17'XXXXX OSVALDS RUM XXXXX
21715 IF Flagga(5)>4 THEN Flagga(5)=1 ELSE Flagga(5)=Flagga(5)+1
21717 IF Flagga(5)=1 THEN GOTO 21730
21720 PRINT "Du är i Osvalds rum." : GOTO 21800
21730 Skrivrad 1, 357, 359
21760 IF Flagga(15)=0 AND Flagga(7)=1 THEN PRINT "En trappa går nedåt."
21761 IF Flagga(15)=1 AND Flagga(7)=1 THEN PRINT "Det finns rester av en trappa här."
21800 GOSUB 12200
21810 IF X1=1 THEN GOTO 21730
21815 IF X=0 OR X=7 THEN GOTO 21900
21820 ON X GOTO 21900,21840,25000,18000,22000,21900
21840 IF Flagga(7)=0 THEN GOTO 21900
21845 IF Flagga(15)<>0 THEN PRINT "Trappan är avspärrad av Stugans gatukontor." : GOTO 21715
21847 IF Pryl(1)<>1 THEN GOTO 13235
21850 PRINT "Trappan rasar ihop." : Flagga(15)=1 : GOTO 21715
21900 Felmeddelande
21910 GOTO 21720
22000 REM XXX GARDEROBEN XXXXX Z=4 XXXX
22005 PRINT "Du är i en mörk garderob."
22010 PRINT "Bakom dej och till vänster finns det dörrar."
22030 Flagga(4)=1 : Z=4
22040 GOSUB 12200
22080 IF X1=1 THEN GOTO 22005
22090 IF X=6 THEN GOTO 21700
22100 IF X=3 THEN GOTO 9020
22105 Felmeddelande
22110 PRINT "Du är i garderoben."
22120 GOTO 22040
25000 Z=15'XXX THORVALDS RUM XXXXX
25001 PRINT "Du är i Thorvalds rum. Vid väggen står en stor förseglad"
25004 IF Pryl(1)=0 THEN Pryl(1)=15
25005 PRINT "kista. I taket finns en taklucka och i golvet finns ett hål."
25008 IF Pryl(3)=31 THEN PRINT "På väggen står det: KORKSKRUV HJÄLPER TILL MED KASS..."
25010 GOSUB 12200
25012 IF X>0 THEN ON X GOTO 25050,15300,25100,21700,10020,21100,25060
25014 IF X1=1 THEN GOTO 25000
25016 IF Lika(1,A$,"ÖPPNA","") THEN GOTO 25045
25019 IF Lika(1,A$,"LÅS UPP","") THEN PRINT "Det finns inget lås." : GOTO 25025
25020 IF Lika(1,A$,"KISTAN","") THEN GOTO 25035
25023 Felmeddelande
25025 PRINT "Du är i Thorvalds rum." : GOTO 25010
25035 IF Pryl(31)<>1 THEN PRINT "Du kan inte öppna kistan." : GOTO 25025
25036 Skrivrad 1, 573, 575 : Pryl(16)=1 : Pryl(31)=5 : GOTO 25000
25045 IF A$="ÖPPNA" THEN A$=LASIN$("Öppna vadå? ")
25046 IF Lika(1,A$,"KISTAN","") THEN GOTO 25035
25047 IF Lika(1,A$,"TAKLUCKAN","LUCKAN")=0 THEN GOTO 25023
25050 IF Pryl(17)<>1 AND Pryl(17)<>Z THEN PRINT "Takluckan sitter för högt!" : GOTO 25000
25051 PRINT "Du klättrar upp på stegen och öppnar luckan."
25053 IF Pryl(2)<>0 THEN PRINT "Där finns inget, så du klättrar ner igen." : GOTO 25000
25054 Skrivrad 1, 366, 367 : Pryl(2)=15 : GOTO 25025
25060 IF Pryl(2)>0 THEN GOTO 25070
25062 IF Pryl(17)<>1 AND Pryl(17)<>Z THEN PRINT "Det behövs en stege för att nå upp."
25064 IF Pryl(17)=1 OR Pryl(17)=Z THEN PRINT "Öppna takluckan!"
25066 Poang=Poang-5
25068 GOTO 25025
25070 IF Pryl(31)=5 THEN GOTO 25023
25072 PRINT "Kistan kan bara öppnas med en kofot."
25074 GOTO 25066
25100 Faunsang : Skrivrad 1, 369, 379
25116 IF RND(1)<0.7 OR Flagga(29)=1 THEN GOTO 25130
25117 PRINT "Han kastar en kniv mot dej...                     ";
25118 IF RND(1)<0.5 THEN GOTO 25121 ELSE PRINT "Den träffar!!" : Z=15 : BEEP
25119 GOSUB 7500
25120 GOTO 9461
25121 PRINT "Den missar!" : PRINT "Golvet ger plötsligt vika och du faller."
25122 GOTO 15300
25130 PRINT "Du är i ett mörkt rum."
25135 Z=96
25136 GOSUB 12200
25210 IF X=1 THEN GOTO 21100
25212 IF X=6 THEN GOTO 10020
25215 IF X1=1 THEN GOTO 25130
25220 Felmeddelande
25230 GOTO 25130
26000 Z=100'XXX TEFELONSTUGAN XXX Z=100 ZZZZZZZZZZ
26005 Flagga(27)=Flagga(27)+1
26010 IF Flagga(27)>3 AND Flagga(27)<8 THEN GOTO 26030
26015 PRINT "Du är i en stuga med dörrar bakåt, åt vänster och åt höger."
26020 PRINT "Högt upp i taket finns ett fönster."
26025 GOTO 26035
26030 PRINT "Du är i stugan."
26035 IF Flagga(27)=8 THEN Flagga(27)=4
26040 Rumsbeskrivning
26045 IF Flagga(27)=1 AND Jack(100)=1 AND Pryl(25)=100 THEN PRINT "Telefonen ringer." : Telefon : ON PLAY(1) Itelefon : PLAY ON
26050 GOSUB 12215
26055 IF Lika(1,A$,"SVARA","SVAR") THEN GOTO 26100
26065 IF X1=1 THEN GOTO 26015
26070 IF X>2 AND X<7 THEN GOTO 26085
26075 Felmeddelande
26080 GOTO 26030
26085 IF Flagga(27)=1 THEN Flagga(27)=0
26088 PLAY OFF : PLAY "MF"
26090 ON (X-2) GOTO 9190,7556,26075,26150
26100 IF Flagga(27)>1 OR Jack(100)=0 OR Pryl(25)<>100 THEN GOTO 26075
26102 PLAY OFF : PLAY "MF"
26105 Flagga(27)=2
26110 PRINT "Du svarar i telefon och hör en röst:"
26115 IF Namn$="" THEN Namn$=LASIN$("- Vad heter du? ")'Namninläsning
26120 PRINT "Hej, ";Namn$;"! Bra att du också har skaffat en telefon."
26122 PRINT "<klick>"
26125 GOTO 26030
26150 IF Pryl(1)=1 THEN PRINT "Dörren är igenbommad av Stugans gatukontor." : GOTO 26000
26155 GOTO 2127
31350 REM ****** Hoppa till rätt rum **********
31351 IF Z=0 THEN GOTO 9461'Återupplivning
31352 IF Z=1 THEN GOTO 9075'Under bryggan
31353 IF Z=4 THEN GOTO 22000'&&&&&
31355 IF Z<8 OR Z>100 THEN PRINT "Otillåtet Z-värde!" : STOP'&&&&&
31360 IF Z<20 THEN ON Z-7 GOTO 9991,15050,15000,15300,16000,16500,17000,25000,10020,21700,9300,2044'&&&&&
31365 IF Z<31 THEN ON Z-19 GOTO 2115,9000,9035,9065,9145,2075,9175,9100,9020,9190,21100'&&&&&
31370 IF Z<41 THEN ON Z-30 GOTO 21340,7570,2066,1909,7556,8300,8330,8071,8095,8365'&&&&&
31375 IF Z<51 THEN ON Z-40 GOTO 8381,8400,8000,8020,8035,15350,15370,15386,9361,2200'&&&&&
31380 IF Z<61 THEN ON Z-50 GOTO 2241,8420,1500,9490,9510,9545,9558,13000,13235,8800'&&&&&
31385 IF Z<71 THEN ON Z-60 GOTO 21600,13173,21650,14000,14100,20180,20240,20255,2019,20000'&&&&&
31390 IF Z<81 THEN ON Z-70 GOTO 20020,20030,20040,20055,20070,20085,20107,9390,20155,20143'&&&&&
31395 IF Z<91 THEN ON Z-80 GOTO 20200,20165,20270,20285,20300,20315,20330,9424,1929,15432'&&&&&
31400 ON Z-90 GOTO 2033,1919,1950,1960,1970,25130,8148,2150,2127,26000'&&&&&
32000 ON ERROR GOSUB 32450'XXXXX NU BÖRJAR VI XXXXX
32001 ON KEY(1) Poangsiffra : KEY(1) ON
32003 Flagga(0)=53 :FOR I=1 TO Flagga(0) : Flagga(I)=0 : NEXT I
32006 FOR I=1 TO 10 : KEY I,"" : NEXT I
32007 OPEN "sol.hot" AS #4 LEN=SIZE(C$)
32008 READ RECORD #4,254,C$
32009 IF VAL(C$)<>LOF(4)*3 THEN PRINT "Fel i SOL.HOT. Kopiera från din originalskiva." : STOP
32010 Flagga(16)=3 : Flagga(20)=-1 : Flagga(30)=96 : Flagga(32)=1
32012 Flagga(33)=1  : Flagga(40)=1 : Flagga(45)=1  : Flagga(48)=-1
32020 Namn$="" : Jack(0)=100
32025 LOCATE ,,0,0,0
32030 Skrivrad 1, 381, 387 : PLAY "MBMLT127O2L16G.L32F+L16G.L32"
32032 PLAY "AL4GL16F.L32EL16F.L32GL4FL16E.L32E-L16E.L32FL4E"
32034 Skrivrad 1,388,394:PLAY "MNL16D.L32C+L16D.L32EL4DL8C.L16EL8G.L16>C<L8B.L16AL8G.L16FL4EGDG"
32036 Skrivrad 1,395,403:PLAY "L8C.L16EL8G.L16>C<L8B.L16AL8G.L16FL4EGDGMF"
32037 A$=Insov$(10,1) : Poang=50 : Maxpoang=307
32040 CLS : LOCATE ,,0,0,0
32050 FOR I=1 TO Jack(0) : READ Jack(I) : NEXT I
32068 IF LEFT$(DATE$,5)="04-01" THEN A1=1 ELSE A1=0
32070 PRINT "Välkommen till Stugan!!!!!" : PRINT
32090 IF Jasvar("Har du vågat dej in här förut? ") THEN PRINT : GOTO 32200
32091 PRINT
32100 PRINT "Då behövs lite hjälp och instruktioner!" : PRINT
32110 Skrivrad 1, 132, 150 : A$=Insov$(10,1)
32200 Pryl(0)=33 : Varde(0)=15'Inläsning av prylarna
32202 FOR I=1 TO Varde(0)
32204 READ SAK$(I,1),SAK$(I,2),SAK$(I,3),SAK$(I,0),Pryl(I),Genus(I),Varde(I):NEXT I
32208 FOR I=Varde(0)+1 TO Pryl(0)
32210 READ SAK$(I,1),SAK$(I,2),SAK$(I,3),SAK$(I,0),Pryl(I),Genus(I) : NEXT I
32214 GOTO 20000
32250 REM Data för telefonjack. 1=jack, 2=plats för jack
32252 DATA 0,0,0,2,0, 0,0,2,2,2, 2,2,2,2,2, 2,1,0,0,0, 2,2,2,2,2
32254 DATA 0,0,0,0,0, 1,0,0,2,2, 2,0,2,0,2, 0,0,1,2,0, 2,0,2,0,0
32256 DATA 0,2,0,2,0, 2,0,1,2,0, 0,2,0,0,0, 0,0,0,2,0, 0,0,0,0,0
32258 DATA 0,0,1,0,2, 0,0,0,0,0, 0,0,0,2,2, 0,0,2,0,2, 2,1,0,0,1
32300 DATA "DIAMANT","DIAMA","DIAMA","diamanten",15,0,15
32302 DATA "GURKA","GURKA","ILLAL","gurkan",0,0,1
32304 DATA "SILVERTACKA","SILVE","TACKA","silvertackan",31,0,10
32306 DATA "HILLEBARD","HILLE","JUVEL","hillebarden",2,0,20
32308 DATA "DÖSKALLE","DÖSKA","SKALL","döskallen",0,0,5
32310 DATA "KLOCKA","VÄCKA","KLOCK","klockan",59,0,15
32312 DATA "GULDMYNT","GULD","MYNT","guldmynten",0,2,10
32314 DATA "BIBEL","BIBEL","FAMILJEB","familjebibeln",36,0,10
32316 DATA "KONTRAKT","KONTR","SKÄRT","kontraktet",0,1,15
32318 DATA "LAGERKRANS","LAGER","KRANS","lagerkransen",53,0,15
32320 DATA "PÄRLHALSBAND","PÄRL","HALSB","pärlhalsbandet",0,1,25
32322 DATA "FAUNSKO","FAUN","SKO","faunskon",0,0,5
32324 DATA "MEDALJ","MEDAL","MEDAL","medaljen",2,0,12
32326 DATA "ALGER","ALGER","ALG","algerna",0,2,10
32328 DATA "SPEGEL","SPEGE","SPEGE","spegeln",9,0,7
32332 DATA "CYKELPUMP","CYKEL","PUMP","cykelpumpen",0,0
32334 DATA "STEGE","STEGE","STEGE","stegen",4,0
32336 DATA "BRÄNNVINSFLASKA","BRÄNN","BRÄNN","brännvinsflaskan",97,0
32338 DATA "VATTENFLASKA","VATTENF","VATTENF","vattenflaskan",0,0
32340 DATA "BOLL","BOLL","BOLL","bollen",8,0
32342 DATA "SPADE","SPADE","SPADE","spaden",61,0
32344 DATA "LIK","LIK","LIK","liket",0,1
32346 DATA "KATALOG","KATAL","TELEFONK","katalogen",0,0
32348 DATA "LAMPA","LAMPA","LAMPA","lampan",0,0
32350 DATA "TELEFON","TELEF","TELEF","telefonen",100,0
32352 DATA "NYCKLAR","NYCKL","NYCKE","nycklarna",54,2
32354 DATA "SAX","SAX","SAX","saxen",0,0
32356 DATA "SLÄGGA","SLÄGG","SLÄGG","släggan",0,0
32357 DATA "VAKT","VAKT","VAKT","vakten",58,0
32358 DATA "FÖRLÄNGNINGSSLADD","FÖRLÄ","SLADD","förlängningssladden",26,0
32360 DATA "KOFOT","KOFOT","KOFOT","kofoten",0,0
32362 DATA "SENGÅNGARE","SENGÅ","APA","sengångaren",82,0
32364 DATA "POLLETTER","POLLE","NATRI","polletterna",75,2
32450 BEEP : PRINT "Thorvald kommer och ber om ursäkt för fel nr";ERR;"."
32455 PRINT "Han gör en magisk gest och försvinner i ett gult moln."
32460 RETURN
32500 REM XXX SLUT XXXX
32502 IF M2%=1 THEN PRINT #2,W$ : CLOSE 2 : M2%=0'&&&&& LOGGA
32503 IF M3%=1 THEN CLOSE 3 : M3%=0'&&&&&
32590 PRINT "Du fick";Poang;"av";Maxpoang;"möjliga poäng!"
32595 IF Poang=Maxpoang THEN GOTO 32660
32600 IF Poang<50 THEN I=50 : PRINT "Du kan klassas som en klantig nybörjare." : GOTO 32700
32604 IF Poang<55 THEN I=55 : PRINT "Du är en ren amatör inom stugforskningen." : GOTO 32700
32610 IF Poang<65 THEN I=65 : PRINT "Du är en duktig nybörjare inom stugforskningen." : GOTO 32700
32612 IF Poang<90 THEN I=90 : PRINT "Du är en erfaren stugforskare." : GOTO 32700
32614 IF Poang<120 THEN I=120 : PRINT "Du kan kalla dej en stugfogde." : GOTO 32700
32620 IF Poang<150 THEN I=150 : PRINT "Du är en erfaren stugfogde." : GOTO 32700
32630 IF Poang<200 THEN I=200 : PRINT "Du är en väldigt erfaren stugfogde." : GOTO 32700
32640 IF Poang<250 THEN I=250 : PRINT "Du är biträdande expert på hus i Småland." : GOTO 32700
32645 IF Poang<300 THEN I=300 : PRINT "Du är expert på hus i Småland." : GOTO 32700
32650 IF Poang<Maxpoang THEN I=Maxpoang : PRINT "Du är föreslagen som medlem i stugrådet." : GOTO 32700
32660 Skrivrad 1, 577, 578 : GOTO 32720
32700 PRINT "För att komma upp i nästa klass behöver du";I-Poang;"poäng till."
32720 REM Eventuell loggning av resultat
32760 PRINT : PRINT "Thorvald hälsar:   - Välkommen tillbaka!"
32762 PRINT
32766 STOP'         NU ÄR PROGRAMMET NÄSTAN SLUT            KKKKKOLOLOLOLLKHH
32767 END

ENDFILE
