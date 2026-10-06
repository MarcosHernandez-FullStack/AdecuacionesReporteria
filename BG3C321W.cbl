000100 IDENTIFICATION DIVISION.                                         00010001
000200*-----------------------*                                         00020001
000300 PROGRAM-ID.  BG3C321W.                                           00030001
000400                                                                  00040001
000500******************************************************************00050001
000600*   ** DISTRIBUCION DE LOS DEPOSITOS SEGUN ESCALA DE MONTOS **   *00060001
000700*   **                 A N E X O   N. 13                    **   *00070001
000800*   ** INFORMACION CONSOLIDADA POR CLIENTE                  **   *00080001
000900*   ** AGRUPA LO EMITIDO POR : BG3C3150, BG3C3160, BG3C3170 **   *00090001
001000*   **                       Y CT3CAX80                     **   *00100001
001100******************************************************************00110001
001200*        L O G    D E   M O D I F I C A C I O N E S              *00120001
001300******************************************************************00130001
001400******************************************************************00140001
001500*  PETICION   FECHA    AUTOR            DESCRIPCION              *00150001
001600* --------- ---------- -----  ---------------------------------- *00160001
001700* 200601187  170306   RVF     GENERAR ARCHIVO DE TOTALES Y LISTAR*00170001
001800*                             REPORTE CONSOLIDADO O X PRODUCTO   *00180001
001900* 200608014  020806   RVF     INCLUIR CORASU M36 (IGUAL A M13)   *00190001
001900* 201001166  270110   LAC     MEJORA DE ANEXOS                   *00200001
002000*----------------------------------------------------------------*00210001
002000* GSI-1201537  02-05-2007  P020659  INCLURI CORASU M37 Y M38.    *00220001
002000* 200902167    15-07-2010  P020951  MIGRACION SISTEMA CTS A CTAS *00230001
      *                                   PERSONALES. INHABILITAR      *00240001
      *                                   DATOS DEL ARCHIVO CTS CONVI- *00250001
      *                                   VENCIA.                      *00260001
      *201305033     28-05-2013 P014263   SE INCREMENTAN LOS TRAMOS DE *00270001
      *                                   IMPORTE DE LA SBS            *00280001
      *6792016011-05 23-08-2016   MDP     CAMBIO DENOMINACION Y SIMBOLO*00290001
      *                                   MONEDA SOLES                 *00300001
      *IGH001        11-11-2019 XP94134   SE INCORPORA LA LECTURA DE   *00310001
      *                                   ARCHIVO PARA LA OBTECION DEL *00320001
      *                                   LA CLASIFICACION DEL CLIENTE *00330001
      *IGH001        11-11-2019 XP94134   SE INCORPORA LA LECTURA DE   *00331001
      *                                   ARCHIVO PARA LA OBTECION DEL *00332001
      *                                   LA CLASIFICACION DEL CLIENTE *00333001
      *PSDAEMF2-446   P021122   2024-04-11    SE AGREGA SEPARADORES A  *00334001
      *   GLB                                 REPORTES                 *00335001
002100******************************************************************00340001
002200                                                                  00350001
002300 ENVIRONMENT DIVISION.                                            00360001
002400*--------------------*                                            00370001
002500 INPUT-OUTPUT SECTION.                                            00380001
002600*--------------------*                                            00390001
002700 FILE-CONTROL.                                                    00400001
002800*------------*                                                    00410001
002900     SELECT E1DQANX0              ASSIGN E1DQANX0.                00420001
003000                                                                  00430001
003100     SELECT S1DQAX13              ASSIGN S1DQAX13.                00440001
003200                                                                  00450001
003300* 200601187-INI                                                   00460001
003400     SELECT S2DQTOTL              ASSIGN S2DQTOTL.                00470001
003500* 200601187-FIN                                                   00480001
      *IGH001 --> I                                                     00490001
           SELECT E1CORASU          ASSIGN E1CORASU                     00500001
                                    FILE STATUS   IS FILE-STATUS.       00510001
      *IGH001 <-- F                                                     00520001
003600                                                                  00530001
003700 DATA DIVISION.                                                   00540001
003800*-------------*                                                   00550001
003900 FILE SECTION.                                                    00560001
004000*------------*                                                    00570001
004100 FD  E1DQANX0                                                     00580001
004200     RECORDING MODE  IS  F                                        00590001
004300     LABEL RECORDS   ARE STANDARD                                 00600001
004400     BLOCK CONTAINS  0   RECORDS.                                 00610001
004500 01  REG-E1DQANX0.                                                00620001
004600     05  ANX-CENTRAL        PIC X(08).                            00630001
004700     05  ANX-IND-FILE       PIC X.                                00640001
004800     05  ANX-LIBRE          PIC X.                                00650001
004900     05  ANX-DATA.                                                00660001
005000         10  ANX-CCC        PIC X(18).                            00670001
005100         10  ANX-DAT        PIC X(252).                           00680001
005200*                                                                 00690001
005300 FD  S1DQAX13                                                     00700001
005400     RECORDING MODE F                                             00710001
005500     BLOCK CONTAINS 0 RECORDS                                     00720001
005600     LABEL RECORDS ARE OMITTED.                                   00730001
005700 01  REG-SALIDA.                                                  00740001
   GLB*    05 REG-SAL                   PIC X(132).                     00750002
   GLB     05 REG-SAL                   PIC X(170).                     00751002
005900                                                                  00760001
006000* 200601187-INI                                                   00770001
006100 FD  S2DQTOTL                                                     00780001
006200     RECORDING MODE F                                             00790001
006300     BLOCK CONTAINS 0 RECORDS                                     00800001
006400     LABEL RECORDS ARE OMITTED.                                   00810001
006500 01  REG-S2DQTOTL.                                                00820001
006600     05 SAL1-AAMMDD               PIC X(08).                      00830001
006700     05 SAL1-MODALIDAD            PIC X(02).                      00840001
006800     05 SAL1-MONEDA               PIC X(03).                      00850001
006900     05 SAL1-ESC-DESDE            PIC 9(13)V99.                   00860001
007000     05 SAL1-ESC-HASTA            PIC 9(13)V99.                   00870001
007100     05 SAL1-PN-NUMERO            PIC 9(15).                      00880001
007200     05 SAL1-PN-MONTO             PIC 9(13)V99.                   00890001
007300     05 SAL1-PJ-SFL-NUMERO        PIC 9(15).                      00900001
007400     05 SAL1-PJ-SFL-MONTO         PIC 9(13)V99.                   00910001
007500     05 SAL1-PJ-OTR-NUMERO        PIC 9(15).                      00920001
007600     05 SAL1-PJ-OTR-MONTO         PIC 9(13)V99.                   00930001
007700     05 SAL1-TOTALES-NUMERO       PIC 9(15).                      00940001
007800     05 SAL1-TOTALES-MONTO        PIC 9(13)V99.                   00950001
007900* 200601187-FIN                                                   00960001
008000                                                                  00970001
      *IGH001 --> I                                                     00980001
       FD  E1CORASU                                                     00990001
           LABEL RECORD STANDARD                                        01000001
           RECORDING MODE IS F                                          01010001
           BLOCK CONTAINS 0 RECORDS.                                    01020001
                                                                        01030001
       01  REG-E1CORASU           PIC X(290).                           01040001
      *IGH001 <-- F                                                     01050001
008100 WORKING-STORAGE SECTION.                                         01060001
008200*-----------------------*                                         01070001
008300 01 SW-INICIO                     PIC 9         VALUE ZEROS.      01080001
008400 01 W-REG-HEADER.                                                 01090001
008500    05 FILLER                     PIC X(01)     VALUE SPACES.     01100001
008600    05 FILLER                     PIC X(14)     VALUE             01110001
008700       'HEADERBG3C321W'.                                          01120001
008800    05 WT-FECHA.                                                  01130001
008900       10 WT-DIA                  PIC 99.                         01140001
009000       10 FILLER                  PIC X(01)     VALUE '/'.        01150001
009100       10 WT-MES                  PIC 99.                         01160001
009200       10 FILLER                  PIC X(01)     VALUE '/'.        01170001
009300       10 WT-ANO                  PIC 99.                         01180001
       01 W-CLAVE-A.                                                    01190001
          05 W-ANT-CENTRAL                 PIC X(08)     VALUE SPACES.  01200001
          05 W-ANT-CORASU                  PIC X(03)     VALUE SPACES.  01210001
                                                                        01220001
      *201001166-INI                                                    01230001
       01 W-CLAVE-N.                                                    01240001
          05 W-NUE-CENTRAL                 PIC X(08)     VALUE SPACES.  01250001
          05 W-NUE-CORASU                  PIC X(03)     VALUE SPACES.  01260001
      *201001166-FIN                                                    01270001
009600                                                                  01280001
009700* 200601187-INI                                                   01290001
009800 01 W-ANT-TIPO-REG                PIC X         VALUE SPACES.     01300001
009900 01 W-ANT-PRODUCTO                PIC X(2)      VALUE SPACES.     01310001
010000* 200601187-FIN                                                   01320001
010100                                                                  01330001
010200 01 CT-LEIDOS                     PIC 9(09)     VALUE 0.          01340001
010300 01 W-CEROS                       PIC 9(06)     VALUE 0    COMP-3.01350001
010400 01 W-MENOS                       PIC 9(06)     VALUE 0    COMP-3.01360001
010500 01 W-SOLES                       PIC 9(15)V99  VALUE 0    COMP-3.01370001
010600 01 W-SALDO-CENTRAL               PIC 9(15)V99  VALUE 0    COMP-3.01380001
010700 01 W-PAG                         PIC 999       VALUE 0.          01390001
010800 01 II                            PIC 999       VALUE 0    COMP-3.01400001
010900 01 I                             PIC 999       VALUE 0    COMP-3.01410001
011000 01 J                             PIC 999       VALUE 0    COMP-3.01420001
011100 01 K                             PIC 999       VALUE 0    COMP-3.01430001
011200 01 WS-NRO                        PIC 999       VALUE 0    COMP-3.01440001
011300 01 WS-LIN                        PIC 999       VALUE 0    COMP-3.01450001
      *IGH001 --> I                                                     01460001
       01 FILE-STATUS                   PIC X(02)     VALUE SPACES.     01470001
       01 WSV-DIVISA                    PIC X(03)     VALUE SPACES.     01480001
       01 WSV-ARCHIVO                   PIC X(08)     VALUE SPACES.     01490001
       01 WSV-STATUS                    PIC X(02)     VALUE SPACES.     01500001
       01 WSV-OPERACION                 PIC X(12)     VALUE SPACES.     01510001
       01 WSA-LEIDOS-E1CORASU           PIC 9(06)     VALUE 0.          01520001
       01 I-CORA                        PIC 9(05)     VALUE 0.          01530001
       01 I-CORULT                      PIC 9(05)     VALUE 0.          01540001
       01 WSV-TEMPO-CORASU              PIC X(3)      VALUE SPACES.     01550001
      *IGH001 <-- F                                                     01560001
011400                                                                  01570001
011500 01 WS-SWITCH.                                                    01580001
011600    05  SW-REG-SEL                PIC X         VALUE 'N'.        01590001
011700        88 SI-REG-SEL                           VALUE 'S'.        01600001
011800        88 NO-REG-SEL                           VALUE 'N'.        01610001
011900    05  SW-ACUMULA                PIC X         VALUE 'N'.        01620001
012000        88 SW-SI-ACUMULA                        VALUE 'S'.        01630001
012100        88 SW-NO-ACUMULA                        VALUE 'N'.        01640001
      *IGH001 --> I                                                     01650001
       01 WSS-E1CORASU-FIN              PIC X(01) VALUE 'S'.            01660001
          88 WSS-E1CORASU-FIN-SI                  VALUE 'S'.            01670001
          88 WSS-E1CORASU-FIN-NO                  VALUE 'N'.            01680001
                                                                        01690001
       01  SW-E1CORASU-STATUS               PIC X(01) VALUE 'N'.        01700001
           88  FIN-E1CORASU                           VALUE 'S'.        01710001
      *IGH001 <-- F                                                     01720001
012200                                                                  01730001
012300 01 TABLA-DE-MESES.                                               01740001
012400    05 FILLER                     PIC X(09)     VALUE '  ENERO  '.01750001
012500    05 FILLER                     PIC X(09)     VALUE ' FEBRERO '.01760001
012600    05 FILLER                     PIC X(09)     VALUE '  MARZO  '.01770001
012700    05 FILLER                     PIC X(09)     VALUE '  ABRIL  '.01780001
012800    05 FILLER                     PIC X(09)     VALUE '  MAYO   '.01790001
012900    05 FILLER                     PIC X(09)     VALUE '  JUNIO  '.01800001
013000    05 FILLER                     PIC X(09)     VALUE '  JULIO  '.01810001
013100    05 FILLER                     PIC X(09)     VALUE ' AGOSTO  '.01820001
013200    05 FILLER                     PIC X(09)     VALUE 'SETIEMBRE'.01830001
013300    05 FILLER                     PIC X(09)     VALUE ' OCTUBRE '.01840001
013400    05 FILLER                     PIC X(09)     VALUE 'NOVIEMBRE'.01850001
013500    05 FILLER                     PIC X(09)     VALUE 'DICIEMBRE'.01860001
013600 01 FILLER             REDEFINES  TABLA-DE-MESES.                 01870001
013700    05 MESES           OCCURS 12  PIC X(09).                      01880001
013800                                                                  01890001
013900     COPY BGECSLD.                                                01900001
014000                                                                  01910001
014100     COPY BQECPDT.                                                01920001
014200                                                                  01930001
014300     COPY FCMAWCTS.                                               01940001
014400                                                                  01950001
014500****      COPY BATCH PARA EL ACCESO A TABLAS CORPORATIVAS     ****01960001
014600 COPY TCWC1000.                                                   01970001
014700                                                                  01980001
014800****      COPY BATCH PARA EL ACCESO A TIPO DE CAMBIO          ****01990001
014900 COPY TCWC1250.                                                   02000001
015000                                                                  02010001
015100****   COPY PARA LAS ESCALAS DEL FONDO DE SEGURO DE DEPOSITO  ****02020001
015200 COPY TCTC5540.                                                   02030001
015300                                                                  02040001
      *                                                                 02050001
      *IGH001 --> I                                                     02060001
       01 TABLA-CORASUS.                                                02070001
          05 TAB-CORASUS OCCURS 20000 TIMES.                            02080001
             10 WSV-CLASIFIC   PIC X(05).                               02090001
             10 WSV-CORASU     PIC X(03).                               02100001
      *IGH001 <-- F                                                     02110001
      *                                                                 02120001
015400 01  TABLA-IMP-MON.                                               02130001
015500      03 TABLA-M         OCCURS 19 TIMES.                         02140001
015600         05 ACU-IMPTME          PIC S9(15)V99            COMP-3.  02150001
015700                                                                  02160001
015800 01  TABLA-IMPORTES.                                              02170001
      *201305033-INI                                                    02180001
015900*     03 TABLA-T         OCCURS 15 TIMES.                         02190001
      *201305033-FIN                                                    02200001
015900      03 TABLA-T         OCCURS 18 TIMES.                         02210001
016000         05 ACU-NUMNAT          PIC 9(9)                 COMP-3.  02220001
016100         05 ACU-IMPNAT          PIC 9(16)V99             COMP-3.  02230001
016200         05 ACU-NUMSFL          PIC 9(9)                 COMP-3.  02240001
016300         05 ACU-IMPSFL          PIC 9(16)V99             COMP-3.  02250001
016400         05 ACU-NUMCFL          PIC 9(9)                 COMP-3.  02260001
016500         05 ACU-IMPCFL          PIC 9(16)V99             COMP-3.  02270001
016600         05 ACU-NUMTOT          PIC 9(9)                 COMP-3.  02280001
016700         05 ACU-IMPTOT          PIC 9(16)V99             COMP-3.  02290001
016800         05 ACU-OFICIN          PIC 9(03).                        02300001
016900                                                                  02310001
      *IGH001 --> I                                                     02320001
017000*01 W-CORASU  PIC X(03).                                          02330001
017100*   88 NAT  VALUE   'F00', 'F01', 'F02'.                          02340001
017200*   88 SFL  VALUE   'M01', 'M12', 'M32', 'M35'.                   02350001
      *   88 CFL  VALUE   'M02', 'M10', 'M13', 'M14', 'M15', 'M16',     02360001
      *                   'M17', 'M20', 'M26', 'M27', 'M29', 'M30',     02370001
      *                   'M31', 'M34', 'M37', 'M38', 'M39', 'M40',     02380001
      *                   'M41', 'M42', 'M44'.                          02390001
      *   88 CFL-2 VALUE  'M03', 'M04', 'M05', 'M06', 'M07', 'M08',     02400001
      *                   'M18', 'M19', 'M21', 'M22', 'M23', 'M24',     02410001
      *                   'M25', 'M28', 'M33', 'M36', 'M43', 'M45',     02420001
      *                   'M46', 'M47', 'M48'.                          02430001
       01 W-CORASU  PIC X(05).                                          02440001
          88 NAT     VALUE   '00001'.                                   02450001
          88 SFL     VALUE   '00002'.                                   02460001
          88 CFL     VALUE   '00003'.                                   02470001
          88 CFL-2   VALUE   '00004'.                                   02480001
          88 NO-REG  VALUE   '99999'.                                   02490001
      *IGH001 <-- F                                                     02500001
018200*201011112-FIN                                                    02510001
018300 01  TABLA-MONEDAS.                                               02520001
018400     02 TAB-MONEDAS.                                              02530001
      *6792016011-05-I                                                  02540001
018500*       03 FILLER        PIC X(31) VALUE '01S O L E S   S/.'.     02550001
018500        03 FILLER        PIC X(31) VALUE '01S O L E S   S/ '.     02560001
      *6792016011-05-F                                                  02570001
018600        03 FILLER        PIC X(31) VALUE '02DOLARES     US$'.     02580001
018700        03 FILLER        PIC X(31) VALUE '03MARCOS      DM '.     02590001
018800        03 FILLER        PIC X(31) VALUE '05FRANCO SUIZOFSW'.     02600001
018900        03 FILLER        PIC X(31) VALUE '32L ESTERLINASLE '.     02610001
019000        03 FILLER        PIC X(31) VALUE '14FLORINES HOLFHL'.     02620001
019100        03 FILLER        PIC X(31) VALUE '09LIRAS ITALIALIT'.     02630001
019200        03 FILLER        PIC X(31) VALUE '12DOLARES CAN CAN'.     02640001
019300        03 FILLER        PIC X(31) VALUE '08FRANCO FRANCFF '.     02650001
019400        03 FILLER        PIC X(31) VALUE '04ECU         ECU'.     02660001
019500        03 FILLER        PIC X(31) VALUE '02DOLARES     US$'.     02670001
019600        03 FILLER        PIC X(31) VALUE '34EURO        EUR'.     02680001
019700        03 FILLER        PIC X(31) VALUE '13YEN         JPY'.     02690001
019800        03 FILLER        PIC X(31) VALUE '14YUAN        CNY'.     02700001
019900        03 FILLER        PIC X(31) VALUE '15PESO MEX    MXN'.     02710001
020000        03 FILLER        PIC X(31) VALUE '16PESO CHI    CLP'.     02720001
020100        03 FILLER        PIC X(31) VALUE '17PESO COL    COP'.     02730001
020200        03 FILLER        PIC X(31) VALUE '36REALES BRAS BRL'.     02740001
020300        03 FILLER        PIC X(31) VALUE '02DOLARES     US$'.     02750001
      *IGH001 --> I                                                     02760001
020400*    02 T-MONEDAS        REDEFINES TAB-MONEDAS.                   02770001
020500*       03 FILLER        OCCURS 19 TIMES.                         02780001
020600*          04 T-CODMON   PIC 99.                                  02790001
020700*          04 T-MONEDA   PIC X(12).                               02800001
020800*          04 T-SIMBOLO  PIC XXX.                                 02810001
020900*          04 T-TIPCAM   PIC 9(8)V9(6).                           02820001
       01 T-MONEDAS.                                                    02830001
          03 FILLER        OCCURS 19 TIMES.                             02840001
             04 T-SIMBOLO  PIC XXX.                                     02850001
             04 T-TIPCAM   PIC 9(8)V9(6).                               02860001
      *IGH001 <-- F                                                     02870001
021000                                                                  02880001
021100 01 ARRAY-PRINCIPAL.                                              02890001
021200    05 VALORES-N1      OCCURS 17 TIMES.                           02900001
021300       10 TOPE1                   PIC 9(09)V99.                   02910001
021400       10 TOPE2                   PIC 9(09)V99.                   02920001
021500                                                                  02930001
021600 01 INDICE-ARRAY.                                                 02940001
021700    05 L                          PIC 9(02).                      02950001
021800                                                                  02960001
021900 01 CONSTANTES.                                                   02970001
022000    05 CTE-PROGRAMA               PIC X(08)  VALUE 'BG3C321W'.    02980001
022100    05 CTE-OPCION                 PIC X(01)  VALUE '1'.           02990001
022200    05 CTE-TABLA-0554             PIC X(04)  VALUE '0554'.        03000001
022300    05 CTE-ENTIDAD                PIC X(04)  VALUE '0011'.        03010001
022400    05 CTE-R                      PIC X(01)  VALUE 'R'.           03020001
022500    05 CTE-1                      PIC 9(01)  VALUE 1.             03030001
022600    05 CTE-TC9C1000               PIC X(08)  VALUE 'TC9C1000'.    03040001
022700    05 CTE-TC9C1810               PIC X(08)  VALUE 'TC9C1810'.    03050001
      *IGH001 --> I                                                     03060001
          05 CTE-TC9C1809               PIC X(08)  VALUE 'TC9C1809'.    03070001
      *IGH001 <-- F                                                     03080001
022800    05 CTE-CLAVE.                                                 03090001
022900       10 CTE-SECUENCIA           PIC 9(02).                      03100001
023000    05 CTE-I                      PIC X(01)  VALUE 'I'.           03110001
023100                                                                  03120001
023200 01 DATOS-OUTPUT.                                                 03130001
023300    05 WS-LIMITE-I                PIC 9(09)V9(02).                03140001
023400    05 WS-LIMITE-S                PIC 9(09)V9(02).                03150001
023500    05 FILLER                     PIC X(228).                     03160001
023600                                                                  03170001
023700 01 VARIABLES-EDITADAS.                                           03180001
023800    05 ED-SECUENCIA               PIC Z9.                         03190001
023900    05 ED-LIMITE-I                PIC ZZZ,ZZZ,ZZ9.99.             03200001
024000    05 ED-LIMITE-S                PIC ZZZ,ZZZ,ZZ9.99.             03210001
024100                                                                  03220001
024200 01 TIT-1.                                                        03230001
024300    05 FILLER                     PIC X(01)  VALUE SPACES.        03240001
024400    05 TEX-PLAZA                  PIC X(04)  VALUE SPACES.        03250001
024500    05 FILLER                     PIC X(36)  VALUE                03260001
024600       ' SUPERINTENDENCIA DE BANCA Y SEGUROS'.                    03270001
024700                                                                  03280001
024800 01 TIT-2.                                                        03290001
024900    05 FILLER                     PIC X(32)  VALUE ' '.           03300001
025000    05 FILLER                     PIC X(72)  VALUE                03310001
025100       'INSTITUCION: BBV BANCO CONTINENTAL   CODIGO: 011'.        03320001
025200    05 FILLER                     PIC X(10)  VALUE 'ANEXO N.13'.  03330001
025300                                                                  03340001
025400 01 TIT-3.                                                        03350001
025500    05 FILLER                     PIC X(40)  VALUE ' '.           03360001
025600    05 FILLER                     PIC X(43)  VALUE                03370001
025700       'DEPOSITOS SEGUN ESCALA DE MONTOS'.                        03380001
025800    05 I-SISTEMA                  PIC X(27)  VALUE ' '.           03390001
025900    05 I-MONEDA                   PIC X(03)  VALUE ' '.           03400001
026000                                                                  03410001
026100 01 TIT-4.                                                        03420001
026200    05 FILLER                     PIC X(43)  VALUE ' '.           03430001
026300    05 FILLER                     PIC X(03)  VALUE 'AL '.         03440001
026400    05 I-DIA                      PIC 99.                         03450001
026500    05 FILLER                     PIC X(04)  VALUE ' DE '.        03460001
026600    05 I-MES                      PIC X(09).                      03470001
026700    05 FILLER                     PIC X(04)  VALUE ' DE '.        03480001
026800    05 I-SIGLO                    PIC X(02).                      03490001
026900    05 I-ANO                      PIC 99.                         03500001
027000                                                                  03510001
027100 01 TIT-5.                                                        03520001
027200    05 FILLER                     PIC X(48)  VALUE ' BG3C321W'.   03530001
027300    05 FILLER                     PIC X(17)  VALUE                03540001
      *6792016011-05-I                                                  03550001
027400*      '(EN NUEVOS SOLES)'.                                       03560001
027400       '(EN SOLES)       '.                                       03570001
      *6792016011-05-F                                                  03580001
027500                                                                  03590001
027600* 200601187-INI                                                   03600001
027700    05 FILLER                     PIC X(42)  VALUE SPACES.        03610001
027800    05 T5-TIPO                    PIC X(25)  VALUE SPACES.        03620001
027900* 200601187-FIN                                                   03630001
028000                                                                  03640001
028100 01 TIT-6.                                                        03650001
028200    05 FILLER                     PIC X(31)  VALUE                03660001
028300       '     E S C A L A S'.                                      03670001
028400    05 FILLER                     PIC X(43)  VALUE                03680001
028500       'PERSONAS  NATURALES  --------------PERSONAS'.             03690001
028600    05 FILLER                     PIC X(41)  VALUE                03700001
028700       ' JURIDICAS------------     T O T A L E S'.                03710001
028800                                                                  03720001
028900 01 TIT-6A.                                                       03730001
029000    05 FILLER                     PIC X(31)  VALUE ' '.           03740001
029100    05 FILLER                     PIC X(42)  VALUE                03750001
029200       '                     PRIVADAS SIN FINES   '.              03760001
029300    05 FILLER                     PIC X(42)  VALUE                03770001
029400       ' OTRAS               '.                                   03780001
029500                                                                  03790001
029600 01 TIT-7.                                                        03800001
029700    05 FILLER                     PIC X(31)  VALUE ' '.           03810001
029800    05 FILLER                     PIC X(42)  VALUE                03820001
029900       '                           DE LUCRO       '.              03830001
030000    05 FILLER                     PIC X(42)  VALUE                03840001
030100       '                     '.                                   03850001
030200                                                                  03860001
030300 01 TIT-8.                                                        03870001
030400    05 FILLER                     PIC X(31)  VALUE ' '.           03880001
030500    05 FILLER                     PIC X(44)  VALUE                03890001
030600       'NRO.  /  M O N T O     NRO.  /  M O N T O   '.            03900001
030700    05 FILLER                     PIC X(47)  VALUE                03910001
030800       '  NRO.  /  M O N T O      NRO.  /  M O N T O'.            03920001
                                                                        03920101
   GLB 01 TIT-8A.                                                       03921002
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.           03922002
   GLB    05 FILLER                     PIC X(08)  VALUE                03924002
   GLB       'ESCALAS;'.                                                03925002
   GLB    05 FILLER                     PIC X(07)  VALUE                03927002
   GLB       'NRO-PN;'.                                                 03928002
   GLB    05 FILLER                     PIC X(09)  VALUE                03929102
   GLB       'MONTO-PN;'.                                               03929202
   GLB    05 FILLER                     PIC X(08)  VALUE                03929402
   GLB       'NRO-SFL;'.                                                03929502
   GLB    05 FILLER                     PIC X(10)  VALUE                03929702
   GLB       'MONTO-SFL;'.                                              03929802
   GLB    05 FILLER                     PIC X(08)  VALUE                03930002
   GLB       'NRO-CFL;'.                                                03930102
   GLB    05 FILLER                     PIC X(10)  VALUE                03930302
   GLB       'MONTO-CFL;'.                                              03930402
   GLB    05 FILLER                     PIC X(10)  VALUE                03930602
   GLB       'NRO-TOTAL;'.                                              03930702
   GLB    05 FILLER                     PIC X(12)  VALUE                03930902
   GLB       'MONTO-TOTAL;'.                                            03931002
030900                                                                  03932001
031000 01 RAYA.                                                         03940001
031100    05 FILLER                     PIC X(62)  VALUE ALL '*'.       03950001
031200    05 FILLER                     PIC X(62)  VALUE ALL '*'.       03960001
031300                                                                  03970001
032900                                                                  04130001
031400 01 DETA.                                                         04131001
   GLB    05 X-1                        PIC X(04)  VALUE ';DE '.        04132002
031600    05 I-TOPE1                    PIC ZZZZ,ZZ9.99.                04133001
031700    05 X-2                        PIC X(03)  VALUE ' A '.         04134001
031800    05 I-TOPE2                    PIC ZZZZ,ZZZ.ZZ.                04135001
031900    05 R-TOPE2          REDEFINES                                 04136001
032000       I-TOPE2                    PIC X(11).                      04137001
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.           04138002
032200    05 I-NUMNAT                   PIC ZZZ,ZZZ,ZZ9.                04139002
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.           04139102
   GLB    05 I-IMPNAT                   PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.     04139202
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.           04139302
032400    05 I-NUMSFL                   PIC ZZZ,ZZZ,ZZ9.                04139401
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.           04139502
   GLB    05 I-IMPSFL                   PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.     04139602
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.           04139702
032600    05 I-NUMCFL                   PIC ZZZ,ZZZ,ZZ9.                04139801
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.           04139902
   GLB    05 I-IMPCFL                   PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.     04140002
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.           04140102
032800    05 I-NUMTOT                   PIC ZZZ,ZZZ,ZZ9.                04140201
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.           04140302
   GLB    05 I-IMPTOT                   PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.     04140402
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.           04140502
   GLB*01 DETA-B.                                                       04141002
   GLB*   05 FILLER                     PIC X(29)  VALUE SPACES.        04150002
   GLB*   05 I-IMPNAT                   PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.     04160002
   GLB*   05 FILLER                     PIC X(02)  VALUE SPACES.        04170002
   GLB*   05 I-IMPSFL                   PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99B.    04180002
   GLB*   05 FILLER                     PIC X(02)  VALUE SPACES.        04190002
   GLB*   05 I-IMPCFL                   PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99B.    04200002
   GLB*   05 FILLER                     PIC X(02)  VALUE SPACES.        04210002
   GLB*   05 I-IMPTOT                   PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99B.    04220002
033900                                                                  04230001
034000 01 DET-ME.                                                       04240001
034100    05 FILLER                     PIC X(29)  VALUE                04250001
034200       ';TOTAL EN MONEDA DE ORIGEN   '.                           04260002
          05 FILLER                     PIC X(01)  VALUE ';'.           04261002
034300    05 I-MONEDA-DET               PIC X(4)   VALUE SPACES.        04270001
          05 FILLER                     PIC X(01)  VALUE ';'.           04271002
034400    05 I-IMPTME                   PIC ZZZZZ,ZZZ,ZZZ,ZZ9.99.       04280001
          05 FILLER                     PIC X(01)  VALUE ';'.           04281002
034500                                                                  04290001
034600 01 DET-TC.                                                       04300001
034700    05 FILLER                     PIC X(29)  VALUE                04310001
034800       ';TIPO DE CAMBIO S.B.S. USADO '.                           04320002
          05 FILLER                     PIC X(01)  VALUE ';'.           04321002
034900    05 FILLER                     PIC X(10)  VALUE SPACES.        04330001
          05 FILLER                     PIC X(01)  VALUE ';'.           04331002
035000    05 I-TIPCAM                   PIC ZZZ,ZZZ.999999.             04340001
035100 05 FILLER                     PIC X(01)  VALUE ';'.              04350002
035200 01 DET-ER.                                                       04360001
035300    05 FILLER                     PIC X(01)  VALUE ' '.           04370001
035400    05 I-QUECTA                   PIC X(23)  VALUE ' '.           04380001
035500    05 I-CENTRO                   PIC X(04).                      04390001
035600    05 FILLER                     PIC X(01)  VALUE '-'.           04400001
035700    05 I-CUENTA                   PIC X(10)BB.                    04410001
035800    05 FILLER                     PIC X(22)  VALUE                04420001
035900       'TIENE CLASE DE CUENTA '.                                  04430001
036000    05 I-CORASU                   PIC X(03).                      04440001
036100                                                                  04450001
036200 01  SW-E1DQANX0-STATUS               PIC X(01) VALUE 'N'.        04460001
036300     88  FIN-E1DQANX0                           VALUE 'S'.        04470001
036400                                                                  04480001
036500 01 FECHA-CR.                                                     04490001
036600    05 CRDIA                      PIC 99.                         04500001
036700    05 FILLER                     PIC X(01).                      04510001
036800    05 CRMES                      PIC 99.                         04520001
036900    05 FILLER                     PIC X(01).                      04530001
037000    05 CRANO                      PIC 99.                         04540001
037100                                                                  04550001
037200 01 FECHAX.                                                       04560001
037300    05 ANOX                       PIC 99     VALUE ZEROS.         04570001
037400    05 MESX                       PIC 99     VALUE ZEROS.         04580001
037500    05 DIAX                       PIC 99     VALUE ZEROS.         04590001
037600                                                                  04600001
037700 01 CRDATE.                                                       04610001
037800    05 CRD-DIA                    PIC 99     VALUE ZEROS.         04620001
037900    05 FILLER                     PIC X(01)  VALUE '/'.           04630001
038000    05 CRD-MES                    PIC 99     VALUE ZEROS.         04640001
038100    05 FILLER                     PIC X(01)  VALUE '/'.           04650001
038200    05 CRD-ANO                    PIC 99     VALUE ZEROS.         04660001
038300 02 W-FECHA-PROCESO.                                              04670001
038400    04 W-FECHA-SS                 PIC XX     VALUE  SPACES.       04680001
038500    04 W-FECHA-AA                 PIC XX     VALUE  SPACES.       04690001
038600    04 W-GUION-1                  PIC X      VALUE  '-'.          04700001
038700    04 W-FECHA-MM                 PIC XX     VALUE  SPACES.       04710001
038800    04 W-GUION-2                  PIC X      VALUE  '-'.          04720001
038900    04 W-FECHA-DD                 PIC XX     VALUE  SPACES.       04730001
039000 01  CTE-CONSTANTES.                                              04740001
039100     05  CTE-19        PIC X(02)   VALUE '19'.                    04750001
039200     05  CTE-20        PIC X(02)   VALUE '20'.                    04760001
039300                                                                  04770001
039400 LINKAGE SECTION.                                                 04780001
039500*---------------*                                                 04790001
039600 01 LK-PARAMETROS.                                                04800001
039700    05 LK-LONGIT                  PIC S9(04)               COMP.  04810001
039800    05 LK-FECHA.                                                  04820001
039900       10 LK-FECANO               PIC 9(02).                      04830001
040000       10 LK-FECMES               PIC 9(02).                      04840001
040100       10 LK-FECDIA               PIC 9(02).                      04850001
040200* 200601187-INI                                                   04860001
040300    05 FILLER                     PIC X(01).                      04870001
040400    05 LK-IND                     PIC X(02).                      04880001
040500* 200601187-FIN                                                   04890001
040600                                                                  04900001
040700 PROCEDURE DIVISION USING LK-PARAMETROS.                          04910001
040800*--------------------------------------*                          04920001
040900     PERFORM 00100-PROCESO-INICIO.                                04930001
041000     PERFORM 00200-PROCESO-PRINCIPAL.                             04940001
041100     PERFORM 00300-PROCESO-FINAL.                                 04950001
041200     STOP RUN.                                                    04960001
041300                                                                  04970001
041400 00100-PROCESO-INICIO.                                            04980001
041500*--------------------*                                            04990001
041600     IF LK-LONGIT = ZEROS OR                                      05000001
041700        LK-FECHA NOT NUMERIC             OR                       05010001
041800        LK-FECMES < 01 OR LK-FECMES > 12 OR                       05020001
041900        LK-FECDIA < 01 OR LK-FECDIA > 31                          05030001
042000        ACCEPT FECHAX                  FROM DATE                  05040001
042100        MOVE ANOX                      TO LK-FECANO               05050001
042200        MOVE MESX                      TO LK-FECMES               05060001
042300        MOVE DIAX                      TO LK-FECDIA               05070001
042400     END-IF.                                                      05080001
042500                                                                  05090001
042600     MOVE LK-FECANO                    TO CRD-ANO W-FECHA-AA.     05100001
042700     MOVE LK-FECMES                    TO CRD-MES W-FECHA-MM.     05110001
042800     MOVE LK-FECDIA                    TO CRD-DIA W-FECHA-DD.     05120001
042900                                                                  05130001
043000* 200601187-INI                                                   05140001
043100     IF LK-LONGIT < 7                                             05150001
043200        MOVE SPACES  TO LK-IND                                    05160001
043300     END-IF.                                                      05170001
043400*    DISPLAY 'TIPO DE REPORTE ' LK-IND                            05180001
043500* 200601187-FIN                                                   05190001
043600                                                                  05200001
043700     INITIALIZE ARRAY-PRINCIPAL                                   05210001
043800                INDICE-ARRAY.                                     05220001
043900                                                                  05230001
044000     PERFORM 00110-CARGA-ARRAY VARYING L FROM 1 BY 1 UNTIL L > 17.05240001
044100                                                                  05250001
      *IGH001 --> I                                                     05260001
044200*    PERFORM VARYING II FROM 1 BY 1 UNTIL II > 19                 05270001
044300*       MOVE 1     TO T-TIPCAM (II)                               05280001
044400*    END-PERFORM.                                                 05290001
044500*                                                                 05300001
044600*    PERFORM 00120-CARGAR-TABLA-DE-MONEDAS.                       05310001
           PERFORM  VARYING I FROM 1 BY 1 UNTIL  I > 19                 05320001
                                                                        05330001
                    MOVE SPACES             TO T-SIMBOLO   (I)          05340001
                    MOVE ZEROES             TO T-TIPCAM    (I)          05350001
           END-PERFORM                                                  05360001
      *                                                                 05370001
           MOVE 'PEN'                        TO T-SIMBOLO(1)            05380001
           MOVE 1                            TO T-TIPCAM(1)             05390001
      *IGH001 --> F                                                     05400001
044700                                                                  05410001
044800     MOVE CRDATE                       TO FECHA-CR                05420001
044900     MOVE CRDIA                        TO I-DIA                   05430001
045000     MOVE MESES (CRMES)                TO I-MES                   05440001
045100     MOVE CRANO                        TO I-ANO                   05450001
045200                                                                  05460001
045300     IF CRANO LESS 80                                             05470001
045400        MOVE CTE-20     TO I-SIGLO W-FECHA-SS                     05480001
045500     ELSE                                                         05490001
045600        MOVE CTE-19     TO I-SIGLO W-FECHA-SS                     05500001
045700     END-IF                                                       05510001
045800                                                                  05520001
045900     INITIALIZE TABLA-IMPORTES.                                   05530001
046000     INITIALIZE TABLA-IMP-MON.                                    05540001
046100                                                                  05550001
046200     OPEN INPUT  E1DQANX0                                         05560001
046300          OUTPUT S1DQAX13.                                        05570001
046400                                                                  05580001
046500* 200601187-INI                                                   05590001
046600     OPEN OUTPUT S2DQTOTL.                                        05600001
046700* 200601187-FIN                                                   05610001
      *IGH001 --> I                                                     05620001
           SET WSS-E1CORASU-FIN-NO           TO TRUE                    05630001
           OPEN INPUT  E1CORASU.                                        05640001
           IF FILE-STATUS NOT = ZEROES                                  05650001
              MOVE 'E1CORASU'                TO WSV-ARCHIVO             05660001
              MOVE FILE-STATUS               TO WSV-STATUS              05670001
              MOVE 'OPEN'                    TO WSV-OPERACION           05680001
              PERFORM 900900-ERROR-ARCHIVO                              05690001
           END-IF                                                       05700001
           READ E1CORASU                                                05710001
           AT END                                                       05720001
              SET WSS-E1CORASU-FIN-SI   TO TRUE                         05730001
              IF WSA-LEIDOS-E1CORASU = ZEROES                           05740001
                 MOVE '16'              TO RETURN-CODE                  05750001
                 DISPLAY 'ARCHIVO DE CORASUS VACIO'                     05760001
                 MOVE 'E1CORASU'        TO WSV-ARCHIVO                  05770001
                 MOVE FILE-STATUS       TO WSV-STATUS                   05780001
                 MOVE 'READ'            TO WSV-OPERACION                05790001
                 PERFORM 900900-ERROR-ARCHIVO                           05800001
              END-IF                                                    05810001
           NOT AT END                                                   05820001
              IF FILE-STATUS = ZEROES                                   05830001
                 ADD 1                    TO WSA-LEIDOS-E1CORASU        05840001
              ELSE                                                      05850001
                 MOVE '16'                 TO RETURN-CODE               05860001
                 MOVE 'E1CORASU'           TO WSV-ARCHIVO               05870001
                 MOVE FILE-STATUS          TO WSV-STATUS                05880001
                 MOVE 'READ'               TO WSV-OPERACION             05890001
                 SET WSS-E1CORASU-FIN-SI   TO TRUE                      05900001
                 PERFORM 900900-ERROR-ARCHIVO                           05910001
              END-IF                                                    05920001
           END-READ.                                                    05930001
      *                                                                 05940001
           MOVE   1                          TO I-CORA                  05950001
           PERFORM UNTIL I-CORA > 20000                                 05960001
              MOVE SPACES                    TO WSV-CLASIFIC(I-CORA)    05970001
                                                WSV-CORASU(I-CORA)      05980001
              ADD 1                          TO I-CORA                  05990001
           END-PERFORM                                                  06000001
      *                                                                 06010001
           MOVE   1                          TO I-CORA                  06020001
           MOVE   0                          TO I-CORULT                06030001
           PERFORM UNTIL I-CORA > 10000 OR WSS-E1CORASU-FIN-SI          06040001
              MOVE REG-E1CORASU(101:05)      TO WSV-CLASIFIC(I-CORA)    06050001
              MOVE REG-E1CORASU(071:03)      TO WSV-CORASU(I-CORA)      06060001
              ADD 1                          TO I-CORA                  06070001
                                                I-CORULT                06080001
              READ E1CORASU                                             06090001
              AT END                                                    06100001
                 SET WSS-E1CORASU-FIN-SI   TO TRUE                      06110001
              NOT AT END                                                06120001
                 IF FILE-STATUS = ZEROES                                06130001
                    ADD 1                    TO WSA-LEIDOS-E1CORASU     06140001
                 ELSE                                                   06150001
                    MOVE '16'                 TO RETURN-CODE            06160001
                    MOVE 'E1CORASU'           TO WSV-ARCHIVO            06170001
                    MOVE FILE-STATUS          TO WSV-STATUS             06180001
                    MOVE 'READ'               TO WSV-OPERACION          06190001
                    SET WSS-E1CORASU-FIN-SI   TO TRUE                   06200001
                    PERFORM 900900-ERROR-ARCHIVO                        06210001
                 END-IF                                                 06220001
              END-READ                                                  06230001
           END-PERFORM.                                                 06240001
      *IGH001 <-- F                                                     06250001
046800                                                                  06260001
046900 00110-CARGA-ARRAY.                                               06270001
047000*-----------------*                                               06280001
047100     INITIALIZE TCWC1000.                                         06290001
047200     MOVE L                            TO CTE-SECUENCIA.          06300001
047300     MOVE CTE-OPCION                   TO W100-CDOPCIO.           06310001
047400     MOVE CTE-TABLA-0554               TO W100-CDTABLA.           06320001
047500     MOVE CTE-ENTIDAD                  TO W100-STBANCO.           06330001
047600     MOVE CTE-R                        TO W100-TCCIDIOM.          06340001
047700     MOVE CTE-CLAVE                    TO W100-CLAVTG.            06350001
047800     MOVE CTE-1                        TO W100-NUCLAVE.           06360001
047900     CALL 'TC9C1000'                   USING TCWC1000.            06370001
048000     EVALUATE W100-CDRETORN                                       06380001
048100     WHEN '00'                                                    06390001
048200          MOVE W100-CONTOCUR(1)        TO DATOS-OUTPUT            06400001
048300          MOVE CTE-SECUENCIA           TO ED-SECUENCIA            06410001
048400          MOVE WS-LIMITE-I             TO TOPE1(L)                06420001
048500                                          ED-LIMITE-I             06430001
048600          MOVE WS-LIMITE-S             TO TOPE2(L)                06440001
048700                                          ED-LIMITE-S             06450001
048800*         DISPLAY ' CODIGO DE SECUENCIA : ' ED-SECUENCIA          06460001
048900*         DISPLAY ' LIMITE INFERIOR     : ' ED-LIMITE-I           06470001
049000*         DISPLAY ' LIMITE SUPERIOR     : ' ED-LIMITE-S           06480001
049100     WHEN OTHER                                                   06490001
049200          DISPLAY ' DATO NO EXISTE EN TABLAS CORPORATIVAS'        06500001
049300          DISPLAY ' CLAVE ENVIADA       : ' CTE-CLAVE             06510001
049400          DISPLAY ' LIMITE INFERIOR     : ' ED-LIMITE-I           06520001
049500          DISPLAY ' LIMITE SUPERIOR     : ' ED-LIMITE-S           06530001
049600     END-EVALUATE.                                                06540001
049700*                                                                 06550001
      *IGH001 --> I                                                     06560001
049800*00120-CARGAR-TABLA-DE-MONEDAS.                                   06570001
049900*-----------------------------                                    06580001
050000*    INITIALIZE TCWC1250.                                         06590001
050100*                                                                 06600001
050200*    MOVE '2'          TO W125-CDOPCIO.                           06610001
050300*    MOVE '0011'       TO W125-TCCENTIT.                          06620001
050400*    MOVE 'S'          TO W125-INDDIVBI.                          06630001
050500*    MOVE '20'         TO W125-FHCAMBIO(7:2).                     06640001
050600*    MOVE LK-FECANO    TO W125-FHCAMBIO(9:2).                     06650001
050700*    MOVE LK-FECMES    TO W125-FHCAMBIO(4:2).                     06660001
050800*    MOVE LK-FECDIA    TO W125-FHCAMBIO(1:2).                     06670001
050900*    MOVE '.'          TO W125-FHCAMBIO(3:1) W125-FHCAMBIO(6:1).  06680001
051000*    CALL CTE-TC9C1810    USING TCWC1250.                         06690001
051100*                                                                 06700001
051200*    EVALUATE W125-CDRETORN                                       06710001
051300*       WHEN '00'                                                 06720001
051400*            PERFORM 00121-GUARDA-CAMBIOS                         06730001
051500*       WHEN OTHER                                                06740001
051600*            DISPLAY 'ERROR RUTINA TC9C1810: TIPO DE CAMBIO '     06750001
051700*            DISPLAY 'W125-FHCAMBIO    = ' W125-FHCAMBIO          06760001
051800*            DISPLAY 'W125-CDRETORN    = ' W125-CDRETORN          06770001
051900*            DISPLAY 'W125-SQLCODE     = ' W125-SQLCODE           06780001
052000*            DISPLAY 'W125-TABLENAME   = ' W125-TABLENAME         06790001
052100*            DISPLAY 'W125-SQLERRM-LON = ' W125-SQLERRM-LON       06800001
052200*            DISPLAY 'PROCESO CANCELADO - AVISAR A SISTEMAS'      06810001
052300*            MOVE 16 TO RETURN-CODE                               06820001
052400*            STOP RUN                                             06830001
052500*    END-EVALUATE.                                                06840001
052600*    DISPLAY '- FECHA        = ' W125-FHCAMBIO.                   06850001
052700*                                                                 06860001
052800*00121-GUARDA-CAMBIOS.                                            06870001
052900*---------------------*                                           06880001
053000*                                                                 06890001
053100*    PERFORM VARYING II FROM 1 BY 1 UNTIL II > 50                 06900001
053200*       EVALUATE W125-CDDIVISC (II)                               06910001
053300*          WHEN 'USD' COMPUTE T-TIPCAM (2) = W125-CAMBBAJO (II) / 06920001
053400*                                            W125-CTUNICAM (II)   06930001
053500*          WHEN 'DEM' COMPUTE T-TIPCAM (3) = W125-CAMBBAJO (II) / 06940001
053600*                                            W125-CTUNICAM (II)   06950001
053700*          WHEN 'CHF' COMPUTE T-TIPCAM (4) = W125-CAMBBAJO (II) / 06960001
053800*                                            W125-CTUNICAM (II)   06970001
053900*          WHEN 'GBP' COMPUTE T-TIPCAM (5) = W125-CAMBBAJO (II) / 06980001
054000*                                            W125-CTUNICAM (II)   06990001
054100*          WHEN 'NLG' COMPUTE T-TIPCAM (6) = W125-CAMBBAJO (II) / 07000001
054200*                                            W125-CTUNICAM (II)   07010001
054300*          WHEN 'ITL' COMPUTE T-TIPCAM (7) = W125-CAMBBAJO (II) / 07020001
054400*                                            W125-CTUNICAM (II)   07030001
054500*          WHEN 'CAD' COMPUTE T-TIPCAM (8) = W125-CAMBBAJO (II) / 07040001
054600*                                            W125-CTUNICAM (II)   07050001
054700*          WHEN 'FRF' COMPUTE T-TIPCAM (9) = W125-CAMBBAJO (II) / 07060001
054800*                                            W125-CTUNICAM (II)   07070001
054900*          WHEN 'XEU' COMPUTE T-TIPCAM (10) = W125-CAMBBAJO (II) /07080001
055000*                                            W125-CTUNICAM (II)   07090001
055100*          WHEN 'ARS' COMPUTE T-TIPCAM (11) = W125-CAMBBAJO (II) /07100001
055200*                                            W125-CTUNICAM (II)   07110001
055300*          WHEN 'EUR' COMPUTE T-TIPCAM (12) = W125-CAMBBAJO (II) /07120001
055400*                                            W125-CTUNICAM (II)   07130001
055500*          WHEN 'JPY' COMPUTE T-TIPCAM (13) = W125-CAMBBAJO (II) /07140001
055600*                                            W125-CTUNICAM (II)   07150001
055700*          WHEN 'CNY' COMPUTE T-TIPCAM (14) = W125-CAMBBAJO (II) /07160001
055800*                                            W125-CTUNICAM (II)   07170001
      *          WHEN 'MXN' COMPUTE T-TIPCAM (15) = W125-CAMBBAJO (II) /07180001
      *                                            W125-CTUNICAM (II)   07190001
      *          WHEN 'CLP' COMPUTE T-TIPCAM (16) = W125-CAMBBAJO (II) /07200001
      *                                            W125-CTUNICAM (II)   07210001
      *          WHEN 'COP' COMPUTE T-TIPCAM (17) = W125-CAMBBAJO (II) /07220001
      *                                            W125-CTUNICAM (II)   07230001
056100*          WHEN 'BRL' COMPUTE T-TIPCAM (18) = W125-CAMBBAJO (II) /07240001
056200*                                            W125-CTUNICAM (II)   07250001
056300*        END-EVALUATE                                             07260001
056400*    END-PERFORM.                                                 07270001
      *----------------------------------------------------------------*07280001
      *                    200060-CARGAR-CAMBIO                        *07290001
      *                                                                *07300001
      * - BUSCA EL TIPO DE CAMBIO EN SOLES DE LA DIVISA EXTRANJERA     *07310001
      *   EN RUTINA DE TIPO DE CAMBIO.                                 *07320001
      *----------------------------------------------------------------*07330001
       200060-CARGAR-CAMBIO.                                            07340001
      *---------------------                                            07350001
           INITIALIZE TCWC1250                                          07360001
                                                                        07370001
           MOVE '0011'                     TO W125-TCCENTIT             07380001
           MOVE 1                          TO W125-CDOPCIO              07390001
           MOVE 'S'                        TO W125-INDDIVBI             07400001
           MOVE WSV-DIVISA                 TO W125-CDDIVISS             07410001
                                                                        07420001
           MOVE ALL '.'                    TO W125-FHCAMBIO             07430001
           MOVE I-SIGLO                    TO W125-FHCAMBIO (7:2)       07440001
           MOVE LK-FECANO                  TO W125-FHCAMBIO (9:2)       07450001
           MOVE LK-FECMES                  TO W125-FHCAMBIO (4:2)       07460001
           MOVE LK-FECDIA                  TO W125-FHCAMBIO (1:2)       07470001
                                                                        07480001
           CALL CTE-TC9C1809               USING TCWC1250               07490001
                                                                        07500001
           EVALUATE W125-CDRETORN                                       07510001
           WHEN ZEROES                                                  07520001
                COMPUTE T-TIPCAM (I)  = W125-CAMBBAJO(1) /              07530001
                                        W125-CTUNICAM(1)                07540001
           WHEN 45                                                      07550001
           WHEN 50                                                      07560001
                MOVE 0                     TO T-TIPCAM (I)              07570001
           WHEN OTHER                                                   07580001
                 MOVE 16 TO RETURN-CODE                                 07590001
                 DISPLAY '**** ERROR EN EL PROGRAMA BG3C321W ****'      07600001
                 DISPLAY 'ERROR EN LA RUTINA TC9C1809'                  07610001
                 DISPLAY 'RETORNO DE RUTINA:   ' W125-CDRETORN          07620001
                 DISPLAY '   W125-CDOPCIO  :   ' W125-CDOPCIO           07630001
                 DISPLAY '   W125-TCCENTIT :   ' W125-TCCENTIT          07640001
                 DISPLAY '   W125-FHCAMBIO :   ' W125-FHCAMBIO          07650001
                 DISPLAY 'RETORNO DE PROGRAMA: ' RETURN-CODE            07660001
                 DISPLAY '***************************************'      07670001
                 PERFORM 999999-STOP-RUN                                07680001
           END-EVALUATE                                                 07690001
           .                                                            07700001
      *                                                                 07710001
      *IGH001 <-- F                                                     07720001
056500                                                                  07730001
056600 00200-PROCESO-PRINCIPAL.                                         07740001
056700*-----------------------*                                         07750001
056800     PERFORM 00210-LEE-MAESTRO                                    07760001
056900                                                                  07770001
057000     PERFORM UNTIL FIN-E1DQANX0                                   07780001
057100        PERFORM 00220-SELECCIONA-REGISTRO                         07790001
057200        IF SI-REG-SEL                                             07800001
057300           PERFORM 00230-PROCESO                                  07810001
057400        END-IF                                                    07820001
057500        PERFORM 00210-LEE-MAESTRO                                 07830001
057600     END-PERFORM.                                                 07840001
057700                                                                  07850001
057800     IF SW-SI-ACUMULA                                             07860001
057900        PERFORM 00235-QUIEBRE-CENTRAL                             07870001
058000     END-IF.                                                      07880001
058100                                                                  07890001
058200 00210-LEE-MAESTRO.                                               07900001
058300*-----------------*                                               07910001
058400     READ E1DQANX0  AT END                                        07920001
058500          SET FIN-E1DQANX0 TO TRUE                                07930001
058600     END-READ.                                                    07940001
058700                                                                  07950001
058800     IF NOT FIN-E1DQANX0                                          07960001
058900        ADD 1              TO CT-LEIDOS                           07970001
059000     END-IF.                                                      07980001
059100                                                                  07990001
      *                                                                 08000001
059200 00220-SELECCIONA-REGISTRO.                                       08010001
059300*-------------------------*                                       08020001
059400     SET SI-REG-SEL        TO TRUE.                               08030001
059500                                                                  08040001
059600     EVALUATE ANX-IND-FILE                                        08050001
059700        WHEN '1'    PERFORM 00221-VALIDA-FILE1                    08060001
059800        WHEN '2'    PERFORM 00222-VALIDA-FILE2                    08070001
      *200902167-I                                                      08080001
059900*       WHEN '3'    PERFORM 00223-VALIDA-FILE3                    08090001
      *200902167-F                                                      08100001
060000        WHEN OTHER  SET NO-REG-SEL   TO TRUE                      08110001
060100     END-EVALUATE.                                                08120001
060200                                                                  08130001
060300 00221-VALIDA-FILE1.                                              08140001
060400*------------------*                                              08150001
060500     MOVE ANX-DATA   TO BGECSLD.                                  08160001
060600                                                                  08170001
      *IGH001 --> I                                                     08180001
060700*    MOVE SLD-CORASU TO W-CORASU                                  08190001
           MOVE SLD-CORASU TO WSV-TEMPO-CORASU                          08200001
           PERFORM 900000-BUSCA-CORASU                                  08210001
           MOVE WSV-CLASIFIC(I-CORA)   TO W-CORASU                      08220001
      *IGH001 <-- F                                                     08230001
060800                                                                  08240001
060900     IF (NAT OR CFL OR SFL) AND SLD-INDESTA NOT EQUAL 'C' AND     08250001
061000        SLD-SALDO-DISPUE >= 0                                     08260001
061100        IF SLD-PRODUCTO EQUAL '08'                                08270001
061200           IF SLD-FECHA-PROVEN > W-FECHA-PROCESO                  08280001
061300              CONTINUE                                            08290001
061400           ELSE                                                   08300001
061500              SET NO-REG-SEL       TO TRUE                        08310001
061600           END-IF                                                 08320001
061700        END-IF                                                    08330001
061800     ELSE                                                         08340001
061900        IF CFL-2 AND SLD-INDESTA NOT EQUAL 'C' AND                08350001
062000           SLD-SALDO-DISPUE >= 0                                  08360001
062100           CONTINUE                                               08370001
062200        ELSE                                                      08380001
062300           SET NO-REG-SEL       TO TRUE                           08390001
062400        END-IF                                                    08400001
062500     END-IF.                                                      08410001
062600                                                                  08420001
062700 00222-VALIDA-FILE2.                                              08430001
062800*------------------*                                              08440001
062900     MOVE ANX-DATA           TO BQECPDT.                          08450001
      *IGH001 --> I                                                     08460001
063000*    MOVE PDT-CORASU         TO W-CORASU.                         08470001
           MOVE PDT-CORASU TO WSV-TEMPO-CORASU.                         08480001
           PERFORM 900000-BUSCA-CORASU.                                 08490001
           MOVE WSV-CLASIFIC(I-CORA)   TO W-CORASU.                     08500001
      *IGH001 <-- F                                                     08510001
063100     IF (NAT OR CFL OR SFL OR CFL-2) AND                          08520001
              PDT-PRODUCTO NOT EQUAL '09'                               08530001
063200        SET SI-REG-SEL       TO TRUE                              08540001
063300     ELSE                                                         08550001
063400        SET NO-REG-SEL       TO TRUE                              08560001
063500     END-IF.                                                      08570001
063600*200902167-I                                                      08580001
063700*00223-VALIDA-FILE3.                                              08590001
063800*------------------*                                              08600001
063900*    MOVE ANX-DATA           TO REG-MAESTRO-INV.                  08610001
064000*    IF L-SITCTA = 'C'           OR                               08620001
064100*       L-OFICTA = ZEROS         OR                               08630001
064200*       L-SALACT IS NOT NUMERIC  OR                               08640001
064300*      (L-SALACT = ZEROS AND L-NUMCTA < '2000000000') OR          08650001
064400*      (L-MONCTA NOT = '1' AND '2' AND '8' AND '9' AND '3')       08660001
064500*       SET NO-REG-SEL       TO TRUE                              08670001
064600*    END-IF.                                                      08680001
064700*200902167-F                                                      08690001
                                                                        08700001
064800 00230-PROCESO.                                                   08710001
064900*-------------*                                                   08720001
      *201001166-INI                                                    08730001
      *    IF ANX-CENTRAL NOT = W-ANT-CENTRAL OR                        08740001
           MOVE ANX-CENTRAL TO W-NUE-CENTRAL                            08750001
           EVALUATE ANX-IND-FILE                                        08760001
             WHEN '1'  MOVE SLD-CORASU   TO W-NUE-CORASU                08770001
             WHEN '2'  MOVE PDT-CORASU   TO W-NUE-CORASU                08780001
      *200902167-I                                                      08790001
      *      WHEN '3'  MOVE 'F00'        TO W-NUE-CORASU                08800001
      *200902167-F                                                      08810001
           END-EVALUATE                                                 08820001
065000     IF W-CLAVE-N NOT = W-CLAVE-A     OR                          08830001
065100       (ANX-CENTRAL = '00000000'      OR                          08840001
065200        ANX-CENTRAL = SPACES)                                     08850001
              IF W-ANT-CENTRAL = W-NUE-CENTRAL                          08860001
                 DISPLAY 'ERROR CLIENTE - CORASUS: ' W-ANT-CENTRAL      08870001
              END-IF                                                    08880001
065300        IF SW-SI-ACUMULA                                          08890001
065400           PERFORM 00235-QUIEBRE-CENTRAL                          08900001
065500        END-IF                                                    08910001
065600        MOVE ZEROS           TO W-SALDO-CENTRAL                   08920001
065700        SET SW-NO-ACUMULA    TO TRUE                              08930001
065800        MOVE ANX-CENTRAL     TO W-ANT-CENTRAL                     08940001
065900        EVALUATE ANX-IND-FILE                                     08950001
066000          WHEN '1'  MOVE SLD-CORASU   TO W-ANT-CORASU             08960001
066100          WHEN '2'  MOVE PDT-CORASU   TO W-ANT-CORASU             08970001
      *200902167-I                                                      08980001
066200*         WHEN '3'  MOVE 'F00'        TO W-ANT-CORASU             08990001
      *200902167-F                                                      09000001
066300        END-EVALUATE                                              09010001
066400     END-IF.                                                      09020001
066500                                                                  09030001
066600     EVALUATE ANX-IND-FILE                                        09040001
066700          WHEN '1'  PERFORM 00231-ACUMULA-SLD1                    09050001
066800          WHEN '2'  PERFORM 00232-ACUMULA-SLD2                    09060001
      *200902167-I                                                      09070001
066900*         WHEN '3'  PERFORM 00233-ACUMULA-SLD3                    09080001
      *200902167-F                                                      09090001
067000     END-EVALUATE.                                                09100001
067100                                                                  09110001
067200******************************************************************09120001
067300*  A C U M U L A  SALDO    FILE 1                                *09130001
067400******************************************************************09140001
067500 00231-ACUMULA-SLD1.                                              09150001
067600*------------------*                                              09160001
      *IGH001 --> I                                                     09170001
067700*    MOVE 1    TO I.                                              09180001
067800*    IF SLD-DIVISA = 'PEN'    MOVE  1 TO  I.                      09190001
067900*    IF SLD-DIVISA = 'USD'    MOVE  2 TO  I.                      09200001
068000*    IF SLD-DIVISA = 'DEM'    MOVE  3 TO  I.                      09210001
068100*    IF SLD-DIVISA = 'CHF'    MOVE  4 TO  I.                      09220001
068200*    IF SLD-DIVISA = 'GBP'    MOVE  5 TO  I.                      09230001
068300*    IF SLD-DIVISA = 'NLG'    MOVE  6 TO  I.                      09240001
068400*    IF SLD-DIVISA = 'ITL'    MOVE  7 TO  I.                      09250001
068500*    IF SLD-DIVISA = 'CAD'    MOVE  8 TO  I.                      09260001
068600*    IF SLD-DIVISA = 'FRF'    MOVE  9 TO  I.                      09270001
068700*    IF SLD-DIVISA = 'XEU'    MOVE 10 TO  I.                      09280001
068800*    IF SLD-DIVISA = 'ARS'    MOVE 11 TO  I.                      09290001
068900*    IF SLD-DIVISA = 'EUR'    MOVE 12 TO  I.                      09300001
069000*    IF SLD-DIVISA = 'JPY'    MOVE 13 TO  I.                      09310001
069100*    IF SLD-DIVISA = 'CNY'    MOVE 14 TO  I.                      09320001
069100*    IF SLD-DIVISA = 'MXN'    MOVE 15 TO  I.                      09330001
069100*    IF SLD-DIVISA = 'CLP'    MOVE 16 TO  I.                      09340001
069200*    IF SLD-DIVISA = 'COP'    MOVE 17 TO  I.                      09350001
069300*    IF SLD-DIVISA = 'BRL'    MOVE 18 TO  I.                      09360001
           MOVE 0  TO I                                                 09370001
           PERFORM UNTIL I > 19                                         09380001
                   OR T-SIMBOLO(I) = SLD-DIVISA                         09390001
                ADD  1          TO I                                    09400001
                IF T-SIMBOLO(I) = SPACES                                09410001
                   MOVE  SLD-DIVISA TO T-SIMBOLO(I)                     09420001
                                       WSV-DIVISA                       09430001
                   PERFORM 200060-CARGAR-CAMBIO                         09440001
                END-IF                                                  09450001
           END-PERFORM                                                  09460001
      *IGH001 <-- F                                                     09470001
069400                                                                  09480001
069500     IF I = 1                                                     09490001
069600        MOVE SLD-SALDO-DISPUE TO W-SOLES                          09500001
069700     ELSE                                                         09510001
069800        COMPUTE W-SOLES ROUNDED =                                 09520001
069900                SLD-SALDO-DISPUE * T-TIPCAM (I)                   09530001
070000     END-IF.                                                      09540001
070100                                                                  09550001
070200     IF I > 0 AND I < 20                                          09560001
070300        ADD  SLD-SALDO-DISPUE TO ACU-IMPTME (I)                   09570001
070400     ELSE                                                         09580001
070500        DISPLAY 'ERROR EN MONEDA ' I SLD-DIVISA                   09590001
070600     END-IF.                                                      09600001
070700*201305033-INI                                                    09610001
070800*    IF W-SOLES > TOPE2 (14)                                      09620001
070800     IF W-SOLES > TOPE2 (17)                                      09630001
070700*201305033-FIN                                                    09640001
070900        DISPLAY 'MAYOR AL TOPE MAX ' W-SOLES ' ' SLD-CENTRO-ALTA  09650001
071000            SLD-CUENTA ' ' SLD-PRODUCTO ' ' SLD-DIVISA ' '        09660001
071100            SLD-CORASU '-' ANX-CENTRAL                            09670001
071300        ADD W-SOLES       TO W-SALDO-CENTRAL                      09680001
071200     ELSE                                                         09690001
071300        ADD W-SOLES       TO W-SALDO-CENTRAL                      09700001
071400     END-IF.                                                      09710001
071500                                                                  09720001
071600     SET SW-SI-ACUMULA TO TRUE.                                   09730001
071700                                                                  09740001
071800******************************************************************09750001
071900*  A C U M U L A  SALDO    FILE 2                                *09760001
072000******************************************************************09770001
072100 00232-ACUMULA-SLD2.                                              09780001
072200*------------------*                                              09790001
072300     COMPUTE PDT-IMPORTE-PAG = 0.00 - PDT-IMPORTE-PAG.            09800001
      *IGH001 --> I                                                     09810001
072400*    MOVE 1   TO I.                                               09820001
072500*    IF PDT-DIVISA = 'PEN'    MOVE  1 TO  I.                      09830001
072600*    IF PDT-DIVISA = 'USD'    MOVE  2 TO  I.                      09840001
072700*    IF PDT-DIVISA = 'DEM'    MOVE  3 TO  I.                      09850001
072800*    IF PDT-DIVISA = 'CHF'    MOVE  4 TO  I.                      09860001
072900*    IF PDT-DIVISA = 'GBP'    MOVE  5 TO  I.                      09870001
073000*    IF PDT-DIVISA = 'NLG'    MOVE  6 TO  I.                      09880001
073100*    IF PDT-DIVISA = 'ITL'    MOVE  7 TO  I.                      09890001
073200*    IF PDT-DIVISA = 'CAD'    MOVE  8 TO  I.                      09900001
073300*    IF PDT-DIVISA = 'FRF'    MOVE  9 TO  I.                      09910001
073400*    IF PDT-DIVISA = 'XEU'    MOVE 10 TO  I.                      09920001
073500*    IF PDT-DIVISA = 'ARS'    MOVE 11 TO  I.                      09930001
073600**   IF PDT-DIVISA = 'EUR'    MOVE 12 TO  I.                      09940001
073700*    IF PDT-DIVISA = 'JPY'    MOVE 13 TO  I.                      09950001
073800*    IF PDT-DIVISA = 'CNY'    MOVE 14 TO  I.                      09960001
073800*    IF PDT-DIVISA = 'MXN'    MOVE 15 TO  I.                      09970001
073800*    IF PDT-DIVISA = 'CLP'    MOVE 16 TO  I.                      09980001
073900*    IF PDT-DIVISA = 'COP'    MOVE 17 TO  I.                      09990001
074000*    IF PDT-DIVISA = 'BRL'    MOVE 18 TO  I.                      10000001
           MOVE 0  TO I                                                 10010001
           PERFORM UNTIL I > 19                                         10020001
                   OR T-SIMBOLO(I) = PDT-DIVISA                         10030001
                ADD  1          TO I                                    10040001
                IF T-SIMBOLO(I) = SPACES                                10050001
                   MOVE  PDT-DIVISA TO T-SIMBOLO(I)                     10060001
                                       WSV-DIVISA                       10070001
                   PERFORM 200060-CARGAR-CAMBIO                         10080001
                END-IF                                                  10090001
           END-PERFORM                                                  10100001
      *IGH001 <-- F                                                     10110001
074100                                                                  10120001
074200     IF I = 1                                                     10130001
074300        MOVE PDT-IMPORTE-PAG  TO W-SOLES                          10140001
074400     ELSE                                                         10150001
074500        COMPUTE W-SOLES ROUNDED =                                 10160001
074600                PDT-IMPORTE-PAG  * T-TIPCAM (I)                   10170001
074700     END-IF.                                                      10180001
074800                                                                  10190001
074900     IF I > 0 AND I < 20                                          10200001
075000        ADD  PDT-IMPORTE-PAG  TO ACU-IMPTME (I)                   10210001
075100     ELSE                                                         10220001
070500        DISPLAY 'ERROR EN MONEDA ' I PDT-DIVISA                   10230001
075300     END-IF.                                                      10240001
075400*201305033-INI                                                    10250001
075500*    IF W-SOLES > TOPE2 (14)                                      10260001
      *201305033-FIN                                                    10270001
075500     IF W-SOLES > TOPE2 (17)                                      10280001
075600        DISPLAY 'MAYOR AL TOPE MAX ' W-SOLES ' ' PDT-CENTRO       10290001
075700            PDT-CUENTA ' ' PDT-PRODUCTO ' ' PDT-DIVISA ' '        10300001
075800            PDT-CORASU ' ' ANX-CENTRAL                            10310001
076000        ADD W-SOLES       TO W-SALDO-CENTRAL                      10320001
075900     ELSE                                                         10330001
076000        ADD W-SOLES       TO W-SALDO-CENTRAL                      10340001
076100     END-IF.                                                      10350001
076200                                                                  10360001
076300     SET SW-SI-ACUMULA TO TRUE.                                   10370001
076400*200902167-I                                                      10380001
076500******************************************************************10390001
076600*  A C U M U L A  SALDO    FILE 3                                *10400001
076700******************************************************************10410001
076800*00233-ACUMULA-SLD3.                                              10420001
076900*------------------*                                              10430001
077000                                                                  10440001
077100*    MOVE 1   TO I.                                               10450001
077200*    IF L-MONCTA = '1'  OR '9'  MOVE 1  TO  I.                    10460001
077300*    IF L-MONCTA = '2'  OR '8'  MOVE 2  TO  I.                    10470001
077400*    IF L-MONCTA = '3'          MOVE 12 TO  I.                    10480001
077500*                                                                 10490001
077600*    IF I = 1                                                     10500001
077700*       MOVE L-SALACT         TO W-SOLES                          10510001
077800*    ELSE                                                         10520001
077900*       COMPUTE W-SOLES ROUNDED =                                 10530001
078000*               L-SALACT         * T-TIPCAM (I)                   10540001
078100*    END-IF.                                                      10550001
078200*                                                                 10560001
078300*    IF I > 0 AND I < 20                                          10570001
078400*       ADD  L-SALACT         TO ACU-IMPTME (I)                   10580001
078500*    ELSE                                                         10590001
078600*       DISPLAY 'ERROR EN MONEDA ' I                              10600001
078700*    END-IF.                                                      10610001
078800*                                                                 10620001
078900*    IF W-SOLES > TOPE2 (14)                                      10630001
079000*       DISPLAY 'MAYOR AL TOPE MAX ' W-SOLES ' ' L-NUMCTA         10640001
079100*           L-MONCTA '-' ANX-CENTRAL                              10650001
079200*    ELSE                                                         10660001
079300*       ADD W-SOLES       TO W-SALDO-CENTRAL                      10670001
079400*    END-IF.                                                      10680001
079500*                                                                 10690001
079600*    SET SW-SI-ACUMULA TO TRUE.                                   10700001
079700*200902167-F                                                      10710001
079800******************************************************************10720001
079900*  A C U M U L A  TOTALES  POR CLIENTE                           *10730001
080000******************************************************************10740001
080100 00235-QUIEBRE-CENTRAL.                                           10750001
080200*---------------------*                                           10760001
      *IGH001 --> I                                                     10770001
080300*    MOVE W-ANT-CORASU    TO W-CORASU.                            10780001
           MOVE W-ANT-CORASU    TO WSV-TEMPO-CORASU.                    10790001
           PERFORM 900000-BUSCA-CORASU.                                 10800001
           MOVE WSV-CLASIFIC(I-CORA)   TO W-CORASU.                     10810001
      *IGH001 <-- F                                                     10820001
080400                                                                  10830001
080500     IF NAT                                                       10840001
080600        PERFORM LLENO-NAT VARYING J FROM 1 BY 1 UNTIL J > 17      10850001
080700     ELSE                                                         10860001
080800        IF SFL                                                    10870001
080900           PERFORM LLENO-SFL VARYING J FROM 1 BY 1 UNTIL J > 17   10880001
081000        ELSE                                                      10890001
081100           IF CFL OR CFL-2                                        10900001
081200            PERFORM LLENO-CFL VARYING J FROM 1 BY 1 UNTIL J > 17. 10910001
081300                                                                  10920001
081400 LLENO-NAT.                                                       10930001
081500*---------*                                                       10940001
081600     IF W-SALDO-CENTRAL NOT > TOPE2 (J)  OR                       10950001
081700        J = 17                                                    10960001
      *       DISPLAY 'CLIENTN - ' ANX-CENTRAL                          10970001
      *       DISPLAY 'CUENTAN - ' SLD-CUENTA                           10980001
      *       DISPLAY 'TITULAN - ' SLD-DIAS-DEU                         10990001
      *201305033-INI                                                    11000001
081800        ADD  1                TO ACU-NUMNAT (J)  ACU-NUMTOT (J)   11010001
081900                                 ACU-NUMNAT (18) ACU-NUMTOT (18)  11020001
081800*       ADD  SLD-DIAS-DEU     TO ACU-NUMNAT (J)  ACU-NUMTOT (J)   11030001
081900*                                ACU-NUMNAT (18) ACU-NUMTOT (18)  11040001
      *201305033-FIN                                                    11050001
082000        ADD  W-SALDO-CENTRAL  TO ACU-IMPNAT (J)  ACU-IMPTOT (J)   11060001
082100                                 ACU-IMPNAT (18) ACU-IMPTOT (18)  11070001
082200        MOVE 20               TO J.                               11080001
082300                                                                  11090001
082400 LLENO-SFL.                                                       11100001
082500*---------*                                                       11110001
082600     IF W-SALDO-CENTRAL NOT > TOPE2 (J)  OR                       11120001
082700        J = 17                                                    11130001
      *201305033-INI                                                    11140001
082800        ADD  1                TO ACU-NUMSFL (J)  ACU-NUMTOT (J)   11150001
082900                                 ACU-NUMSFL (18) ACU-NUMTOT (18)  11160001
      *       DISPLAY 'CLIENTS - ' ANX-CENTRAL                          11170001
      *       DISPLAY 'CUENTAS - ' SLD-CUENTA                           11180001
      *       DISPLAY 'TITULAS - ' SLD-DIAS-DEU                         11190001
      *       ADD  SLD-DIAS-DEU     TO ACU-NUMSFL (J)  ACU-NUMTOT (J)   11200001
      *                                ACU-NUMSFL (18) ACU-NUMTOT (18)  11210001
      *201305033-FIN                                                    11220001
083000        ADD  W-SALDO-CENTRAL  TO ACU-IMPSFL (J)  ACU-IMPTOT (J)   11230001
083100                                 ACU-IMPSFL (18) ACU-IMPTOT (18)  11240001
083200        MOVE 20               TO J.                               11250001
083300                                                                  11260001
083400 LLENO-CFL.                                                       11270001
083500*---------*                                                       11280001
083600     IF W-SALDO-CENTRAL NOT > TOPE2 (J)  OR                       11290001
083700        J = 17                                                    11300001
      *201305033-INI                                                    11310001
083800        ADD  1                TO ACU-NUMCFL (J)  ACU-NUMTOT (J)   11320001
083900                                 ACU-NUMCFL (18) ACU-NUMTOT (18)  11330001
083800*       ADD  SLD-DIAS-DEU     TO ACU-NUMCFL (J)  ACU-NUMTOT (J)   11340001
083900*                                ACU-NUMCFL (18) ACU-NUMTOT (18)  11350001
      *201305033-FIN                                                    11360001
084000        ADD  W-SALDO-CENTRAL  TO ACU-IMPCFL (J)  ACU-IMPTOT (J)   11370001
084100                                 ACU-IMPCFL (18) ACU-IMPTOT (18)  11380001
084200        MOVE 20               TO J.                               11390001
084300                                                                  11400001
084400 00300-PROCESO-FINAL.                                             11410001
084500*-------------------*                                             11420001
084600                                                                  11430001
084700* 200601187-INI.                                                  11440001
084800     EVALUATE LK-IND                                              11450001
084900       WHEN '01'  MOVE 'CUENTAS CORRIENTES' TO T5-TIPO            11460001
085000       WHEN '02'  MOVE 'CONTIAHORRO AUTOMATICO' TO T5-TIPO        11470001
085100       WHEN '03'  MOVE 'P L A Z O S           ' TO T5-TIPO        11480001
085200       WHEN '15'  MOVE 'F A C T O R I N G     ' TO T5-TIPO        11490001
085300       WHEN '08'  MOVE 'C. B. M. E.           ' TO T5-TIPO        11500001
085400       WHEN '14'  MOVE 'CERTIFICADO DEPOSITO'   TO T5-TIPO        11510001
085500       WHEN '07'  MOVE 'COMP.TIEMPO DE SERVICIOS' TO T5-TIPO      11520001
085600       WHEN '18'  MOVE 'CHEQUES CERTIFICADOS '  TO T5-TIPO        11530001
085700       WHEN OTHER MOVE SPACES                   TO T5-TIPO        11540001
085800     END-EVALUATE.                                                11550001
085900* 200601187-FIN.                                                  11560001
086000                                                                  11570001
086100     PERFORM 00310-TITULOS.                                       11580001
086200                                                                  11590001
086300* 200601187-INI.                                                  11600001
086400     INITIALIZE REG-S2DQTOTL.                                     11610001
086500     MOVE LK-IND                  TO SAL1-MODALIDAD               11620001
086600     MOVE '20'                    TO SAL1-AAMMDD (1:2)            11630001
086700     MOVE LK-FECANO               TO SAL1-AAMMDD (3:2)            11640001
086800     MOVE LK-FECMES               TO SAL1-AAMMDD (5:2)            11650001
086900     MOVE LK-FECDIA               TO SAL1-AAMMDD (7:2)            11660001
087000* 200601187-FIN.                                                  11670001
087100                                                                  11680001
087200     PERFORM 00320-IMPRIME-TOTAL                                  11690001
087300                           VARYING J FROM 1 BY 1 UNTIL J > 18.    11700001
087400                                                                  11710001
087500     PERFORM VARYING I FROM 1 BY 1 UNTIL I > 19                   11720001
087600        MOVE T-SIMBOLO (I)  TO I-MONEDA-DET                       11730001
087700*       DISPLAY I-MONEDA-DET ' ' ACU-IMPTME (I)                   11740001
087800        IF ACU-IMPTME (I) > 0                                     11750001
087900           IF I > 1                                               11760001
088000              MOVE ACU-IMPTME (I)   TO I-IMPTME                   11770001
088100              MOVE T-TIPCAM   (I)   TO I-TIPCAM                   11780001
088200              MOVE DET-ME           TO REG-SAL                    11790001
088300*             MOVE 2                TO WS-NRO                     11800002
088400              PERFORM LLAMAR-IMP                                  11810001
088500              MOVE DET-TC           TO REG-SAL                    11820001
088600*             MOVE 2                TO WS-NRO                     11830002
088700              PERFORM LLAMAR-IMP                                  11840001
088800           END-IF                                                 11850001
088900        END-IF                                                    11860001
089000     END-PERFORM.                                                 11870001
089100                                                                  11880001
089200     PERFORM 00330-CERRAR-ARCHIVOS.                               11890001
089300                                                                  11900001
089400 00310-TITULOS.                                                   11910001
089500*-------------*                                                   11920001
   GLB*    MOVE ZEROS                        TO WS-LIN.                 11930002
   GLB*    WRITE REG-SALIDA  FROM TIT-1      AFTER PAGE.                11940002
   GLB*    WRITE REG-SALIDA  FROM TIT-2      AFTER 1. MOVE 1 TO WS-NRO. 11950002
   GLB*    WRITE REG-SALIDA  FROM TIT-3      AFTER 1. MOVE 1 TO WS-NRO. 11960002
   GLB*    WRITE REG-SALIDA  FROM TIT-4      AFTER 1. MOVE 1 TO WS-NRO. 11970002
   GLB*    WRITE REG-SALIDA  FROM TIT-5      AFTER 1. MOVE 1 TO WS-NRO. 11980002
   GLB*    WRITE REG-SALIDA  FROM RAYA       AFTER 2. MOVE 2 TO WS-NRO. 11990002
   GLB*    WRITE REG-SALIDA  FROM TIT-6      AFTER 1. MOVE 1 TO WS-NRO. 12000002
   GLB*    WRITE REG-SALIDA  FROM TIT-6A     AFTER 1. MOVE 1 TO WS-NRO. 12010002
   GLB*    WRITE REG-SALIDA  FROM TIT-7      AFTER 1. MOVE 1 TO WS-NRO. 12020002
   GLB*    WRITE REG-SALIDA  FROM TIT-8      AFTER 1. MOVE 1 TO WS-NRO. 12030002
   GLB*    WRITE REG-SALIDA  FROM RAYA       AFTER 1. MOVE 1 TO WS-NRO. 12040002
   GLB     WRITE REG-SALIDA  FROM TIT-1.                                12041002
   GLB     WRITE REG-SALIDA  FROM TIT-2.                                12042002
   GLB     WRITE REG-SALIDA  FROM TIT-3.                                12043002
   GLB     WRITE REG-SALIDA  FROM TIT-4.                                12044002
   GLB     WRITE REG-SALIDA  FROM TIT-5.                                12045002
   GLB     WRITE REG-SALIDA  FROM TIT-8A.                               12046002
090800                                                                  12050001
090900 00320-IMPRIME-TOTAL.                                             12060001
091000*-------------------*                                             12070001
091100     IF J = 18                                                    12080001
   GLB        MOVE ';   '        TO X-1                                 12081002
091200        MOVE 'TOTALES'     TO R-TOPE2                             12090001
091300* 200601187-INI                                                   12100001
091400        MOVE SPACES        TO SAL1-MODALIDAD                      12110001
091500     ELSE                                                         12120001
091600        MOVE ';DE'         TO X-1                                 12130001
091700        MOVE ' A'          TO X-2                                 12140001
091800        MOVE TOPE1     (J) TO I-TOPE1                             12150001
091900                              SAL1-ESC-DESDE                      12160001
092000        IF J = 17                                                 12170001
092100           MOVE 'MAS '     TO R-TOPE2                             12180001
092200           MOVE 9999999999999.99 TO SAL1-ESC-HASTA                12190001
092300        ELSE                                                      12200001
092400           MOVE TOPE2  (J) TO I-TOPE2                             12210001
092500                              SAL1-ESC-HASTA                      12220001
092600        END-IF                                                    12230001
092700     END-IF.                                                      12240001
092800                                                                  12250001
092900     MOVE ACU-NUMNAT (J) TO I-NUMNAT SAL1-PN-NUMERO               12260001
093000     MOVE ACU-IMPNAT (J) TO I-IMPNAT SAL1-PN-MONTO                12270001
093100     MOVE ACU-NUMSFL (J) TO I-NUMSFL SAL1-PJ-SFL-NUMERO           12280001
093200     MOVE ACU-IMPSFL (J) TO I-IMPSFL SAL1-PJ-SFL-MONTO            12290001
093300     MOVE ACU-NUMCFL (J) TO I-NUMCFL SAL1-PJ-OTR-NUMERO           12300001
093400     MOVE ACU-IMPCFL (J) TO I-IMPCFL SAL1-PJ-OTR-MONTO            12310001
093500     MOVE ACU-NUMTOT (J) TO I-NUMTOT SAL1-TOTALES-NUMERO          12320001
093600     MOVE ACU-IMPTOT (J) TO I-IMPTOT SAL1-TOTALES-MONTO           12330001
093700                                                                  12340001
093800     WRITE REG-S2DQTOTL.                                          12350001
093900* 200601187-FIN.                                                  12360001
094000                                                                  12370001
   GLB*    IF J = 18                                                    12380002
   GLB*       MOVE RAYA            TO REG-SAL                           12390002
   GLB*       MOVE 2               TO WS-NRO                            12400002
   GLB*       PERFORM LLAMAR-IMP                                        12410002
   GLB*    END-IF.                                                      12420002
094600                                                                  12430001
094700     MOVE DETA               TO REG-SAL                           12440001
   GLB*    MOVE 1                  TO WS-NRO                            12450002
094900     PERFORM LLAMAR-IMP                                           12460001
095000                                                                  12470001
   GLB*    MOVE DETA-B             TO REG-SAL                           12480002
   GLB*    MOVE 1                  TO WS-NRO                            12490002
   GLB*    PERFORM LLAMAR-IMP                                           12500002
095400                                                                  12510001
   GLB*    IF J = 18                                                    12520002
   GLB*       MOVE RAYA            TO REG-SAL                           12530002
   GLB*       MOVE 2               TO WS-NRO                            12540002
   GLB*       PERFORM LLAMAR-IMP                                        12550002
   GLB*    END-IF.                                                      12560002
096000                                                                  12570001
   GLB*    MOVE SPACES             TO DETA.                             12580002
   GLB     INITIALIZE DETA                                              12581002
           .                                                            12582001
096200*                                                                 12590001
096300 LLAMAR-IMP.                                                      12600001
096400*----------*                                                      12610001
   GLB*    ADD  WS-NRO                       TO WS-LIN.                 12620002
   GLB*    WRITE REG-SALIDA                  AFTER WS-NRO.              12630002
   GLB*    IF WS-LIN > 60                                               12640002
   GLB*       PERFORM 00310-TITULOS                                     12650002
   GLB*    END-IF.                                                      12660002
   GLB     WRITE REG-SALIDA                                             12670002
           .                                                            12671001
      *IGH001 --> I                                                     12680001
      *                                                                 12690001
       900000-BUSCA-CORASU.                                             12700001
      *-------------------*                                             12710001
           MOVE 1                      TO I-CORA                        12720001
           PERFORM UNTIL WSV-TEMPO-CORASU = WSV-CORASU(I-CORA)          12730001
                   OR I-CORA > I-CORULT                                 12740001
              ADD 1                    TO I-CORA                        12750001
           END-PERFORM                                                  12760001
                                                                        12770001
           IF I-CORA > I-CORULT                                         12780001
              DISPLAY '**** ERROR EN EL PROGRAMA BG3C321W ****'         12790001
              DISPLAY 'CORASU NO ENCONTRADO: ' WSV-TEMPO-CORASU         12800001
              DISPLAY 'RETORNO DE CANCELACION: ' RETURN-CODE            12810001
              DISPLAY 'CUENTA                : ' ANX-CCC                12820001
              DISPLAY '***************************************'         12830001
              MOVE WSV-TEMPO-CORASU     TO WSV-CORASU(I-CORA)           12840001
              MOVE '99999'              TO WSV-CLASIFIC(I-CORA)         12850001
              MOVE I-CORA               TO I-CORULT                     12860001
           END-IF.                                                      12870001
      *                                                                 12880001
       900900-ERROR-ARCHIVO.                                            12890001
      *--------------------*                                            12900001
                                                                        12910001
           DISPLAY '**** ERROR EN EL PROGRAMA BG3C321W ****'            12920001
           DISPLAY 'ARCHIVO:     ' WSV-ARCHIVO                          12930001
           DISPLAY 'FILE STATUS: ' WSV-STATUS                           12940001
           DISPLAY 'OPERACION:   ' WSV-OPERACION                        12950001
           DISPLAY 'COD RETORNO: ' RETURN-CODE                          12960001
           DISPLAY '***************************************'            12970001
           PERFORM 999999-STOP-RUN.                                     12980001
       999999-STOP-RUN.                                                 12990001
      *---------------*                                                 13000001
           STOP RUN.                                                    13010001
      *IGH001 <-- F                                                     13020001
                                                                        13030001
097100 00330-CERRAR-ARCHIVOS.                                           13040001
097200*---------------------*                                           13050001
097300     CLOSE E1DQANX0                                               13060001
097400           S1DQAX13                                               13070001
097500* 200601187-INI                                                   13080001
097600     CLOSE S2DQTOTL.                                              13090001
097700* 200601187-FIN                                                   13100001
097500*IGH001 -->                                                       13110001
           CLOSE E1CORASU.                                              13120001
097700*IGH001 <-- F                                                     13130001
097800     DISPLAY 'LEIDOS ' CT-LEIDOS.                                 13140001
097900                                                                  13150001
098000******************************************************************13160001
098100*                    FINAL DEL PROGRAMA                          *13170001
098200******************************************************************13180001
