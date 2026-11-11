000100 IDENTIFICATION DIVISION.                                         00010001
000200*-----------------------*                                         00020001
000300 PROGRAM-ID.  BG3C3170.                                           00030001
000400                                                                  00040001
000500******************************************************************00050001
000600*   ** DISTRIBUCION DE LOS DEPOSITOS SEGUN ESCALA DE MONTOS **   *00060001
000700*   **                 A N E X O   N. 8                     **   *00070001
000800*   ** '09' --> CHEQUES CERTIFICADOS.                       **   *00080001
000900******************************************************************00090001
001000*        L O G    D E   M O D I F I C A C I O N E S              *00100001
001100******************************************************************00110001
001200*  AUTOR   FECHA          DESCRIPCION                            *00120001
001300*  -----   -----          -----------                            *00130001
001400*  JOSE CARLOS MONTOYA ARENAS.                                   *00140001
001500*  JCMA1   19/03/1999     SE INCLUYE EL ACCESO A TABLAS CORPORA- *00150001
001600*                         TIVAS PARA OBTENER LOS VALORES DE LAS  *00160001
001700*                         ESCALAS RESPECTIVAS. SE ANULA EL COPY  *00170001
001800*                         S01ESCAL.                              *00180001
001900*                         TABLA CORPORATIVA : 0554.              *00190001
002000* APLICACION : FONDO DE SEGURO DE DEPOSITO.                      *00200001
002100*----------------------------------------------------------------*00210001
002200*  WILLIAMS PRADO                                                *00220001
002300*                                                                *00230001
002400*  WPS M001 05/04/1999    SE MODIFICA TABLA DE MONEDAS PARA OB-  *00240001
002500*                         TENER REPORTE PARA LIBRAS ESTERLINAS   *00250001
002600*                         QUE NO ESTABA SALIENDO Y ADICIONAR PA- *00260001
002700*                         RA MONEDAS EURO Y ECU QUE SON NUEVAS.  *00270001
002800*                                                                *00280001
002900*  WPS M002 06/04/1999    SE MODIFICA LA LLAMADA A LA RUTINA DE  *00290001
003000*                         CARGA DE MONEDAS 'EMESACTF' POR RUTI-  *00300001
003100*                         NA PROPIA EN EL PROGRAMA.              *00310001
003200* EVM M001 07/04/99 MODIFICACIONES PROYECTO 2000                 *00320001
003300*----------------------------------------------------------------*00330001
003400*  ISABEL GALLO                                                  *00340001
003500*                                                                *00350001
003600*  IGS M001 30/07/1999    SE MODIFICA CLASIFICACION DE PERSONAS  *00360001
003700*                         JURIDICAS, PRIVADAS SIN FINES DE LUCRO,*00370001
003800*                         (M01 Y M12 ); Y OTRAS (M02, M10, M11,  *00380001
003900*                         M13, M14, M15, M16 )                   *00390001
004000******************************************************************00400001
004100*  IGS01    03/09/2002    SE AGREGA CORASUS 34 Y 35              *00410001
004200*  RVF      05/07/2005    SE AGREGA CORASU  27 (200505177)       *00420001
004300*  RVF      01/04/2006    SE TOTALIZA X CLIENTE (200603285)      *00430001
004400*  RVF      02/08/2006    ADICIONAR CORASU M36  (200608014)      *00440001
      *----------------------------------------------------------------*00464002
      * 6792016011-05 23AGO16   MDP   CAMBIO DENOMINACION Y SIMBOLO DE *00464102
      *                               MONEDA SOLES                     *00465002
      *----------------------------------------------------------------*
      * IFRS9-170913   P024168   2017-09-13   SE AGREGA CORASU CON FIN *
      *                                       DE LUCRO M52, M53.       *
      * PSDAEMF2-446   P021122   2024-04-11   SE AGREGA SEPARADORES A  *
      *                                       REPORTES                 *
      ******************************************************************00466002
004700                                                                  00470001
004800 ENVIRONMENT DIVISION.                                            00480001
004900*--------------------*                                            00490001
005000 INPUT-OUTPUT SECTION.                                            00500001
005100*--------------------*                                            00510001
005200 FILE-CONTROL.                                                    00520001
005300*------------*                                                    00530001
005400     SELECT E1DQSLD0              ASSIGN E1DQSLD0.                00540001
005500                                                                  00550001
005600*@IBC-WPS-M002                                                    00560001
005700* ARCHIVO DE TIPO DE CAMBIO DE MONEDAS VIGENTES                   00570001
005800     SELECT E2DQMON0              ASSIGN UT-S-EMESACTU.           00580001
005900*@FBC-WPS-M002                                                    00590001
006000                                                                  00600001
006100     SELECT S1DQAX80              ASSIGN BGLSANX8.                00610001
006200*@I-NBLE                                                          00620001
006300     SELECT S2DQAX80              ASSIGN S2DQAX80.                00630001
006400*@F-NBLE                                                          00640001
006500                                                                  00650001
006600 DATA DIVISION.                                                   00660001
006700*-------------*                                                   00670001
006800 FILE SECTION.                                                    00680001
006900*------------*                                                    00690001
007000 FD  E1DQSLD0                                                     00700001
007100     LABEL RECORD STANDARD                                        00710001
007200     RECORDING MODE IS F                                          00720001
007300     BLOCK CONTAINS 0 RECORDS.                                    00730001
007400* 200603285-INI                                                   00740001
007500*    COPY BQECPDT.                                                00750001
007600                                                                  00760001
007700 01  REG-E1DQANX0.                                                00770001
007800     05  ANX-CENTRAL        PIC X(08).                            00780001
007900     05  ANX-IND-FILE       PIC X.                                00790001
008000     05  ANX-LIBRE          PIC X.                                00800001
008100     05  ANX-DATA.                                                00810001
008200         10  ANX-CCC        PIC X(18).                            00820001
008300         10  ANX-DAT        PIC X(252).                           00830001
008400* 200603285-FIN                                                   00840001
008500*@IBC-WPS-M002                                                    00850001
008600* DEFINICION DEL ARCHIVO DE TIPO DE CAMBIO DE MONEDAS VIGENTES    00860001
008700                                                                  00870001
008800 FD  E2DQMON0                                                     00880001
008900     BLOCK CONTAINS 0 RECORDS                                     00890001
009000     LABEL RECORD STANDARD                                        00900001
009100     RECORDING MODE IS F                                          00910001
009200     DATA RECORD IS REG-MONEDAS.                                  00920001
009300                                                                  00930001
009400 01  REG-MONEDAS.                                                 00940001
009500     02  RM-EQUIVALENTE           PIC 9(08)V9(7).                 00950001
009600     02  RM-SIMBOLO               PIC X(03).                      00960001
009700     02  RM-MONEDA                PIC X(22).                      00970001
009800     02  RM-NUMERO                PIC 9(03).                      00980001
009900     02  FILLER                   PIC X(37).                      00990001
010000                                                                  01000001
010100*@FBC-WPS-M002                                                    01010001
010200                                                                  01020001
010300 FD  S1DQAX80                                                     01030001
010400     RECORDING MODE F                                             01040001
010500     BLOCK CONTAINS 0 RECORDS                                     01050001
010600     LABEL RECORDS ARE OMITTED.                                   01060001
010700 01  REG-SALIDA.                                                  01070001
010800     05 REG-SAL                   PIC X(164).                     01080001
010900                                                                  01090001
011000*@I-NBLE                                                          01100001
011100 FD  S2DQAX80                                                     01110001
011200     RECORDING MODE F                                             01120001
011300     BLOCK CONTAINS 0 RECORDS                                     01130001
011400     LABEL RECORDS ARE OMITTED.                                   01140001
011500 01  REG-SALIDA1.                                                 01150001
011600     05 SAL1-AAMMDD               PIC X(08).                      01160001
011700     05 SAL1-MODALIDAD            PIC X(02).                      01170001
011800     05 SAL1-MONEDA               PIC X(03).                      01180001
011900     05 SAL1-ESC-DESDE            PIC 9(13)V99.                   01190001
012000     05 SAL1-ESC-HASTA            PIC 9(13)V99.                   01200001
012100     05 SAL1-PN-NUMERO            PIC 9(15).                      01210001
012200     05 SAL1-PN-MONTO             PIC 9(13)V99.                   01220001
012300     05 SAL1-PJ-SFL-NUMERO        PIC 9(15).                      01230001
012400     05 SAL1-PJ-SFL-MONTO         PIC 9(13)V99.                   01240001
012500     05 SAL1-PJ-OTR-NUMERO        PIC 9(15).                      01250001
012600     05 SAL1-PJ-OTR-MONTO         PIC 9(13)V99.                   01260001
012700     05 SAL1-TOTALES-NUMERO       PIC 9(15).                      01270001
012800     05 SAL1-TOTALES-MONTO        PIC 9(13)V99.                   01280001
012900*@F-NBLE                                                          01290001
013000                                                                  01300001
013100 WORKING-STORAGE SECTION.                                         01310001
013200*-----------------------*                                         01320001
013300 01 SW-INICIO                     PIC 9         VALUE ZEROS.      01330001
013400 01 W-REG-HEADER.                                                 01340001
013500    05 FILLER                     PIC X(01)     VALUE SPACES.     01350001
013600    05 FILLER                     PIC X(14)     VALUE             01360001
013700       'HEADERBG3C3170'.                                          01370001
013800    05 WT-FECHA.                                                  01380001
013900       10 WT-DIA                  PIC 99.                         01390001
014000       10 FILLER                  PIC X(01)     VALUE '/'.        01400001
014100       10 WT-MES                  PIC 99.                         01410001
014200       10 FILLER                  PIC X(01)     VALUE '/'.        01420001
014300       10 WT-ANO                  PIC 99.                         01430001
014400 01 W-CEROS                       PIC 9(06)     VALUE 0    COMP-3.01440001
014500 01 W-MENOS                       PIC 9(06)     VALUE 0    COMP-3.01450001
014600 01 W-SOLES                       PIC 9(15)V99  VALUE 0    COMP-3.01460001
014600 01 WA-SOLES                      PIC 9(15)V99  VALUE 0.          01460102
      *--                                                               01460202
      *01 W-SALDO-CLIENTE               PIC 9(15)V99  VALUE 0    COMP-3.01461002
      *--                                                               01462002
014700 01 W-PAG                         PIC 999       VALUE 0          .01470001
014800 01 I                             PIC 999       VALUE 0    COMP-3.01480001
014900 01 J                             PIC 999       VALUE 0    COMP-3.01490001
015000 01 K                             PIC 999       VALUE 0    COMP-3.01500001
015100 01 WS-NRO                        PIC 999       VALUE 0    COMP-3.01510001
015200 01 WS-LIN                        PIC 999       VALUE 0    COMP-3.01520001
015300 01 TABLA-DE-MESES.                                               01530001
015400    05 FILLER                     PIC X(09)     VALUE '  ENERO  '.01540001
015500    05 FILLER                     PIC X(09)     VALUE ' FEBRERO '.01550001
015600    05 FILLER                     PIC X(09)     VALUE '  MARZO  '.01560001
015700    05 FILLER                     PIC X(09)     VALUE '  ABRIL  '.01570001
015800    05 FILLER                     PIC X(09)     VALUE '  MAYO   '.01580001
015900    05 FILLER                     PIC X(09)     VALUE '  JUNIO  '.01590001
016000    05 FILLER                     PIC X(09)     VALUE '  JULIO  '.01600001
016100    05 FILLER                     PIC X(09)     VALUE ' AGOSTO  '.01610001
016200    05 FILLER                     PIC X(09)     VALUE 'SETIEMBRE'.01620001
016300    05 FILLER                     PIC X(09)     VALUE ' OCTUBRE '.01630001
016400    05 FILLER                     PIC X(09)     VALUE 'NOVIEMBRE'.01640001
016500    05 FILLER                     PIC X(09)     VALUE 'DICIEMBRE'.01650001
016600 01 FILLER             REDEFINES  TABLA-DE-MESES.                 01660001
016700    05 MESES           OCCURS 12  PIC X(09).                      01670001
016800                                                                  01680001
016900*COPY S01ESCAL.                                                   01690001
017000                                                                  01700001
017100****   COPY PARA LAS ESCALAS DEL FONDO DE SEGURO DE DEPOSITO  ****01710001
017200 COPY TCTC5540.                                                   01720001
017300                                                                  01730001
017400****      COPY BATCH PARA EL ACCESO A TABLAS CORPORATIVAS     ****01740001
017500 COPY TCWC1000.                                                   01750001
017600                                                                  01760001
017700* 200603285-INI                                                   01770001
017800     COPY BQECPDT.                                                01780001
017900* 200603285-FIN                                                   01790001
018000                                                                  01800001
018100 01  TABLA-IMPORTES.                                              01810001
018200     02 TABLA-P            OCCURS 03 TIMES.                       01820001
018300        03 TABLA-M         OCCURS 200 TIMES.                      01830001
018400           04 ACU-IMPTME          PIC 9(15)V99             COMP-3.01840001
018500           04 TABLA-T      OCCURS 18 TIMES.                       01850002
018600              05 ACU-NUMNAT       PIC 9(07)                COMP-3.01860001
018700              05 ACU-IMPNAT       PIC 9(15)V99             COMP-3.01870001
018800              05 ACU-NUMSFL       PIC 9(07)                COMP-3.01880001
018900              05 ACU-IMPSFL       PIC 9(15)V99             COMP-3.01890001
019000              05 ACU-NUMCFL       PIC 9(07)                COMP-3.01900001
019100              05 ACU-IMPCFL       PIC 9(15)V99             COMP-3.01910001
019200              05 ACU-NUMTOT       PIC 9(07)                COMP-3.01920001
019300              05 ACU-IMPTOT       PIC 9(15)V99             COMP-3.01930001
019400              05 ACU-OFICIN       PIC 9(03).                      01940001
019500 01  TABLA-MONEDAS.                                               01950001
019600     02 TAB-MONEDAS OCCURS 200 TIMES.                             01960001
023600           04 T-CODMON   PIC 99.                                  02360001
023700           04 T-MONEDA   PIC X(12).                               02370001
023800           04 T-SIMBOLO  PIC XXX.                                 02380001
023900           04 T-TIPCAM   PIC 9(8)V9(6).                           02390001
024000                                                                  02400001
024100 01 ARRAY-PRINCIPAL.                                              02410001
024200    05 VALORES-N1      OCCURS 17 TIMES.                           02420002
024300       10 TOPE1                   PIC 9(09)V99.                   02430001
024400       10 TOPE2                   PIC 9(09)V99.                   02440001
024500                                                                  02450001
024600 01 INDICE-ARRAY.                                                 02460001
024700    05 L                          PIC 9(02).                      02470001
024800* 200603285-INI                                                   02480001
024900    05 W-SALDO-CLIENTE            PIC 9(15)V99  COMP-3 VALUE 0.   02490002
024900    05 WA-SALDO-CLIENTE           PIC 9(15)V99  VALUE 0.          02491002
025000    05 WK-ANT-CENTRAL             PIC X(8)      VALUE SPACES.     02500001
025100    05 WK-ANT-DIVISA              PIC X(3)      VALUE SPACES.     02510001
025200    05 WK-ANT-PRODUCTO            PIC X(2)      VALUE SPACES.     02520001
025300    05 WK-ANT-CORASU              PIC X(3)      VALUE SPACES.     02530001
025400* 200603285-FI                                                    02540002
025500                                                                  02550001
025600 01 CONSTANTES.                                                   02560001
025700    05 CTE-PROGRAMA               PIC X(08)  VALUE 'BG3C3170'.    02570001
025800    05 CTE-OPCION                 PIC X(01)  VALUE '1'.           02580001
025900    05 CTE-TABLA-0554             PIC X(04)  VALUE '0554'.        02590001
026000    05 CTE-ENTIDAD                PIC X(04)  VALUE '0011'.        02600001
026100    05 CTE-R                      PIC X(01)  VALUE 'R'.           02610001
026200    05 CTE-1                      PIC 9(01)  VALUE 1.             02620001
026300    05 CTE-TC9C1000               PIC X(08)  VALUE 'TC9C1000'.    02630001
026400    05 CTE-CLAVE.                                                 02640001
026500       10 CTE-SECUENCIA           PIC 9(02).                      02650001
026600                                                                  02660001
026700 01 DATOS-OUTPUT.                                                 02670001
026800    05 WS-LIMITE-I                PIC 9(09)V9(02).                02680001
026900    05 WS-LIMITE-S                PIC 9(09)V9(02).                02690001
027000    05 FILLER                     PIC X(228).                     02700001
027100                                                                  02710001
027200 01 VARIABLES-EDITADAS.                                           02720001
027300    05 ED-SECUENCIA               PIC Z9.                         02730001
027400    05 ED-LIMITE-I                PIC ZZZ,ZZZ,ZZ9.99.             02740001
027500    05 ED-LIMITE-S                PIC ZZZ,ZZZ,ZZ9.99.             02750001
027600                                                                  02760001
027700 01 TIT-1.                                                        02770001
027800    05 FILLER                     PIC X(01)  VALUE SPACES.        02780001
027900    05 TEX-PLAZA                  PIC X(04)  VALUE SPACES.        02790001
028000    05 FILLER                     PIC X(36)  VALUE                02800001
028100       ' SUPERINTENDENCIA DE BANCA Y SEGUROS'.                    02810001
028200                                                                  02820001
028300 01 TIT-2.                                                        02830001
028400    05 FILLER                     PIC X(32)  VALUE ' '.           02840001
028500    05 FILLER                     PIC X(72)  VALUE                02850001
028600       'INSTITUCION: BANCO BBVA PERU         CODIGO: 011'.        02860001
028700    05 FILLER                     PIC X(10)  VALUE 'ANEXO N.13'.  02870001
028800                                                                  02880001
028900 01 TIT-3.                                                        02890001
029000    05 FILLER                     PIC X(40)  VALUE ' '.           02900001
029100    05 FILLER                     PIC X(43)  VALUE                02910001
029200      'DEPOSITOS SEGUN ESCALA DE MONTOS'.                         02920001
029300*   05 I-SISTEMA                  PIC X(27)  VALUE ' '.           02930001
029400*   05 I-MONEDA                   PIC X(03)  VALUE ' '.           02940001
029300 01 I-SISTEMA                  PIC X(27)  VALUE ' '.              02930001
029400 01 I-MONEDA                   PIC X(03)  VALUE ' '.              02940001
029500                                                                  02950001
029600 01 TIT-4.                                                        02960001
029700    05 FILLER                     PIC X(43)  VALUE ' '.           02970001
029800    05 FILLER                     PIC X(03)  VALUE 'AL '.         02980001
029900    05 I-DIA                      PIC 99.                         02990001
030000    05 FILLER                     PIC X(04)  VALUE ' DE '.        03000001
030100    05 I-MES                      PIC X(09).                      03010001
030200* @M001-I                                                         03020001
030300*   05 FILLER                     PIC X(06)  VALUE ' DE 19'.      03030001
030400    05 FILLER                     PIC X(04)  VALUE ' DE '.        03040001
030500    05 I-SIGLO                    PIC X(02).                      03050001
030600* @M001-F                                                         03060001
030700    05 I-ANO                      PIC 99.                         03070001
030800                                                                  03080001
030900 01 TIT-5.                                                        03090001
031000    05 FILLER                     PIC X(48)  VALUE ' BG3C3170'.   03100001
031100    05 FILLER                     PIC X(17)  VALUE                03110001
      *6792016011-05-I                                                  03111002
031200*      '(EN NUEVOS SOLES)'.                                       03120002
031200       '(EN SOLES)       '.                                       03121002
      *6792016011-05-F                                                  03122002
031300 01 TIT-6.                                                        03130001
031400    05 FILLER                     PIC X(31)  VALUE                03140001
031500       '     E S C A L A S'.                                      03150001
031600    05 FILLER                     PIC X(43)  VALUE                03160001
031700       'PERSONAS  NATURALES  --------------PERSONAS'.             03170001
031800    05 FILLER                     PIC X(41)  VALUE                03180001
031900       ' JURIDICAS------------     T O T A L E S'.                03190001
032000                                                                  03200001
032100*@IBC-IGS-M001                                                    03210001
032200*01 TIT-7.                                                        03220001
032300*   05 FILLER                     PIC X(31)  VALUE ' '.           03230001
032400*   05 FILLER                     PIC X(42)  VALUE                03240001
032500*      '                     SIN FINES  DE LUCRO  '.              03250001
032600*   05 FILLER                     PIC X(42)  VALUE                03260001
032700*      ' CON FINES  DE LUCRO '.                                   03270001
032800*                                                                 03280001
032900 01 TIT-7.                                                        03290001
033000    05 FILLER                     PIC X(31)  VALUE ' '.           03300001
033100    05 FILLER                     PIC X(42)  VALUE                03310001
033200       '                     PRIVADAS SIN FINES   '.              03320001
033300    05 FILLER                     PIC X(42)  VALUE                03330001
033400       ' OTRAS               '.                                   03340001
033500                                                                  03350001
033600 01 TIT-7A.                                                       03360001
033700    05 FILLER                     PIC X(31)  VALUE ' '.           03370001
033800    05 FILLER                     PIC X(42)  VALUE                03380001
033900       '                          DE LUCRO        '.              03390001
034000    05 FILLER                     PIC X(42)  VALUE                03400001
034100       '                     '.                                   03410001
034200                                                                  03420001
034300*@FBC-IGS-M001                                                    03430001
034400 01 TIT-8.                                                        03440001
034500    05 FILLER                     PIC X(31)  VALUE ' '.           03450001
034600    05 FILLER                     PIC X(44)  VALUE                03460001
034700       'NRO.     M O N T O     NRO.     M O N T O   '.            03470001
034800    05 FILLER                     PIC X(47)  VALUE                03480001
034900       '  NRO.     M O N T O      NRO.     M O N T O'.            03490001
038300*PSDAEMF2-446-INI                                                 03830001
030300 01 TIT-8A.
030400    05 FILLER                     PIC X(01)  VALUE ';'.
030500    05 FILLER                     PIC X(08)  VALUE
030600       'ESCALAS;'.
030700    05 FILLER                     PIC X(09)  VALUE
030700       'PRODUCTO;'.
030700    05 FILLER                     PIC X(07)  VALUE
030700       'MONEDA;'.
030700    05 FILLER                     PIC X(07)  VALUE
030700       'NRO-PN;'.
030700    05 FILLER                     PIC X(09)  VALUE
030700       'MONTO-PN;'.
030700    05 FILLER                     PIC X(08)  VALUE
030700       'NRO-SFL;'.
030700    05 FILLER                     PIC X(10)  VALUE
030800       'MONTO-SFL;'.
030700    05 FILLER                     PIC X(08)  VALUE
030800       'NRO-CFL;'.
030700    05 FILLER                     PIC X(10)  VALUE
030800       'MONTO-CFL;'.
030700    05 FILLER                     PIC X(08)  VALUE
030800       'NRO-TOT;'.
030700    05 FILLER                     PIC X(12)  VALUE
030800       'MONTO-TOT;'.
038300*PSDAEMF2-446-FIN                                                 03830001
035000                                                                  03500001
035100 01 RAYA.                                                         03510001
035200    05 FILLER                     PIC X(62)  VALUE ALL '*'.       03520001
035300    05 FILLER                     PIC X(62)  VALUE ALL '*'.       03530001
035300    05 FILLER                     PIC X(10)  VALUE ALL '*'.       03530001
035400                                                                  03540001
035500 01 DETA.                                                         03550001
035600    05 X-1                        PIC X(04)  VALUE ';DE '.        03560001
035700    05 I-TOPE1                    PIC Z,ZZZ,ZZ9.99.               03570001
035800    05 X-2                        PIC X(03)  VALUE ' A '.         03580001
035900    05 I-TOPE2                    PIC Z,ZZZ,ZZZ.ZZ.               03590001
036000    05 R-TOPE2          REDEFINES                                 03600001
036100       I-TOPE2                    PIC X(12).                      03610001
036200    05 FILLER                     PIC X(01)  VALUE ';'.           03620001
036300    05 J-PRODUCTO                 PIC X(27).                      03630001
036200    05 FILLER                     PIC X(01)  VALUE ';'.           03620001
036300    05 J-MONEDA                   PIC X(03).                      03630001
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.
036400    05 I-NUMNAT                   PIC ZZZZZZ9.                    03640001
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.
036400    05 I-IMPNAT                   PIC ZZZZ,ZZZ,ZZ9.99.            03640001
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.
036500    05 I-NUMSFL                   PIC ZZZZZZ9.                    03650001
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.
036600    05 I-IMPSFL                   PIC ZZZZ,ZZZ,ZZ9.99.            03660001
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.
036700    05 I-NUMCFL                   PIC ZZZZZZ9.                    03670001
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.
036800    05 I-IMPCFL                   PIC ZZZZ,ZZZ,ZZ9.99.            03680001
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.
036900    05 I-NUMTOT                   PIC ZZZZ,ZZ9.                   03690001
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.
037000    05 I-IMPTOT                   PIC ZZZZ,ZZZ,ZZ9.99.            03700001
   GLB    05 FILLER                     PIC X(01)  VALUE ';'.
037100                                                                  03710001
037200 01 DET-ME.                                                       03720001
037300    05 FILLER                     PIC X(29)  VALUE                03730001
037400       ';TOTAL EN MONEDA DE ORIGEN   '.                           03740001
037500    05 I-IMPTME                   PIC ZZZ,ZZZ,ZZ9.99.             03750001
037600                                                                  03760001
037700 01 DET-TC.                                                       03770001
037800    05 FILLER                     PIC X(29)  VALUE                03780001
037900       ';TIPO DE CAMBIO S.B.S. USADO '.                           03790001
038000    05 I-TIPCAM                   PIC ZZZ,ZZZ.999999.             03800001
038100                                                                  03810001
038200 01 DET-ER.                                                       03820001
038300    05 FILLER                     PIC X(01)  VALUE ' '.           03830001
038400    05 I-QUECTA                   PIC X(23)  VALUE ' '.           03840001
038500    05 I-CENTRO                   PIC X(04).                      03850001
038600    05 FILLER                     PIC X(01)  VALUE '-'.           03860001
038700    05 I-CUENTA                   PIC X(10)BB.                    03870001
038800    05 FILLER                     PIC X(22)  VALUE                03880001
038900       'TIENE CLASE DE CUENTA '.                                  03890001
039000    05 I-CORASU                   PIC X(03).                      03900001
039100                                                                  03910001
039200*@IBC-WPS-M002                                                    03920001
039300* SE RETIRA EL LLAMADO AL COPY DE TABLA DE MONEDAS                03930001
039400*   COPY E01COMON.                                                03940001
039500                                                                  03950001
039600 01  TABLA-DE-MONEDAS.                                            03960001
039700     03  VALORES OCCURS 50.                                       03970001
039800         04  EQUIVALENTE              PIC 9(8)V9(7).              03980001
039900         04  SIMBOLO                  PIC X(03).                  03990001
040000         04  MONEDA                   PIC X(22).                  04000001
040100         04  NUMERO                   PIC 9(03).                  04010001
040200                                                                  04020001
040300 01  SW-FIN-ARCHIVO                   PIC X(01) VALUE 'N'.        04030001
040400     88  FIN-ARCHIVO-E2DQMON0                   VALUE 'S'.        04040001
040500                                                                  04050001
040600*200603285-INI                                                    04060001
040700 01  SW-ACUMULA                       PIC X(01) VALUE 'N'.        04070001
040800     88  SW-SI-ACUMULA                          VALUE 'S'.        04080001
040900     88  SW-NO-ACUMULA                          VALUE 'S'.        04090001
041000*200603285-FIN                                                    04100001
041100                                                                  04110001
041200*@FBC-WPS-M002                                                    04120001
041300*PLBD-01-I                                                        04130001
041400 01 W-CORASU  PIC X(03).                                          04140001
041500    88 NAT  VALUE  'F00', 'F01', 'F02'.                           04150002
041800    88 SFL  VALUE  'M01', 'M12', 'M32', 'M35'.                    04180102
042000    88 CFL  VALUE  'M02', 'M10', 'M13', 'M14','M15','M16','M17',  04200002
042100                   'M20', 'M26', 'M27', 'M29','M30','M31','M34',  04210002
042700                   'M37', 'M38', 'M39', 'M40','M41','M42','M44',  04270002
      *IFRS9-170913-INI
                         'M53'.
      *IFRS9-170913-FIN
          88 CFL-2 VALUE 'M03', 'M04', 'M05', 'M06', 'M07', 'M08',      04280002
                         'M18', 'M19', 'M21', 'M22', 'M23', 'M24',      04290002
                         'M25', 'M28', 'M33', 'M36', 'M43', 'M45',      04300002
                         'M46', 'M47', 'M48',                           04310002
      *IFRS9-170913-INI
                         'M52'.
      *IFRS9-170913-FIN
043400                                                                  04340001
043500 01 FECHA-CR.                                                     04350001
043600    05 CRDIA                      PIC 99.                         04360001
043700    05 FILLER                     PIC X(01).                      04370001
043800    05 CRMES                      PIC 99.                         04380001
043900    05 FILLER                     PIC X(01).                      04390001
044000    05 CRANO                      PIC 99.                         04400001
044100                                                                  04410001
044200 01 FECHAX.                                                       04420001
044300    05 ANOX                       PIC 99     VALUE ZEROS.         04430001
044400    05 MESX                       PIC 99     VALUE ZEROS.         04440001
044500    05 DIAX                       PIC 99     VALUE ZEROS.         04450001
044600                                                                  04460001
044700 01 CRDATE.                                                       04470001
044800    05 CRD-DIA                    PIC 99     VALUE ZEROS.         04480001
044900    05 FILLER                     PIC X(01)  VALUE '/'.           04490001
045000    05 CRD-MES                    PIC 99     VALUE ZEROS.         04500001
045100    05 FILLER                     PIC X(01)  VALUE '/'.           04510001
045200    05 CRD-ANO                    PIC 99     VALUE ZEROS.         04520001
045300                                                                  04530001
045400* @M001-I                                                         04540001
045500 01  CTE-CONSTANTES.                                              04550001
045600     05  CTE-19        PIC X(02)  VALUE '19'.                     04560001
045700     05  CTE-20        PIC X(02)  VALUE '20'.                     04570001
045800* @M001-F                                                         04580001
045900                                                                  04590001
046000 LINKAGE SECTION.                                                 04600001
046100*---------------*                                                 04610001
046200 01 LK-PARAMETROS.                                                04620001
046300    05 LK-LONGIT                  PIC S9(04)               COMP.  04630001
046400    05 LK-FECHA.                                                  04640001
046500       10 LK-FECANO               PIC 9(02).                      04650001
046600       10 LK-FECMES               PIC 9(02).                      04660001
046700       10 LK-FECDIA               PIC 9(02).                      04670001
046800*   05 LK-UPSIS.                                                  04680002
046900*      10 UPSI0FLD                PICTURE X.                      04690002
047000*         88 UPSI0                           VALUE '1'.           04700002
047100*      10 UPSI1FLD                PICTURE X.                      04710002
047200*         88 UPSI1                           VALUE '1'.           04720002
047300*      10 UPSI2FLD                PICTURE X.                      04730002
047400*         88 UPSI2                           VALUE '1'.           04740002
047500*      10 UPSI3FLD                PICTURE X.                      04750002
047600*         88 UPSI3                           VALUE '1'.           04760002
047700*      10 UPSI4FLD                PICTURE X.                      04770002
047800*         88 UPSI4                           VALUE '1'.           04780002
047900*      10 UPSI5FLD                PICTURE X.                      04790002
048000*         88 UPSI5                           VALUE '1'.           04800002
048100*      10 UPSI6FLD                PICTURE X.                      04810002
048200*         88 UPSI6                           VALUE '1'.           04820002
048300*      10 UPSI7FLD                PICTURE X.                      04830002
048400*         88 UPSI7                           VALUE '1'.           04840002
048500                                                                  04850001
048600 PROCEDURE DIVISION USING LK-PARAMETROS.                          04860001
048700*--------------------------------------*                          04870001
048800 CHEQUEO-PARAMETROS.                                              04880001
048900*------------------*                                              04890001
049000     IF LK-LONGIT = ZEROS OR                                      04900001
049100* @M001-I                                                         04910001
049200*       LK-FECANO < 92 OR LK-FECANO = 00 OR                       04920001
049300        LK-FECHA NOT NUMERIC            OR                        04930001
049400* @M001-F                                                         04940001
049500        LK-FECMES < 01 OR LK-FECMES > 12 OR                       04950001
049600        LK-FECDIA < 01 OR LK-FECDIA > 31                          04960001
049700        ACCEPT FECHAX                  FROM DATE                  04970001
049800        MOVE ANOX                      TO LK-FECANO               04980001
049900        MOVE MESX                      TO LK-FECMES               04990001
050000        MOVE DIAX                      TO LK-FECDIA               05000001
050100     END-IF.                                                      05010001
050200                                                                  05020001
050300 001-C.                                                           05030001
050400*-----*                                                           05040001
050500     MOVE LK-FECANO                    TO CRD-ANO.                05050001
050600     MOVE LK-FECMES                    TO CRD-MES.                05060001
050700     MOVE LK-FECDIA                    TO CRD-DIA.                05070001
050800                                                                  05080001
050900 INICIO.                                                          05090001
051000*------*                                                          05100001
051100     INITIALIZE ARRAY-PRINCIPAL                                   05110001
051200                INDICE-ARRAY.                                     05120001
051300                                                                  05130001
051400     PERFORM CARGA-ARRAY VARYING L FROM 1 BY 1 UNTIL L > 17.      05140002
051500                                                                  05150001
051600*@IBC-WPS-M002                                                    05160001
051700* SE RETIRA LLAMADA A LA RUTINA 'EMESACTF' POR RUTINA INTERNA     05170001
051800*    CALL 'EMESACTF'                   USING TABLA-DE-MONEDAS.    05180001
051900                                                                  05190001
052000     PERFORM  CARGAR-TABLA-DE-MONEDAS.                            05200001
052100                                                                  05210001
052200*@FBC-WPS-M002                                                    05220001
052300                                                                  05230001
052400     MOVE CRDATE                       TO FECHA-CR                05240001
052500     MOVE CRDIA                        TO I-DIA                   05250001
052600     MOVE MESES (CRMES)                TO I-MES                   05260001
052700     MOVE CRANO                        TO I-ANO                   05270001
052800* @M001-I                                                         05280001
052900     IF CRANO LESS 80                                             05290001
053000        MOVE CTE-20     TO I-SIGLO                                05300001
053100     ELSE                                                         05310001
053200        MOVE CTE-19     TO I-SIGLO                                05320001
053300     END-IF                                                       05330001
053400* @M001-F                                                         05340001
053500*    PERFORM BORRAR-MONEDAS VARYING I FROM 01 BY 1 UNTIL I > 19.  05350001
053600     INITIALIZE TABLA-IMPORTES.                                   05360001
053700     GO TO ABRIR.                                                 05370001
053800                                                                  05380001
053900*@IBC-WPS-M002                                                    05390001
054000* RUTINA QUE CARGA TIPO DE CAMBIO DE MONEDAS VIGENTES             05400001
054100                                                                  05410001
054200*************************                                         05420001
054300 CARGAR-TABLA-DE-MONEDAS.                                         05430001
054400*************************                                         05440001
054500                                                                  05450001
054600     INITIALIZE  TABLA-MONEDAS                                    05460002
054700     MOVE 'N'                         TO SW-FIN-ARCHIVO           05470001
054800                                                                  05480001
054900     OPEN INPUT  E2DQMON0                                         05490001
055000                                                                  05500001
055100     READ E2DQMON0  AT END                                        05510001
055200          SET  FIN-ARCHIVO-E2DQMON0   TO TRUE                     05520001
055300     END-READ                                                     05530001
055400                                                                  05540001
055500     PERFORM  VARYING I FROM 1 BY 1 UNTIL  I > 200                05550001
055600                                       OR  FIN-ARCHIVO-E2DQMON0   05560001
055700                                                                  05570001
055800              MOVE RM-EQUIVALENTE     TO T-TIPCAM  (RM-NUMERO)    05580002
055900              MOVE RM-SIMBOLO         TO T-SIMBOLO (RM-NUMERO)    05590002
056000              MOVE RM-MONEDA          TO T-MONEDA  (RM-NUMERO)    05600002
056100*             DISPLAY RM-NUMERO ' ' RM-SIMBOLO ' ' RM-MONEDA ' '  05610002
056200*             'TIP CAMBIO ' RM-EQUIVALENTE                        05620002
056300              READ E2DQMON0  AT END                               05630001
056400                   SET  FIN-ARCHIVO-E2DQMON0   TO TRUE            05640001
056500              END-READ                                            05650001
056600                                                                  05660001
056700     END-PERFORM                                                  05670001
056800                                                                  05680001
056900     CLOSE  E2DQMON0.                                             05690001
057000                                                                  05700001
057100*@FBC-WPS-M002                                                    05710001
057200                                                                  05720001
057300 CARGA-ARRAY.                                                     05730001
057400*-----------*                                                     05740001
057500     INITIALIZE TCWC1000.                                         05750001
057600                                                                  05760001
057700     MOVE L                            TO CTE-SECUENCIA.          05770001
057800     MOVE CTE-OPCION                   TO W100-CDOPCIO.           05780001
057900     MOVE CTE-TABLA-0554               TO W100-CDTABLA.           05790001
058000     MOVE CTE-ENTIDAD                  TO W100-STBANCO.           05800001
058100     MOVE CTE-R                        TO W100-TCCIDIOM.          05810001
058200     MOVE CTE-CLAVE                    TO W100-CLAVTG.            05820001
058300     MOVE CTE-1                        TO W100-NUCLAVE.           05830001
058400                                                                  05840001
058500     CALL 'TC9C1000'                   USING TCWC1000.            05850001
058600                                                                  05860001
058700     EVALUATE W100-CDRETORN                                       05870001
058800     WHEN '00'                                                    05880001
058900          MOVE W100-CONTOCUR(1)        TO DATOS-OUTPUT            05890001
059000          MOVE CTE-SECUENCIA           TO ED-SECUENCIA            05900001
059100          MOVE WS-LIMITE-I             TO TOPE1(L)                05910001
059200                                          ED-LIMITE-I             05920001
059300          MOVE WS-LIMITE-S             TO TOPE2(L)                05930001
059400                                          ED-LIMITE-S             05940001
059500                                                                  05950001
059600*         DISPLAY ' CODIGO DE SECUENCIA : ' ED-SECUENCIA          05960001
059700*         DISPLAY ' LIMITE INFERIOR     : ' ED-LIMITE-I           05970001
059800*         DISPLAY ' LIMITE SUPERIOR     : ' ED-LIMITE-S           05980001
059900     WHEN OTHER                                                   05990001
060000          DISPLAY ' DATO NO EXISTE EN TABLAS CORPORATIVAS'        06000001
060100          DISPLAY ' CLAVE ENVIADA       : ' CTE-CLAVE             06010001
060200          DISPLAY ' LIMITE INFERIOR     : ' ED-LIMITE-I           06020001
060300          DISPLAY ' LIMITE SUPERIOR     : ' ED-LIMITE-S           06030001
060400     END-EVALUATE.                                                06040001
060500                                                                  06050001
061300 ABRIR.                                                           06130001
061400*-----*                                                           06140001
061500     OPEN INPUT E1DQSLD0                                          06150001
061600         OUTPUT S1DQAX80                                          06160001
061700                S2DQAX80                                          06170001
           PERFORM TITULOS.
061800                                                                  06180001
061900 LEE-MAESTRO.                                                     06190001
062000*-----------*                                                     06200001
062100     READ E1DQSLD0  AT END GO TO IMPRIME-TOTALES.                 06210001
062200                                                                  06220001
062300* 200603285-INI                                                   06230001
062400     MOVE ANX-DATA           TO BQECPDT.                          06240001
062500* 200603285-FIN                                                   06250001
062600                                                                  06260001
062700 ACUMULA.                                                         06270001
062800*-------*                                                         06280001
062900*PLBD-01-I                                                        06290001
063000     MOVE PDT-CORASU TO W-CORASU                                  06300001
063100*    IF PDT-PRODUCTO = 09 OR                                      06310001
063200******  SLD-SALDO-DISPUE < 0 OR                                   06320001
063300*       PDT-CORASU = ('M03' OR 'M04' OR 'M05' OR 'M06' OR         06330001
063400*                     'M07' OR 'M08' OR 'M09')                    06340001
063500******  SLD-INDSITU-SU = 'B' OR 'G' OR                            06350001
063600******  SLD-INDESTA = 'C'                                         06360001
063700*       GO TO LEE-MAESTRO                                         06370001
063800     IF (NAT OR CFL OR SFL OR CFL-2) AND                          06380002
              PDT-PRODUCTO NOT EQUAL 09                                 06381002
063900        CONTINUE                                                  06390001
064000     ELSE                                                         06400001
064100        GO TO LEE-MAESTRO                                         06410001
064200     END-IF.                                                      06420001
064300*PLBD-01-F                                                        06430001
064400* 200603285-INI                                                   06440001
064500     IF ANX-CENTRAL    NOT = WK-ANT-CENTRAL OR                    06450001
064600        ANX-CENTRAL        = '00000000'     OR                    06460001
064700        ANX-CENTRAL        = SPACES         OR                    06470001
064800        PDT-DIVISA     NOT = WK-ANT-DIVISA                        06480001
064900        IF SW-SI-ACUMULA                                          06490001
065000           PERFORM QUIEBRE-CLIENTE                                06500001
065100        END-IF                                                    06510001
065200        MOVE ZEROS           TO W-SALDO-CLIENTE                   06520001
065200        MOVE ZEROS           TO WA-SALDO-CLIENTE                  06521002
065300        SET SW-NO-ACUMULA    TO TRUE                              06530001
065400        MOVE ANX-CENTRAL     TO WK-ANT-CENTRAL                    06540001
065500        MOVE PDT-DIVISA      TO WK-ANT-DIVISA                     06550001
065600        MOVE PDT-CORASU      TO WK-ANT-CORASU                     06560001
065700     END-IF.                                                      06570001
065800* 200603285-FIN                                                   06580001
065900                                                                  06590001
066000     PERFORM ACUMULA-SALDOS.                                      06600001
066200     GO TO LEE-MAESTRO.                                           06620001
066300                                                                  06630001
066400******************************************************************06640001
066500**  A C U M U L A    CONTIAHORRO / PLAZOS / SUPER DEPOSITO      **06650001
066600******************************************************************06660001
066700 ACUMULA-SALDOS.                                                  06670001
066800*--------------*                                                  06680001
           MOVE 1 TO I                                                  06690002
           PERFORM UNTIL PDT-DIVISA = T-SIMBOLO (I) OR I > 200          06700002
              ADD 1 TO I                                                06710002
           END-PERFORM                                                  06720002
           IF I > 200                                                   06730002
              DISPLAY 'ERROR OCCURS, MAS D 200 DIVISAS ' PDT-DIVISA     06740002
              STOP RUN                                                  06750002
           END-IF.                                                      06760002
              MOVE 1 TO K.                                              06780002
069800*-------------------------------------------------------------    06980001
069900     IF I = 1                                                     06990001
070000        MOVE PDT-IMPORTE-PAG  TO W-SOLES                          07000001
070100     ELSE                                                         07010001
070200        COMPUTE W-SOLES ROUNDED =                                 07020001
070300                PDT-IMPORTE-PAG  * T-TIPCAM (I).                  07030002
070400     ADD  PDT-IMPORTE-PAG    TO ACU-IMPTME (K, I).                07040002
                                                                        07051002
070600* 200603285-INI                                                   07060001
070700     ADD W-SOLES    TO  W-SALDO-CLIENTE.                          07070001
070800     SET SW-SI-ACUMULA  TO TRUE.                                  07080001
070900                                                                  07090001
071000 QUIEBRE-CLIENTE.                                                 07100001
071100*---------------*                                                 07110001
071200     MOVE W-SALDO-CLIENTE   TO W-SOLES                            07120001
071300     MOVE WK-ANT-CORASU     TO W-CORASU                           07130001
071400                                                                  07140001
071500* 200603285-FIN                                                   07150001
071600                                                                  07160001
071700*-------------------------------------------------------------    07170001
071800*    IF PDT-CORASU = 'F00'                                        07180001
071900*       PERFORM LLENO-NAT VARYING J FROM 1 BY 1 UNTIL J > 9       07190001
072000*    ELSE                                                         07200001
072100*       IF PDT-CORASU = 'M01' OR 'M12' OR 'M13' OR 'M14' OR       07210001
072200*                       'M15' OR 'M16'                            07220001
072300*          PERFORM LLENO-SFL VARYING J FROM 1 BY 1 UNTIL J > 9    07230001
072400*       ELSE                                                      07240001
072500*          IF PDT-CORASU = 'M02' OR 'M10' OR 'M11'                07250001
072600*             PERFORM LLENO-CFL VARYING J FROM 1 BY 1 UNTIL J > 9.07260001
072700*@IBC-IGS-M001                                                    07270001
072800*    IF PDT-CORASU = 'F00' OR 'F01' OR 'F02'                      07280001
072900*       PERFORM LLENO-NAT VARYING J FROM 1 BY 1 UNTIL J > 9       07290001
073000*    ELSE                                                         07300001
073100*       IF PDT-CORASU = 'M01' OR 'M12' OR 'M18' OR 'M19' OR 'M21' 07310001
073200*          PERFORM LLENO-SFL VARYING J FROM 1 BY 1 UNTIL J > 9    07320001
073300*       ELSE                                                      07330001
073400*          IF PDT-CORASU = 'M02' OR 'M10' OR 'M11' OR 'M13' OR    07340001
073500*                          'M14' OR 'M15' OR 'M16' OR 'M17' OR    07350001
073600*                          'M20'                                  07360001
073700*             PERFORM LLENO-CFL VARYING J FROM 1 BY 1 UNTIL J > 9.07370001
073800*@FBC-IGS-M001                                                    07380001
073900*PLBD-01-I                                                        07390001
074000*    IF PDT-CORASU = 'F00' OR 'F01' OR 'F02'                      07400001
074100     IF NAT                                                       07410001
074200*PLBD-01-F                                                        07420001
074300        PERFORM LLENO-NAT VARYING J FROM 1 BY 1 UNTIL J > 17      07430002
074400     ELSE                                                         07440001
074500*PLBD-01-I                                                        07450001
074600*       IF PDT-CORASU = 'M01' OR 'M12' OR 'M13' OR 'M14' OR 'M15' 07460001
074700*                    OR 'M16'                                     07470001
074800        IF SFL                                                    07480001
074900*PLBD-01-F                                                        07490001
075000           PERFORM LLENO-SFL VARYING J FROM 1 BY 1 UNTIL J > 17   07500002
075100        ELSE                                                      07510001
075200*PLBD-01-I                                                        07520001
075300*          IF PDT-CORASU = 'M02' OR 'M10' OR 'M11' OR 'M17' OR    07530001
075400*                          'M18' OR 'M19' OR 'M20' OR 'M21'       07540001
075500           IF CFL OR CFL-2                                        07550002
075600*PLBD-01-F                                                        07560001
075700            PERFORM LLENO-CFL VARYING J FROM 1 BY 1 UNTIL J > 17. 07570002
075800                                                                  07580001
075900 LLENO-NAT.                                                       07590001
076000*---------*                                                       07600001
076100     IF W-SOLES NOT > TOPE2 (J)                                   07610001
076200        ADD  1        TO ACU-NUMNAT (K I J)  ACU-NUMTOT (K I J)   07620001
076300                         ACU-NUMNAT (K I 18) ACU-NUMTOT (K I 18)  07630002
076400        ADD  W-SOLES  TO ACU-IMPNAT (K I J)  ACU-IMPTOT (K I J)   07640001
076500                         ACU-IMPNAT (K I 18) ACU-IMPTOT (K I 18)  07650002
076600        MOVE 20       TO J.                                       07660001
076700                                                                  07670001
076800 LLENO-SFL.                                                       07680001
076900*---------*                                                       07690001
077000     IF W-SOLES NOT > TOPE2 (J)                                   07700001
077100        ADD  1        TO ACU-NUMSFL (K I J)  ACU-NUMTOT (K I J)   07710001
077200                         ACU-NUMSFL (K I 18) ACU-NUMTOT (K I 18)  07720002
077300        ADD  W-SOLES  TO ACU-IMPSFL (K I J)  ACU-IMPTOT (K I J)   07730001
077400                         ACU-IMPSFL (K I 18) ACU-IMPTOT (K I 18)  07740002
077500        MOVE 20       TO J.                                       07750001
077600                                                                  07760001
077700 LLENO-CFL.                                                       07770001
077800*---------*                                                       07780001
077900     IF W-SOLES NOT > TOPE2 (J)                                   07790001
078000        ADD  1        TO ACU-NUMCFL (K I J)  ACU-NUMTOT (K I J)   07800001
078100                         ACU-NUMCFL (K I 18) ACU-NUMTOT (K I 18)  07810002
078200        ADD  W-SOLES  TO ACU-IMPCFL (K I J)  ACU-IMPTOT (K I J)   07820001
078300                         ACU-IMPCFL (K I 18) ACU-IMPTOT (K I 18)  07830002
078400        MOVE 20       TO J.                                       07840001
078500                                                                  07850001
078600*LISTA-ERRADO.                                                    07860001
078700*------------*                                                    07870001
078800*    MOVE PDT-CENTRO      TO I-CENTRO                             07880001
078900*    MOVE PDT-CUENTA      TO I-CUENTA                             07890001
079000*    MOVE PDT-CORASU      TO I-CORASU                             07900001
079100*    MOVE DET-ER   TO REG-SAL                                     07910001
079200*    MOVE 1        TO WS-NRO.                                     07920001
079300*    PERFORM LLAMAR-IMP.                                          07930001
079400                                                                  07940001
079500 TITULOS.                                                         07950001
079600*-------*                                                         07960001
079700*    MOVE ZEROS                        TO WS-LIN.                 07970001
079800*    WRITE REG-SALIDA  FROM TIT-1      AFTER PAGE.                07980001
079900*    WRITE REG-SALIDA  FROM TIT-2      AFTER 1. MOVE 1 TO WS-NRO. 07990001
080000*    WRITE REG-SALIDA  FROM TIT-3      AFTER 1. MOVE 1 TO WS-NRO. 08000001
080100*    WRITE REG-SALIDA  FROM TIT-4      AFTER 1. MOVE 1 TO WS-NRO. 08010001
080200*    WRITE REG-SALIDA  FROM TIT-5      AFTER 1. MOVE 1 TO WS-NRO. 08020001
080300*    WRITE REG-SALIDA  FROM RAYA       AFTER 2. MOVE 2 TO WS-NRO. 08030001
080400*    WRITE REG-SALIDA  FROM TIT-6      AFTER 1. MOVE 1 TO WS-NRO. 08040001
080500*    WRITE REG-SALIDA  FROM TIT-7      AFTER 1. MOVE 1 TO WS-NRO. 08050001
080600*    WRITE REG-SALIDA  FROM TIT-7A     AFTER 1. MOVE 1 TO WS-NRO. 08060001
080700*    WRITE REG-SALIDA  FROM TIT-8      AFTER 1. MOVE 1 TO WS-NRO. 08070001
080700*    WRITE REG-SALIDA  FROM TIT-8A     AFTER 1. MOVE 1 TO WS-NRO. 08070001
080800*    WRITE REG-SALIDA  FROM RAYA       AFTER 1. MOVE 1 TO WS-NRO. 08080001
080900                                                                  08090001
079800     WRITE REG-SALIDA  FROM TIT-1                                 07980001
079900     WRITE REG-SALIDA  FROM TIT-2                                 07990001
080000     WRITE REG-SALIDA  FROM TIT-3                                 08000001
080100     WRITE REG-SALIDA  FROM TIT-4                                 08010001
080200     WRITE REG-SALIDA  FROM TIT-5                                 08020001
080700     WRITE REG-SALIDA  FROM TIT-8A.                               08070001
080900                                                                  08090001
081000 IMPRIME-TOTALES.                                                 08100001
081100*---------------*                                                 08110001
081200* 200603285-INI                                                   08120001
081300     IF SW-SI-ACUMULA                                             08130001
081400        PERFORM QUIEBRE-CLIENTE                                   08140001
081500     END-IF                                                       08150001
081600* 200603285-FIN                                                   08160001
081700                                                                  08170001
081800     PERFORM VARYING K FROM 1 BY 1 UNTIL K > 3                    08180001
081900      PERFORM IMPRIME-MONEDAS VARYING I FROM 1 BY 1 UNTIL I > 200 08190002
082000     END-PERFORM.                                                 08200001
082100                                                                  08210001
082200 CERRAR-ARCHIVOS.                                                 08220001
082300*---------------*                                                 08230001
082400     CLOSE E1DQSLD0                                               08240001
082500           S1DQAX80                                               08250001
082600           S2DQAX80.                                              08260001
082700     STOP RUN.                                                    08270001
082800                                                                  08280001
082900 IMPRIME-MONEDAS.                                                 08290001
083000*---------------*                                                 08300001
083100     MOVE T-SIMBOLO (I)  TO I-MONEDA.                             08310001
083200     IF K = 1                                                     08320001
083300        MOVE '      CHEQUES CERTIFICADOS' TO I-SISTEMA.           08330001
083400     IF ACU-IMPTME (K I) > 0                                      08340001
083500*       PERFORM TITULOS                                           08350001
083600        PERFORM IMPRIME-TOTAL VARYING J FROM 1 BY 1 UNTIL J > 18  08360002
083700        IF I = 01                                                 08370001
083800           NEXT SENTENCE                                          08380001
083900        ELSE                                                      08390001
084000           MOVE ACU-IMPTME (K I) TO I-IMPTME                      08400001
084100           MOVE T-TIPCAM   (I)   TO I-TIPCAM                      08410001
084200           MOVE DET-ME           TO REG-SAL                       08420001
084300*          MOVE 2                TO WS-NRO   PERFORM LLAMAR-IMP   08430001
084300           PERFORM LLAMAR-IMP                                     08430001
084400           MOVE DET-TC           TO REG-SAL                       08440001
084500*          MOVE 2                TO WS-NRO   PERFORM LLAMAR-IMP.  08450001
084500           PERFORM LLAMAR-IMP.                                    08450001
084600                                                                  08460001
084700*                                                                 08470001
084800 GRABA-TOTAL.                                                     08480001
084900*-----------*                                                     08490001
085000     WRITE REG-SALIDA1.                                           08500001
085100*                                                                 08510001
085200 IMPRIME-TOTAL.                                                   08520001
085300*-------------*                                                   08530001
085400     INITIALIZE  REG-SALIDA1.                                     08540001
085500     IF J = 18                                                    08550002
              MOVE ';  '         TO X-1
085600        MOVE 'TOTALES'     TO R-TOPE2                             08560001
085700        MOVE '  '          TO SAL1-MODALIDAD                      08570001
085800     ELSE                                                         08580001
085900        MOVE ';DE'         TO X-1                                 08590001
086000        MOVE ' A'          TO X-2                                 08600001
086100        MOVE I-SIGLO       TO SAL1-AAMMDD(1:2)                    08610001
086200        MOVE I-ANO         TO SAL1-AAMMDD(3:2)                    08620001
086300        MOVE CRMES         TO SAL1-AAMMDD(5:2)                    08630001
086400        MOVE I-DIA         TO SAL1-AAMMDD(7:2)                    08640001
086500        IF K EQUAL 1                                              08650001
086600           MOVE '18'         TO SAL1-MODALIDAD                    08660001
086700        END-IF                                                    08670001

      *6792016011-05-I                                                  08681002
086800*       EVALUATE I-MONEDA                                         08680001
086900*            WHEN 'S/.'                                           08690002
087000*               MOVE 'PEN'    TO SAL1-MONEDA                      08700001
087100*            WHEN 'US$'                                           08710001
087200*               MOVE 'USD'    TO SAL1-MONEDA                      08720001
087300*            WHEN OTHER                                           08730001
087400*               MOVE I-MONEDA TO SAL1-MONEDA                      08740001
087500*       END-EVALUATE                                              08750001

              IF I-MONEDA       = 'S/.' OR
                 I-MONEDA (1:2) = 'S/'
                 MOVE 'PEN'          TO SAL1-MONEDA                     09690000
              ELSE
                 IF I-MONEDA = 'US$'
                    MOVE 'USD'       TO SAL1-MONEDA                     09690000
                 ELSE
                    MOVE I-MONEDA    TO SAL1-MONEDA
                 END-IF
              END-IF
      *6792016011-05-F                                                  08692002
              MOVE SAL1-MONEDA  TO J-MONEDA
              MOVE I-SISTEMA TO J-PRODUCTO
087600        MOVE TOPE1     (J) TO I-TOPE1                             08760001
087700                           SAL1-ESC-DESDE                         08770001
087800        IF J = 15                                                 08780002
087900           MOVE 'MAS '     TO R-TOPE2                             08790001
088000           MOVE 9999999999999.99   TO SAL1-ESC-HASTA              08800001
088100        ELSE                                                      08810001
088200           MOVE TOPE2  (J) TO I-TOPE2                             08820001
088300                              SAL1-ESC-HASTA.                     08830001
088400     MOVE ACU-NUMNAT (K I J) TO I-NUMNAT                          08840001
088500                                SAL1-PN-NUMERO.                   08850001
088600     MOVE ACU-IMPNAT (K I J) TO I-IMPNAT                          08860001
088700                                SAL1-PN-MONTO.                    08870001
088800     MOVE ACU-NUMSFL (K I J) TO I-NUMSFL                          08880001
088900                                SAL1-PJ-SFL-NUMERO.               08890001
089000     MOVE ACU-IMPSFL (K I J) TO I-IMPSFL                          08900001
089100                                SAL1-PJ-SFL-MONTO.                08910001
089200     MOVE ACU-NUMCFL (K I J) TO I-NUMCFL                          08920001
089300                                SAL1-PJ-OTR-NUMERO.               08930001
089400     MOVE ACU-IMPCFL (K I J) TO I-IMPCFL                          08940001
089500                                SAL1-PJ-OTR-MONTO.                08950001
089600     MOVE ACU-NUMTOT (K I J) TO I-NUMTOT                          08960001
089700                                SAL1-TOTALES-NUMERO.              08970001
089800     MOVE ACU-IMPTOT (K I J) TO I-IMPTOT                          08980001
089900                                SAL1-TOTALES-MONTO.               08990001
090000     IF J NOT EQUAL 18                                            09000002
090100        PERFORM GRABA-TOTAL                                       09010001
090200     END-IF.                                                      09020001
090300*PSDAEMF2-446-INI                                                 09030001
090400*    IF J = 18                                                    09040002
090500*       MOVE RAYA          TO REG-SAL                             09050001
090600*       MOVE 2             TO WS-NRO   PERFORM LLAMAR-IMP.        09060001
090700*    MOVE DETA             TO REG-SAL                             09070001
090800*    MOVE 2                TO WS-NRO   PERFORM LLAMAR-IMP         09080001
090900*    IF J = 18                                                    09090002
091000*       MOVE RAYA          TO REG-SAL                             09100001
091100*       MOVE 2             TO WS-NRO   PERFORM LLAMAR-IMP.        09110001
090700     MOVE DETA             TO REG-SAL                             09070001
090800     PERFORM LLAMAR-IMP                                           09080001
           INITIALIZE DETA.
      *PSDAEMF2-446-FIN
091300                                                                  09130001
091400 LLAMAR-IMP.                                                      09140001
091500*----------*                                                      09150001
091600*    ADD  WS-NRO                       TO WS-LIN.                 09160001
091700*    WRITE REG-SALIDA                  AFTER WS-NRO.              09170001
091700*    WRITE REG-SALIDA                  AFTER WS-NRO.              09170001
091800*    IF WS-LIN > 60                                               09180001
091900*       PERFORM TITULOS                                           09190001
092000*    END-IF.                                                      09200001
           WRITE REG-SALIDA.
092100                                                                  09210001
092200******************************************************************09220001
092300*                    FINAL DEL PROGRAMA                  J.C.M.A.*09230001
092400******************************************************************09240001
