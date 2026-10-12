//*  %%INCLIB MBVP.PROD.SYMBOLS %%INCMEM GLOBALES                       00000100
//BLJP3200 JOB (EEBG,1),'BG',                                           00000200
//             CLASS=C,                                                 00000300
//             MSGCLASS=X,                                              00000400
//             REGION=0M                                                00000500
//JOBLIB   DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.BATCH                  00000600
//         DD DISP=SHR,DSN=PEBP.ALTAMIRA.LOADLIB.RUTINAS                00000700
//         DD DISP=SHR,DSN=LDB2DPPA.SDSNEXIT                            00000800
//         DD DISP=SHR,DSN=LDB2DPPA.SDSNLOAD                            00000900
//*--------------------------------------------------------------------*00001000
//SETVAR   SET AMB=P                                                    00001100
//FECHA    SET FECHA=%%ODATE                                            00001200
//PR01     SET PR01=01                                                  00001300
//PR02     SET PR02=02                                                  00001400
//PR03     SET PR03=03                                                  00001500
//PR08     SET PR08=08                                                  00001600
//PR14     SET PR14=14                                                  00001710
//PR18     SET PR18=18                                                  00001800
//PR07     SET PR07=07                                                  00001900
//*--------------------------------------------------------------------*00002000
//* ELIMINACION DE LOS ARCHIVOS DE SALIDA.                             *00002100
//*--------------------------------------------------------------------*00002200
//BL320099 EXEC PGM=IDCAMS                                              00002300
//SYSPRINT DD SYSOUT=*                                                  00002400
//SYSUDUMP DD SYSOUT=*                                                  00002500
//SYSIN    DD *                                                         00002600
  DELETE PEBP.BGFD.FIX.BG32TEM1.D1%%ODATE PURGE SCRATCH                 00002700
  DELETE PEBP.BGFD.FIX.BG32TEM2.D1%%ODATE PURGE SCRATCH                 00002800
  DELETE PEBP.BGFD.FIX.BG32TEM3.D1%%ODATE PURGE SCRATCH                 00002900
  DELETE PEBP.BGFD.FIX.BG32TEM4.D1%%ODATE PURGE SCRATCH                 00003000
  DELETE PEBP.BGFD.FIX.BG32TEM5.D1%%ODATE PURGE SCRATCH                 00003100
  DELETE PEBP.BGFD.FIX.BG320000.D1%%ODATE PURGE SCRATCH                 00003200
  DELETE PEBP.BGFD.FIX.BG32TM01.D1%%ODATE PURGE SCRATCH                 00003300
  DELETE PEBP.BGFD.FIX.BG320001.D1%%ODATE PURGE SCRATCH                 00003400
  DELETE PEBP.BGFD.FIX.BG32TM02.D1%%ODATE PURGE SCRATCH                 00003500
  DELETE PEBP.BGFD.FIX.BG320002.D1%%ODATE PURGE SCRATCH                 00003600
  DELETE PEBP.BGFD.FIX.BG32TM03.D1%%ODATE PURGE SCRATCH                 00003700
  DELETE PEBP.BGFD.FIX.BG320003.D1%%ODATE PURGE SCRATCH                 00003800
  DELETE PEBP.BGFD.FIX.BG32TM08.D1%%ODATE PURGE SCRATCH                 00003900
  DELETE PEBP.BGFD.FIX.BG320008.D1%%ODATE PURGE SCRATCH                 00004000
  DELETE PEBP.BGFD.FIX.BG32TM14.D1%%ODATE PURGE SCRATCH                 00004100
  DELETE PEBP.BGFD.FIX.BG320014.D1%%ODATE PURGE SCRATCH                 00004200
  DELETE PEBP.BGFD.FIX.BG32TM18.D1%%ODATE PURGE SCRATCH                 00004300
  DELETE PEBP.BGFD.FIX.BG320018.D1%%ODATE PURGE SCRATCH                 00004400
  DELETE PEBP.BGFD.FIX.BG32TM07.D1%%ODATE PURGE SCRATCH                 00004500
  DELETE PEBP.BGFD.FIX.BG320007.D1%%ODATE PURGE SCRATCH                 00004600
  SET MAXCC=0                                                           00004700
//*                                                                     00004800
//*IGH001 --> I                                                         00004900
//*--------------------------------------------------------------------*00005000
//*- DESCARGA DE LA TABLA BGDTTAB                                      *00005100
//*--------------------------------------------------------------------*00005200
//BL320098 EXEC PGM=ADUUMAIN,COND=(4,LT),                               00005300
//         PARM='DSNP,BL320098,NEW,,MSGLEVEL(1)'                        00005400
//STEPLIB  DD DISP=SHR,DSN='BMC.DB2.LOAD.DSNP'                          00005500
//SYSPRINT DD SYSOUT=*                                                  00005600
//UTPRINT  DD SYSOUT=*                                                  00005700
//SYSOUT   DD SYSOUT=*                                                  00005800
//SYSCNTL  DD SYSOUT=*                                                  00005900
//SYSREC00 DD DSN=PEBP.BGFD.FIX.UNLO.BGDTTAB.D1%%ODATE,                 00006000
//            DISP=(NEW,CATLG,DELETE),UNIT=3390,                        00006100
//            DCB=(RECFM=FB,LRECL=290,BLKSIZE=0,DSORG=PS),              00006200
//            SPACE=(CYL,(100,50),RLSE)                                 00006300
//SYSIN     DD *                                                        00006400
  UNLOAD                                                                00006500
    SHRLEVEL CHANGE CONSISTENT YES XBMID XBMP                           00006600
    UNLOADDN SYSREC00                                                   00006700
    CNTLDDN  SYSCNTL                                                    00006800
    FIXEDVARCHAR YES                                                    00006900
    SELECT * FROM MBVP.BGDTTAB;                                         00007000
//*                                                                     00007100
//*--------------------------------------------------------------------*00007200
//* SORT SE FILTRA PARA OBTENER LOS CORASUS Y SU CLASIFICACION         *00007300
//*--------------------------------------------------------------------*00007400
//BL320097 EXEC PGM=ICEMAN,COND=(4,LT)                                  00007500
//SYSOUT   DD SYSOUT=*                                                  00007600
//SORTIN   DD DSN=PEBP.BGFD.FIX.UNLO.BGDTTAB.D1%%ODATE,DISP=SHR         00007700
//SORTOUT  DD DSN=PEBP.BGFD.FIX.SORT.BGDTTAB.D1%%ODATE,                 00007800
//            DISP=(NEW,CATLG,DELETE),UNIT=3390,                        00007900
//            DCB=(RECFM=FB,LRECL=290,BLKSIZE=0,DSORG=PS),              00008000
//            SPACE=(CYL,(100,50),RLSE)                                 00008100
//SYSIN    DD *                                                         00008200
  SORT FIELDS=(01,05,CH,A,071,03,CH,A)                                  00008300
  INCLUDE COND=(01,05,CH,EQ,C'00418')                                   00008400
//*IGH001 <-- F                                                         00008500
//*                                                                     00008600
//*--------------------------------------------------------------------*00008700
//* SORT    FORMATEA ARCHIVO MAESALDO-MAESALDI                         *00008800
//*--------------------------------------------------------------------*00008900
//BL320095 EXEC  PGM=ICEMAN,COND=(4,LT)                                 00009000
//SYSOUT   DD SYSOUT=*                                                  00009100
//SORTIN   DD DSN=PEB&AMB..BGFD.FIX.MAESALDO.D1%%ODATE,DISP=SHR,        00009200
//            DCB=(BUFNO=20)                                            00009300
//         DD DSN=PEB&AMB..BGFD.FIX.MAESALDI.D1%%ODATE,DISP=SHR,        00009400
//            DCB=(BUFNO=20)                                            00009500
//SORTOUT  DD DSN=PEB&AMB..BGFD.FIX.BG32TEM1.D1%%ODATE,                 00009600
//            UNIT=3390,                                                00009700
//            DISP=(NEW,CATLG,DELETE),                                  00009800
//            SPACE=(CYL,(700,350),RLSE),                               00009900
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00010000
//SYSIN    DD *                                                         00010100
  SORT FIELDS=COPY                                                      00010200
  INCLUDE COND=(9,2,CH,EQ,C'01',OR,9,2,CH,EQ,C'02',OR,                  00010300
                9,2,CH,EQ,C'03',OR,9,2,CH,EQ,C'07',OR,                  00010400
                9,2,CH,EQ,C'08',OR,9,2,CH,EQ,C'14')                     00010500
  OUTREC FIELDS=(C'        1 ',1,230,40X)                               00010600
//*--------------------------------------------------------------------*00010700
//* SORT    FORMATEA ARCHIVO BQPDTCHQ                                  *00010800
//*--------------------------------------------------------------------*00010900
//BL320090 EXEC PGM=ICEMAN,COND=(4,LT)                                  00011000
//SYSOUT   DD SYSOUT=*                                                  00011100
//SORTIN   DD DSN=PEB&AMB..BGFD.FIX.BQPDTCHQ.D1%%ODATE,DISP=SHR,        00011200
//            DCB=(BUFNO=20)                                            00011300
//*IGH001 --> I                                                         00011310
//* SORTOUT  DD DSN=PEB&AMB..BGFD.FIX.BG32TEM2.D1%%ODATE,               00011400
//SORTOUT  DD DSN=PEB&AMB..BGFD.FIX.BG32TEM9.D1%%ODATE,                 00011500
//*IGH001 <-- F                                                         00011510
//            UNIT=3390,                                                00011600
//            DISP=(NEW,CATLG,DELETE),                                  00011710
//            SPACE=(CYL,(700,350),RLSE),                               00011800
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00011900
//SYSIN    DD *                                                         00012000
  SORT FIELDS=COPY                                                      00012100
  INCLUDE COND=(38,2,CH,NE,C'09')                                       00012200
  OUTREC FIELDS=(C'        2 ',1,150,120X)                              00012300
//*IGH001 --> I                                                         00012310
//**********************************************************************00012400
//* SORT : JOINKEY PARA OBTENER LA DIVISA                              *00012500
//**********************************************************************00012600
//BL320087 EXEC PGM=ICEMAN,COND=(0,LT)                                  00012700
//SYSOUT   DD SYSOUT=*                                                  00012800
//SORTJNF1 DD DSN=PEBP.BGFD.FIX.BG32TEM9.D1%%ODATE,DISP=SHR             00012900
//SORTJNF2 DD DSN=PEBP.BGFD.FIX.BG32TEM1.D1%%ODATE,DISP=SHR             00013000
//*                                                                     00013100
//OUTF1    DD DSN=PEBP.BGFD.TMP.FIL3OUT1.D1%%ODATE,                     00013200
//            DISP=(NEW,CATLG,DELETE),UNIT=3390,                        00013300
//            SPACE=(CYL,(800,400),RLSE),                               00013400
//            DCB=(RECFM=FB,BLKSIZE=0,DSORG=PS)                         00013500
//*                                                                     00013600
//OUTF2    DD DSN=PEBP.BGFD.TMP.FIL3OUT2.D1%%ODATE,                     00013700
//            DISP=(NEW,CATLG,DELETE),UNIT=3390,                        00013800
//            SPACE=(CYL,(800,400),RLSE),                               00013900
//            DCB=(RECFM=FB,BLKSIZE=0,DSORG=PS)                         00014000
//*                                                                     00014100
//MATCH    DD DSN=PEBP.BGFD.FIX.BG32TEM2.D1%%ODATE,                     00014200
//            DISP=(NEW,CATLG,DELETE),UNIT=3390,                        00014300
//            SPACE=(CYL,(800,400),RLSE),                               00014400
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00014500
//*                                                                     00014600
//SYSIN    DD *                                                         00014700
  JOINKEYS FILE=F1,FIELDS=(11,18,A)                                     00014800
  JOINKEYS FILE=F2,FIELDS=(11,18,A)                                     00014900
  JOIN UNPAIRED,F1,F2                                                   00015000
  REFORMAT FIELDS=(F1:01,280,F2:01,280,?)                               00015100
  SORT FIELDS=COPY                                                      00015200
  OUTFIL FNAMES=OUTF1,INCLUDE=(561,01,CH,EQ,C'1'),BUILD(001,280)        00015300
  OUTFIL FNAMES=OUTF2,INCLUDE=(561,01,CH,EQ,C'2'),BUILD(281,280)        00015400
  OUTFIL FNAMES=MATCH,INCLUDE=(561,01,CH,EQ,C'B'),                      00015500
         BUILD(001,037,332,004,042,239)                                 00015600
//*                                                                     00015700
//*IGH001 <-- F                                                         00015710
//**********************************************************************00015800
//*--------------------------------------------------------------------*00015900
//* SORT    CLASIFICA ARCHIVO POR CODIGO DE CONTRATO                   *00016000
//*--------------------------------------------------------------------*00016100
//BL320085 EXEC  PGM=ICEMAN,COND=(4,LT)                                 00016200
//SYSOUT   DD SYSOUT=*                                                  00016300
//SORTIN   DD DSN=PEB&AMB..BGFD.FIX.BG32TEM1.D1%%ODATE,DISP=SHR,        00016400
//            DCB=(BUFNO=20)                                            00016500
//         DD DSN=PEB&AMB..BGFD.FIX.BG32TEM2.D1%%ODATE,DISP=SHR,        00016600
//            DCB=(BUFNO=20)                                            00016700
//SORTOUT  DD DSN=PEB&AMB..BGFD.FIX.BG32TEM3.D1%%ODATE,                 00016800
//            UNIT=3390,                                                00016900
//            DISP=(NEW,CATLG,DELETE),                                  00017100
//            SPACE=(CYL,(1500,1000),RLSE),                             00017100
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00017200
//SYSIN    DD *                                                         00017300
  SORT FIELDS=(11,18,CH,A,9,1,CH,A)                                     00017400
//*--------------------------------------------------------------------*00017500
//*  BG3C3200  ACTUALIZA CODIGO CENTRAL EN ARCHIVO TEMPORAL            *00017600
//*--------------------------------------------------------------------*00017700
//BL320080 EXEC PGM=BG3C3200,COND=(4,LT)                                00017800
//SYSOUT   DD SYSOUT=*                                                  00017900
//SYSPRINT DD SYSOUT=*                                                  00018000
//SYSABOUT DD SYSOUT=*                                                  00018100
//SYSDBOUT DD SYSOUT=*                                                  00018200
//SYSTSPRT DD SYSOUT=*                                                  00018300
//*                                                                     00018400
//E1DQANX0 DD DSN=PEB&AMB..BGFD.FIX.BG32TEM3.D1%%ODATE,                 00018500
//            DISP=SHR,DCB=(BUFNO=20)                                   00018600
//E3DQCLI0 DD DSN=PEBP.BGFD.FIX.PEDT008.SYSREC01.SORT.D1&FECHA,         00018700
//            DISP=SHR                                                  00018800
//         DD DSN=PEBP.BGFD.FIX.PEDT008.SYSREC02.SORT.D1&FECHA,         00018900
//            DISP=SHR                                                  00019000
//         DD DSN=PEBP.BGFD.FIX.PEDT008.SYSREC03.SORT.D1&FECHA,         00019100
//            DISP=SHR                                                  00019200
//         DD DSN=PEBP.BGFD.FIX.PEDT008.SYSREC04.SORT.D1&FECHA,         00019300
//            DISP=SHR                                                  00019400
//         DD DSN=PEBP.BGFD.FIX.PEDT008.SYSREC05.SORT.D1&FECHA,         00019500
//            DISP=SHR                                                  00019600
//         DD DSN=PEBP.BGFD.FIX.PEDT008.SYSREC06.SORT.D1&FECHA,         00019700
//            DISP=SHR                                                  00019800
//         DD DSN=PEBP.BGFD.FIX.PEDT008.SYSREC07.SORT.D1&FECHA,         00019900
//            DISP=SHR                                                  00020000
//         DD DSN=PEBP.BGFD.FIX.PEDT008.SYSREC08.SORT.D1&FECHA,         00020100
//            DISP=SHR                                                  00020200
//         DD DSN=PEBP.BGFD.FIX.PEDT008.SYSREC09.SORT.D1&FECHA,         00020300
//            DISP=SHR                                                  00020400
//         DD DSN=PEBP.BGFD.FIX.PEDT008.SYSREC10.SORT.D1&FECHA,         00020500
//            DISP=SHR                                                  00020600
//*                                                                     00020700
//*--------------------------------------------------------------------*00020800
//* SORT    CLASIFICA ARCHIVO POR CODIGO CENTRAL                       *00020900
//*--------------------------------------------------------------------*00021000
//BL320070 EXEC PGM=ICEMAN,COND=(4,LT)                                  00021100
//SYSOUT   DD SYSOUT=*                                                  00021200
//SORTIN   DD DSN=PEB&AMB..BGFD.FIX.BG32TEM3.D1%%ODATE,                 00021300
//            DISP=SHR,DCB=(BUFNO=20)                                   00021400
//SORTOUT  DD DSN=PEB&AMB..BGFD.FIX.BG32TEM5.D1%%ODATE,                 00021500
//            UNIT=3390,                                                00021600
//            DISP=(NEW,CATLG,DELETE),                                  00021710
//            SPACE=(CYL,(800,400),RLSE),                               00021800
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00021900
//SYSIN    DD *                                                         00022000
  SORT FIELDS=(1,8,CH,A,9,1,CH,A,62,3,CH,A)                             00022100
//*--------------------------------------------------------------------*00022200
//* SORT    CLASIFICA ARCHIVO POR CODIGO CENTRAL                       *00022300
//*--------------------------------------------------------------------*00022400
//BL320068 EXEC PGM=ICEMAN,COND=(4,LT)                                  00022500
//SYSOUT   DD SYSOUT=*                                                  00022600
//SORTIN   DD DSN=PEBP.BGFD.FIX.BG32TEM5.D1%%ODATE,                     00022700
//            DISP=SHR,DCB=(BUFNO=20)                                   00022800
//SORTOUT  DD DSN=PEBP.BGFD.FIX.BG32TE55.D1%%ODATE,                     00022900
//            UNIT=3390,                                                00023000
//            DISP=(NEW,CATLG,DELETE),                                  00023100
//            SPACE=(CYL,(800,400),RLSE),                               00023200
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00023300
//SYSIN    DD *                                                         00023400
  SORT FIELDS=(11,18,CH,A)                                              00023500
//*--------------------------------------------------------------------*00023600
//BGF11232  EXEC PGM=ICEMAN,COND=(0,LT)                                 00023700
//SYSOUT    DD SYSOUT=*                                                 00023800
//SYSPRINT  DD SYSOUT=*                                                 00023900
//SORTIN    DD DSN=PEBP.PEFD.FIX.DATASBSM.D1%%ODATE,DISP=SHR            00024000
//SORTOUT   DD DSN=PEBP.BGFD.TMP.WF112501.D1%%ODATE,                    00024100
//             UNIT=(3390),SPACE=(CYL,(1000,100),RLSE),                 00024200
//             DCB=(RECFM=FB,LRECL=127,BLKSIZE=0,DSORG=PS),             00024300
//             DISP=(NEW,CATLG,DELETE)                                  00024400
//SYSIN     DD *                                                        00024500
   SORT     FIELDS=(084,18,A),                                          00024600
                    FORMAT=CH                                           00024700
   RECORD   TYPE=F,LENGTH=127                                           00024800
/*                                                                      00024900
//**********************************************************************00025000
//* PASO        : BGF11245                                             *00025100
//* DESCRIPCION : SE OBTIENEN LOS TITULARES DE LAS CUENTAS CORRESPON-  *00025200
//*               DIENTES A INTERESES DEVENGADOS.                      *00025300
//**********************************************************************00025400
//BG93K010 EXEC PGM=IKJEFT1A,COND=(4,LT)                                00025500
//SYSIN    DD DUMMY                                                     00025600
//SYSOUT   DD SYSOUT=*                                                  00025700
//QRLSDB2  DD SYSOUT=*                                                  00025800
//SYSTSPRT DD SYSOUT=*                                                  00025900
//SYSPRINT DD SYSOUT=*                                                  00026000
//*IGH001 --> I                                                         00026010
//E1CORASU DD  DSN=PEBP.BGFD.FIX.SORT.BGDTTAB.D1&FECHA,DISP=SHR         00026100
//*IGH001 <-- F                                                         00026110
//E1DQF122  DD DSN=PEBP.BGFD.FIX.BG32TE55.D1%%ODATE,DISP=SHR            00026200
//E2DQF122  DD DSN=PEBP.BGFD.TMP.WF112501.D1%%ODATE,DISP=SHR            00026300
//S1DQF122  DD DSN=PEBP.BGFD.TMP.BG32TE00.D1%%ODATE,                    00026400
//             UNIT=(3390),SPACE=(CYL,(1000,100),RLSE),                 00026500
//             DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS),             00026600
//             DISP=(NEW,CATLG,DELETE)                                  00026700
//SYS005    DD SYSOUT=(J,,0001),COPIES=01,FCB=H001                      00026800
//SYSTSIN  DD *                                                         00026900
 DSN SYSTEM(DSNP)                                                       00027000
 RUN PROGRAM(BG3C3209) PLAN(BVPBGPB) PARM('%%SIGLO.%%ODATE.00000000')   00027100
 END                                                                    00027200
//*                                                                     00027300
//**********************************************************************00027400
//* ORDENA FILE DE PAGO DE HABERES POR CODIGO DE EMISORA               *00027500
//**********************************************************************00027600
//*BG973797 EXEC  PGM=ICEMAN,COND=(4,LT)                                00027700
//*SORTIN   DD DSN=PEBP.BGFD.TMP.BG32TE00.D1%%ODATE,DISP=SHR            00027800
//*SORTOUT  DD DSN=PEBP.BGFD.TMP.BG32TE01.D1%%ODATE,                    00027900
//*            DISP=(NEW,CATLG,DELETE),                                 00028000
//*            UNIT=3390,                                               00028100
//*            SPACE=(CYL,(500,250),RLSE),                              00028200
//*            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)              00028300
//*SYSOUT   DD SYSOUT=*                                                 00028400
//*SYSIN    DD *                                                        00028500
//**   SORT FIELDS=(1,28,CH,A)                                          00028600
//*                                                                     00028700
//**********************************************************************00028800
//* SELECCIONA UN REGISTRO A ELIMINAR                                 -*00028900
//**********************************************************************00029000
//*BG973796 EXEC  PGM=ICETOOL,COND=(4,LT)                               00029100
//*TOOLMSG  DD SYSOUT=*                                                 00029200
//*DFSMSG   DD SYSOUT=*                                                 00029300
//*SYSPRINT DD SYSOUT=*                                                 00029400
//*SYSUDUMP DD SYSOUT=*                                                 00029500
//*TOOLIN   DD *                                                        00029600
//**    MODE STOP                                                       00029700
//**    SELECT FROM(DATA1) TO(DATA2) ON(1,28,CH) FIRST                  00029800
//*DATA1    DD DSN=PEBP.BGFD.TMP.BG32TE01.D1%%ODATE,DISP=SHR            00029900
//*DATA2    DD DSN=PEBP.BGFD.TMP.BG32TE02.D1%%ODATE,                    00030000
//*            DISP=(NEW,CATLG,DELETE),                                 00030100
//*            UNIT=3390,SPACE=(CYL,(500,250),RLSE),                    00030200
//*            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)              00030300
//*--------------------------------------------------------------------*00030400
//* SORT    CLASIFICA ARCHIVO POR CODIGO CENTRAL                       *00030500
//*--------------------------------------------------------------------*00030600
//BG973795 EXEC PGM=ICEMAN,COND=(4,LT)                                  00030700
//SYSOUT   DD SYSOUT=*                                                  00030800
//SORTIN   DD DSN=PEBP.BGFD.TMP.BG32TE00.D1%%ODATE,                     00030900
//            DISP=SHR,DCB=(BUFNO=20)                                   00031000
//SORTOUT  DD DSN=PEBP.BGFD.FIX.BG32TE01.D1%%ODATE,                     00031100
//            UNIT=3390,                                                00031200
//            DISP=(NEW,CATLG,DELETE),                                  00031300
//            SPACE=(CYL,(800,400),RLSE),                               00031400
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00031500
//SYSIN    DD *                                                         00031600
      SORT FIELDS=(1,8,CH,A,9,1,CH,A,62,3,CH,A)                         00031710
//*---------------------------------------------------------------------00031800
//*- CREACION DE SYSTSIN PARA     PRG BG3C321W                          00031900
//*---------------------------------------------------------------------00032000
//BL320065 EXEC PGM=QU3CSTS,PARM=('&AMB,BG3C321W,&FECHA,'),             00032100
//         COND=(4,LT)                                                  00032200
//E1DQDAE0 DD DSN=PEBP.QGFD.FIX.A000DAE0.X0000,DISP=SHR                 00032300
//SYSTSIN  DD DISP=(NEW,PASS,DELETE),UNIT=3390,                         00032400
//            SPACE=(CYL,(100,50),RLSE),RECFM=FB,LRECL=100              00032500
//IGZSRTCD DD DUMMY                                                     00032600
//*                                                                     00032700
//*--------------------------------------------------------------------*00032800
//*         EJECUCION DEL PROGRAMA  BG3C321W  ANEXO-13 X CLIENTE       *00032900
//*--------------------------------------------------------------------*00033000
//BL320064 EXEC PGM=IKJEFT01,COND=(4,LT)                                00033100
//S1DQ0000 DD SYSOUT=*                                                  00033200
//SYSOUT   DD SYSOUT=*                                                  00033300
//SYSPRINT DD SYSOUT=*                                                  00033400
//SYSTSPRT DD SYSOUT=*                                                  00033500
//SYSDBOUT DD SYSOUT=Z                                                  00033600
//SYSABOUT DD SYSOUT=Z                                                  00033700
//SYSUDUMP DD SYSOUT=Z                                                  00033800
//TCDF0000 DD DSN=PEB&AMB..TCFD.VSL.TCDF0001,DISP=SHR,                  00033900
//            AMP=('BUFND=80,BUFNI=32')                                 00034000
//*IGH001 --> I                                                         00034010
//E1CORASU DD  DSN=PEBP.BGFD.FIX.SORT.BGDTTAB.D1&FECHA,DISP=SHR         00034100
//*IGH001 <-- F                                                         00034110
//E1DQDAE0 DD DSN=PEB&AMB..QGFD.FIX.A000DAE0.X0000,DISP=SHR             00034200
//E1DQANX0 DD DSN=PEB&AMB..BGFD.FIX.BG32TE01.D1%%ODATE,                 00034300
//            DISP=SHR,DCB=(BUFNO=20)                                   00034400
//S1DQAX13 DD DSN=PEB&AMB..BGFD.FIX.BGLS3200.D1%%ODATE,                 00034500
//            DISP=(NEW,CATLG,DELETE),                                  00034600
//            UNIT=3390,                                                00034700
//            SPACE=(CYL,(100,50),RLSE),                                00034800
//            DCB=(LRECL=174,RECFM=FB,BLKSIZE=0,DSORG=PS)               00034900
//S2DQTOTL DD DSN=PEB&AMB..BGFD.FIX.BG320000.D1%%ODATE,                 00035000
//            DISP=(NEW,CATLG,DELETE),                                  00035100
//            UNIT=3390,                                                00035200
//            SPACE=(CYL,(100,50),RLSE),                                00035300
//            DCB=(LRECL=163,RECFM=FB,BLKSIZE=0,DSORG=PS)               00035400
//S1DLINC0 DD SYSOUT=*                                                  00035500
//QRLSDB2  DD SYSOUT=*                                                  00035600
//SYSIN    DD DUMMY                                                     00035700
//SYSTSIN  DD DSN=*.BL320065.SYSTSIN,DISP=(OLD,DELETE,DELETE)           00035800
//*                                                                     00035900
//*--------------------------------------------------------------------*00036000
//*   PASO A CONTROL-D.                                                *00036100
//*--------------------------------------------------------------------*00036200
//*BL320060 EXEC PGM=ICEGENER,COND=(0,NE)                               00036300
//*SYSUT1   DD DSN=PEB&AMB..BGFD.FIX.BGLS3200.D1%%ODATE,DISP=SHR        00036400
//*SYSUT2   DD  SUBSYS=CDAM                                             00036500
//*SYSPRINT DD SYSOUT=*                                                 00036600
//*SYSIN    DD DUMMY                                                    00036700
//*--------------------------------------------------------------------*00036800
//* SORT    SEPARA ARCHIVO POR TIPO DE PRODUCTO                        *00036900
//*--------------------------------------------------------------------*00037000
//BL320055 EXEC PGM=ICEMAN,COND=(4,LT)                                  00037100
//SYSOUT   DD SYSOUT=*                                                  00037200
//SORTIN   DD DSN=PEB&AMB..BGFD.FIX.BG32TE01.D1%%ODATE,                 00037300
//            DISP=SHR,DCB=(BUFNO=20)                                   00037400
//SORT00A  DD DSN=PEB&AMB..BGFD.FIX.BG32TM01.D1%%ODATE,                 00037500
//            UNIT=3390,                                                00037600
//            DISP=(NEW,CATLG,DELETE),                                  00037700
//            SPACE=(CYL,(500,250),RLSE),                               00037800
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00037900
//SORT00B  DD DSN=PEB&AMB..BGFD.FIX.BG32TM02.D1%%ODATE,                 00038000
//            UNIT=3390,                                                00038100
//            DISP=(NEW,CATLG,DELETE),                                  00038200
//            SPACE=(CYL,(500,250),RLSE),                               00038300
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00038400
//SORT00C  DD DSN=PEB&AMB..BGFD.FIX.BG32TM03.D1%%ODATE,                 00038500
//            UNIT=3390,                                                00038600
//            DISP=(NEW,CATLG,DELETE),                                  00038700
//            SPACE=(CYL,(500,250),RLSE),                               00038800
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00038900
//SORT00D  DD DSN=PEB&AMB..BGFD.FIX.BG32TM08.D1%%ODATE,                 00039000
//            UNIT=3390,                                                00039100
//            DISP=(NEW,CATLG,DELETE),                                  00039200
//            SPACE=(CYL,(500,250),RLSE),                               00039300
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00039400
//SORT00E  DD DSN=PEB&AMB..BGFD.FIX.BG32TM14.D1%%ODATE,                 00039500
//            UNIT=3390,                                                00039600
//            DISP=(NEW,CATLG,DELETE),                                  00039700
//            SPACE=(CYL,(500,250),RLSE),                               00039800
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00039900
//SORT00F  DD DSN=PEB&AMB..BGFD.FIX.BG32TM18.D1%%ODATE,                 00040000
//            UNIT=3390,                                                00040100
//            DISP=(NEW,CATLG,DELETE),                                  00040200
//            SPACE=(CYL,(500,250),RLSE),                               00040300
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00040400
//SORT00G  DD DSN=PEB&AMB..BGFD.FIX.BG32TM07.D1%%ODATE,                 00040500
//            UNIT=3390,                                                00040600
//            DISP=(NEW,CATLG,DELETE),                                  00040700
//            SPACE=(CYL,(500,250),RLSE),                               00040800
//            DCB=(RECFM=FB,LRECL=280,BLKSIZE=0,DSORG=PS)               00040900
//*200902167                                                            00041000
//*OUTFIL INCLUDE=(9,1,CH,EQ,C'3'),FNAMES=SORT00G                       00041100
//*200902167-F                                                          00041200
//SYSIN    DD *                                                         00041300
  SORT FIELDS=COPY                                                      00041400
  OUTFIL INCLUDE=(9,1,CH,EQ,C'1',&,46,2,CH,EQ,C'01'),FNAMES=SORT00A     00041500
  OUTFIL INCLUDE=(9,1,CH,EQ,C'1',&,46,2,CH,EQ,C'02'),FNAMES=SORT00B     00041600
  OUTFIL INCLUDE=(9,1,CH,EQ,C'1',&,46,2,CH,EQ,C'03'),FNAMES=SORT00C     00041710
  OUTFIL INCLUDE=(9,1,CH,EQ,C'1',&,46,2,CH,EQ,C'08'),FNAMES=SORT00D     00041800
  OUTFIL INCLUDE=(9,1,CH,EQ,C'1',&,46,2,CH,EQ,C'14'),FNAMES=SORT00E     00041900
  OUTFIL INCLUDE=(9,1,CH,EQ,C'2'),FNAMES=SORT00F                        00042000
  OUTFIL INCLUDE=(9,1,CH,EQ,C'1',&,46,2,CH,EQ,C'07'),FNAMES=SORT00G     00042100
//*---------------------------------------------------------------------00042200
//*- CREACION DE SYSTSIN PARA     PRG BG3C321W                          00042300
//*---------------------------------------------------------------------00042400
//BL320050 EXEC PGM=QU3CSTS,PARM=('&AMB,BG3C321W,&FECHA,&PR01'),        00042500
//         COND=(4,LT)                                                  00042600
//E1DQDAE0 DD DSN=PEBP.QGFD.FIX.A000DAE0.X0000,DISP=SHR                 00042700
//SYSTSIN  DD DISP=(NEW,PASS,DELETE),UNIT=3390,                         00042800
//            SPACE=(CYL,(100,50),RLSE),RECFM=FB,LRECL=100              00042900
//IGZSRTCD DD DUMMY                                                     00043000
//*                                                                     00043100
//*--------------------------------------------------------------------*00043200
//*         EJECUCION DEL PROGRAMA  BG3C321W  ANEXO-13 X CLIENTE       *00043300
//*--------------------------------------------------------------------*00043400
//BL320049 EXEC PGM=IKJEFT01,COND=(4,LT)                                00043500
//S1DQ0000 DD SYSOUT=*                                                  00043600
//SYSOUT   DD SYSOUT=*                                                  00043700
//SYSPRINT DD SYSOUT=*                                                  00043800
//SYSTSPRT DD SYSOUT=*                                                  00043900
//SYSDBOUT DD SYSOUT=Z                                                  00044000
//SYSABOUT DD SYSOUT=Z                                                  00044100
//SYSUDUMP DD SYSOUT=Z                                                  00044200
//TCDF0000 DD DSN=PEB&AMB..TCFD.VSL.TCDF0001,DISP=SHR,                  00044300
//            AMP=('BUFND=80,BUFNI=32')                                 00044400
//*IGH001 --> I                                                         00044410
//E1CORASU DD  DSN=PEBP.BGFD.FIX.SORT.BGDTTAB.D1&FECHA,DISP=SHR         00044500
//*IGH001 <-- F                                                         00044510
//E1DQDAE0 DD DSN=PEB&AMB..QGFD.FIX.A000DAE0.X0000,DISP=SHR             00044600
//E1DQANX0 DD DSN=PEB&AMB..BGFD.FIX.BG32TM01.D1%%ODATE,                 00044700
//            DISP=SHR,DCB=(BUFNO=20)                                   00044800
//*PSDAEMF2-446-INI
//*S1DQAX13 DD  SUBSYS=CDAM                                             00044900
//S1DQAX13 DD DSN=PEB&AMB..BGFD.FIX.BGLS3201.D1%%ODATE,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=3390,
//            SPACE=(CYL,(300,150),RLSE),
//            DCB=(LRECL=174,RECFM=FB,BLKSIZE=0,DSORG=PS)
//*PSDAEMF2-446-FIN
//S2DQTOTL DD DSN=PEB&AMB..BGFD.FIX.BG320001.D1%%ODATE,                 00045000
//            DISP=(NEW,CATLG,DELETE),                                  00045100
//            UNIT=3390,                                                00045200
//            SPACE=(CYL,(100,50),RLSE),                                00045300
//            DCB=(LRECL=163,RECFM=FB,BLKSIZE=0,DSORG=PS)               00045400
//S1DLINC0 DD SYSOUT=*                                                  00045500
//QRLSDB2  DD SYSOUT=*                                                  00045600
//SYSIN    DD DUMMY                                                     00045700
//SYSTSIN  DD DSN=*.BL320050.SYSTSIN,DISP=(OLD,DELETE,DELETE)           00045800
//*--------------------------------------------------------------------*00045900
//*- CREACION DE SYSTSIN PARA     PRG BG3C321W                          00046000
//*---------------------------------------------------------------------00046100
//BL320045 EXEC PGM=QU3CSTS,PARM=('&AMB,BG3C321W,&FECHA,&PR02'),        00046200
//         COND=(4,LT)                                                  00046300
//E1DQDAE0 DD DSN=PEBP.QGFD.FIX.A000DAE0.X0000,DISP=SHR                 00046400
//SYSTSIN  DD DISP=(NEW,PASS,DELETE),UNIT=3390,                         00046500
//            SPACE=(CYL,(100,50),RLSE),RECFM=FB,LRECL=100              00046600
//IGZSRTCD DD DUMMY                                                     00046700
//*                                                                     00046800
//*--------------------------------------------------------------------*00046900
//*         EJECUCION DEL PROGRAMA  BG3C321W  ANEXO-13 X CLIENTE       *00047000
//*--------------------------------------------------------------------*00047100
//BL320044 EXEC  PGM=IKJEFT01,COND=(4,LT)                               00047200
//S1DQ0000 DD SYSOUT=*                                                  00047300
//SYSOUT   DD SYSOUT=*                                                  00047400
//SYSPRINT DD SYSOUT=*                                                  00047500
//SYSTSPRT DD SYSOUT=*                                                  00047600
//SYSDBOUT DD SYSOUT=Z                                                  00047700
//SYSABOUT DD SYSOUT=Z                                                  00047800
//SYSUDUMP DD SYSOUT=Z                                                  00047900
//TCDF0000 DD DSN=PEB&AMB..TCFD.VSL.TCDF0001,DISP=SHR,                  00048000
//            AMP=('BUFND=80,BUFNI=32')                                 00048100
//*IGH001 --> I                                                         00048110
//E1CORASU DD  DSN=PEBP.BGFD.FIX.SORT.BGDTTAB.D1&FECHA,DISP=SHR         00048200
//*IGH001 <-- F                                                         00048210
//E1DQDAE0 DD DSN=PEB&AMB..QGFD.FIX.A000DAE0.X0000,DISP=SHR             00048300
//E1DQANX0 DD DSN=PEB&AMB..BGFD.FIX.BG32TM02.D1%%ODATE,                 00048400
//            DISP=SHR,DCB=(BUFNO=20)                                   00048500
//*PSDAEMF2-446-INI
//*S1DQAX13 DD  SUBSYS=CDAM                                             00044900
//S1DQAX13 DD DSN=PEB&AMB..BGFD.FIX.BGLS3202.D1%%ODATE,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=3390,
//            SPACE=(CYL,(300,150),RLSE),
//            DCB=(LRECL=174,RECFM=FB,BLKSIZE=0,DSORG=PS)
//*PSDAEMF2-446-FIN
//S2DQTOTL DD DSN=PEB&AMB..BGFD.FIX.BG320002.D1%%ODATE,                 00048700
//            DISP=(NEW,CATLG,DELETE),                                  00048800
//            UNIT=3390,                                                00048900
//            SPACE=(CYL,(100,50),RLSE),                                00049000
//            DCB=(LRECL=163,RECFM=FB,BLKSIZE=0,DSORG=PS)               00049100
//S1DLINC0 DD SYSOUT=*                                                  00049200
//QRLSDB2  DD SYSOUT=*                                                  00049300
//SYSIN    DD DUMMY                                                     00049400
//SYSTSIN  DD DSN=*.BL320045.SYSTSIN,DISP=(OLD,DELETE,DELETE)           00049500
//*--------------------------------------------------------------------*00049600
//*- CREACION DE SYSTSIN PARA     PRG BG3C321W                          00049700
//*---------------------------------------------------------------------00049800
//BL320040 EXEC PGM=QU3CSTS,PARM=('&AMB,BG3C321W,&FECHA,&PR03'),        00049900
//         COND=(4,LT)                                                  00050000
//E1DQDAE0 DD DSN=PEBP.QGFD.FIX.A000DAE0.X0000,DISP=SHR                 00050100
//SYSTSIN  DD DISP=(NEW,PASS,DELETE),UNIT=3390,                         00050200
//            SPACE=(CYL,(100,50),RLSE),RECFM=FB,LRECL=100              00050300
//IGZSRTCD DD DUMMY                                                     00050400
//*                                                                     00050500
//*--------------------------------------------------------------------*00050600
//*         EJECUCION DEL PROGRAMA  BG3C321W  ANEXO-13 X CLIENTE       *00050700
//*--------------------------------------------------------------------*00050800
//BL320039 EXEC PGM=IKJEFT01,COND=(4,LT)                                00050900
//S1DQ0000 DD SYSOUT=*                                                  00051000
//SYSOUT   DD SYSOUT=*                                                  00051100
//SYSPRINT DD SYSOUT=*                                                  00051200
//SYSTSPRT DD SYSOUT=*                                                  00051300
//SYSDBOUT DD SYSOUT=Z                                                  00051400
//SYSABOUT DD SYSOUT=Z                                                  00051500
//SYSUDUMP DD SYSOUT=Z                                                  00051600
//TCDF0000 DD DSN=PEB&AMB..TCFD.VSL.TCDF0001,DISP=SHR,                  00051710
//            AMP=('BUFND=80,BUFNI=32')                                 00051800
//*IGH001 --> I                                                         00051810
//E1CORASU DD  DSN=PEBP.BGFD.FIX.SORT.BGDTTAB.D1&FECHA,DISP=SHR         00051900
//*IGH001 <-- F                                                         00051910
//E1DQDAE0 DD DSN=PEB&AMB..QGFD.FIX.A000DAE0.X0000,DISP=SHR             00052000
//E1DQANX0 DD DSN=PEB&AMB..BGFD.FIX.BG32TM03.D1%%ODATE,                 00052100
//            DISP=SHR,DCB=(BUFNO=20)                                   00052200
//*PSDAEMF2-446-INI
//*S1DQAX13 DD  SUBSYS=CDAM                                             00044900
//S1DQAX13 DD DSN=PEB&AMB..BGFD.FIX.BGLS3203.D1%%ODATE,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=3390,
//            SPACE=(CYL,(300,150),RLSE),
//            DCB=(LRECL=174,RECFM=FB,BLKSIZE=0,DSORG=PS)
//*PSDAEMF2-446-FIN
//S2DQTOTL DD DSN=PEB&AMB..BGFD.FIX.BG320003.D1%%ODATE,                 00052400
//            DISP=(NEW,CATLG,DELETE),                                  00052500
//            UNIT=3390,                                                00052600
//            SPACE=(CYL,(100,50),RLSE),                                00052700
//            DCB=(LRECL=163,RECFM=FB,BLKSIZE=0,DSORG=PS)               00052800
//S1DLINC0 DD SYSOUT=*                                                  00052900
//QRLSDB2  DD SYSOUT=*                                                  00053000
//SYSIN    DD DUMMY                                                     00053100
//SYSTSIN  DD DSN=*.BL320040.SYSTSIN,DISP=(OLD,DELETE,DELETE)           00053200
//*--------------------------------------------------------------------*00053300
//*- CREACION DE SYSTSIN PARA     PRG BG3C321W                          00053400
//*---------------------------------------------------------------------00053500
//BL320035 EXEC PGM=QU3CSTS,PARM=('&AMB,BG3C321W,&FECHA,&PR08'),        00053600
//         COND=(4,LT)                                                  00053700
//E1DQDAE0 DD DSN=PEBP.QGFD.FIX.A000DAE0.X0000,DISP=SHR                 00053800
//SYSTSIN  DD DISP=(NEW,PASS,DELETE),UNIT=3390,                         00053900
//            SPACE=(CYL,(100,50),RLSE),RECFM=FB,LRECL=100              00054000
//IGZSRTCD DD DUMMY                                                     00054100
//*                                                                     00054200
//*--------------------------------------------------------------------*00054300
//*         EJECUCION DEL PROGRAMA  BG3C321W  ANEXO-13 X CLIENTE       *00054400
//*--------------------------------------------------------------------*00054500
//BL320034 EXEC PGM=IKJEFT01,COND=(4,LT)                                00054600
//S1DQ0000 DD SYSOUT=*                                                  00054700
//SYSOUT   DD SYSOUT=*                                                  00054800
//SYSPRINT DD SYSOUT=*                                                  00054900
//SYSTSPRT DD SYSOUT=*                                                  00055000
//SYSDBOUT DD SYSOUT=Z                                                  00055100
//SYSABOUT DD SYSOUT=Z                                                  00055200
//SYSUDUMP DD SYSOUT=Z                                                  00055300
//TCDF0000 DD DSN=PEB&AMB..TCFD.VSL.TCDF0001,DISP=SHR,                  00055400
//            AMP=('BUFND=80,BUFNI=32')                                 00055500
//*IGH001 --> I                                                         00055510
//E1CORASU DD  DSN=PEBP.BGFD.FIX.SORT.BGDTTAB.D1&FECHA,DISP=SHR         00055600
//*IGH001 <-- F                                                         00055610
//E1DQDAE0 DD DSN=PEB&AMB..QGFD.FIX.A000DAE0.X0000,DISP=SHR             00055700
//E1DQANX0 DD DSN=PEB&AMB..BGFD.FIX.BG32TM08.D1%%ODATE,                 00055800
//            DISP=SHR,DCB=(BUFNO=20)                                   00055900
//*PSDAEMF2-446-INI
//*S1DQAX13 DD  SUBSYS=CDAM                                             00044900
//S1DQAX13 DD DSN=PEB&AMB..BGFD.FIX.BGLS3208.D1%%ODATE,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=3390,
//            SPACE=(CYL,(300,150),RLSE),
//            DCB=(LRECL=174,RECFM=FB,BLKSIZE=0,DSORG=PS)
//*PSDAEMF2-446-FIN
//S2DQTOTL DD DSN=PEB&AMB..BGFD.FIX.BG320008.D1%%ODATE,                 00056100
//            DISP=(NEW,CATLG,DELETE),                                  00056200
//            UNIT=3390,                                                00056300
//            SPACE=(CYL,(100,50),RLSE),                                00056400
//            DCB=(LRECL=163,RECFM=FB,BLKSIZE=0,DSORG=PS)               00056500
//S1DLINC0 DD SYSOUT=*                                                  00056600
//QRLSDB2  DD SYSOUT=*                                                  00056700
//SYSIN    DD DUMMY                                                     00056800
//SYSTSIN  DD DSN=*.BL320035.SYSTSIN,DISP=(OLD,DELETE,DELETE)           00056900
//*--------------------------------------------------------------------*00057000
//*- CREACION DE SYSTSIN PARA     PRG BG3C321W                          00057100
//*---------------------------------------------------------------------00057200
//BL320030 EXEC PGM=QU3CSTS,PARM=('&AMB,BG3C321W,&FECHA,&PR14'),        00057300
//         COND=(4,LT)                                                  00057400
//E1DQDAE0 DD DSN=PEBP.QGFD.FIX.A000DAE0.X0000,DISP=SHR                 00057500
//SYSTSIN  DD DISP=(NEW,PASS,DELETE),UNIT=3390,                         00057600
//            SPACE=(CYL,(100,50),RLSE),RECFM=FB,LRECL=100              00057700
//IGZSRTCD DD DUMMY                                                     00057800
//*                                                                     00057900
//*--------------------------------------------------------------------*00058000
//*         EJECUCION DEL PROGRAMA  BG3C321W  ANEXO-13 X CLIENTE       *00058100
//*--------------------------------------------------------------------*00058200
//BL320029 EXEC PGM=IKJEFT01,COND=(4,LT)                                00058300
//S1DQ0000 DD SYSOUT=*                                                  00058400
//SYSOUT   DD SYSOUT=*                                                  00058500
//SYSPRINT DD SYSOUT=*                                                  00058600
//SYSTSPRT DD SYSOUT=*                                                  00058700
//SYSDBOUT DD SYSOUT=Z                                                  00058800
//SYSABOUT DD SYSOUT=Z                                                  00058900
//SYSUDUMP DD SYSOUT=Z                                                  00059000
//TCDF0000 DD DSN=PEB&AMB..TCFD.VSL.TCDF0001,DISP=SHR,                  00059100
//            AMP=('BUFND=80,BUFNI=32')                                 00059200
//*IGH001 --> I                                                         00059210
//E1CORASU DD  DSN=PEBP.BGFD.FIX.SORT.BGDTTAB.D1&FECHA,DISP=SHR         00059300
//*IGH001 <-- F                                                         00059310
//E1DQDAE0 DD DSN=PEB&AMB..QGFD.FIX.A000DAE0.X0000,DISP=SHR             00059400
//E1DQANX0 DD DSN=PEB&AMB..BGFD.FIX.BG32TM14.D1%%ODATE,                 00059500
//            DISP=SHR,DCB=(BUFNO=20)                                   00059600
//*PSDAEMF2-446-INI
//*S1DQAX13 DD  SUBSYS=CDAM                                             00044900
//S1DQAX13 DD DSN=PEB&AMB..BGFD.FIX.BGLS3214.D1%%ODATE,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=3390,
//            SPACE=(CYL,(300,150),RLSE),
//            DCB=(LRECL=174,RECFM=FB,BLKSIZE=0,DSORG=PS)
//*PSDAEMF2-446-FIN
//S2DQTOTL DD DSN=PEB&AMB..BGFD.FIX.BG320014.D1%%ODATE,                 00059800
//            DISP=(NEW,CATLG,DELETE),                                  00059900
//            UNIT=3390,                                                00060000
//            SPACE=(CYL,(100,50),RLSE),                                00060100
//            DCB=(LRECL=163,RECFM=FB,BLKSIZE=0,DSORG=PS)               00060200
//S1DLINC0 DD SYSOUT=*                                                  00060300
//QRLSDB2  DD SYSOUT=*                                                  00060400
//SYSIN    DD DUMMY                                                     00060500
//SYSTSIN  DD DSN=*.BL320030.SYSTSIN,DISP=(OLD,DELETE,DELETE)           00060600
//*--------------------------------------------------------------------*00060700
//*- CREACION DE SYSTSIN PARA     PRG BG3C321W                          00060800
//*---------------------------------------------------------------------00060900
//BL320025 EXEC PGM=QU3CSTS,PARM=('&AMB,BG3C321W,&FECHA,&PR18'),        00061000
//         COND=(4,LT)                                                  00061100
//E1DQDAE0 DD DSN=PEBP.QGFD.FIX.A000DAE0.X0000,DISP=SHR                 00061200
//SYSTSIN  DD DISP=(NEW,PASS,DELETE),UNIT=3390,                         00061300
//            SPACE=(CYL,(100,50),RLSE),RECFM=FB,LRECL=100              00061400
//IGZSRTCD DD DUMMY                                                     00061500
//*                                                                     00061600
//*--------------------------------------------------------------------*00061710
//*         EJECUCION DEL PROGRAMA  BG3C321W  ANEXO-13 X CLIENTE       *00061800
//*--------------------------------------------------------------------*00061900
//BL320024 EXEC PGM=IKJEFT01,COND=(4,LT)                                00062000
//S1DQ0000 DD SYSOUT=*                                                  00062100
//SYSOUT   DD SYSOUT=*                                                  00062200
//SYSPRINT DD SYSOUT=*                                                  00062300
//SYSTSPRT DD SYSOUT=*                                                  00062400
//SYSDBOUT DD SYSOUT=Z                                                  00062500
//SYSABOUT DD SYSOUT=Z                                                  00062600
//SYSUDUMP DD SYSOUT=Z                                                  00062700
//TCDF0000 DD DSN=PEB&AMB..TCFD.VSL.TCDF0001,DISP=SHR,                  00062800
//            AMP=('BUFND=80,BUFNI=32')                                 00062900
//*IGH001 --> I                                                         00062910
//E1CORASU DD  DSN=PEBP.BGFD.FIX.SORT.BGDTTAB.D1&FECHA,DISP=SHR         00063000
//*IGH001 <-- F                                                         00063010
//E1DQDAE0 DD DSN=PEB&AMB..QGFD.FIX.A000DAE0.X0000,DISP=SHR             00063100
//E1DQANX0 DD DSN=PEB&AMB..BGFD.FIX.BG32TM18.D1%%ODATE,                 00063200
//            DISP=SHR,DCB=(BUFNO=20)                                   00063300
//*PSDAEMF2-446-INI
//*S1DQAX13 DD  SUBSYS=CDAM                                             00044900
//S1DQAX13 DD DSN=PEB&AMB..BGFD.FIX.BGLS3218.D1%%ODATE,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=3390,
//            SPACE=(CYL,(300,150),RLSE),
//            DCB=(LRECL=174,RECFM=FB,BLKSIZE=0,DSORG=PS)
//*PSDAEMF2-446-FIN
//S2DQTOTL DD DSN=PEB&AMB..BGFD.FIX.BG320018.D1%%ODATE,                 00063500
//            DISP=(NEW,CATLG,DELETE),                                  00063600
//            UNIT=3390,                                                00063700
//            SPACE=(CYL,(100,50),RLSE),                                00063800
//            DCB=(LRECL=163,RECFM=FB,BLKSIZE=0,DSORG=PS)               00063900
//S1DLINC0 DD SYSOUT=*                                                  00064000
//QRLSDB2  DD SYSOUT=*                                                  00064100
//SYSIN    DD DUMMY                                                     00064200
//SYSTSIN  DD DSN=*.BL320025.SYSTSIN,DISP=(OLD,DELETE,DELETE)           00064300
//*--------------------------------------------------------------------*00064400
//*- CREACION DE SYSTSIN PARA     PRG BG3C321W                          00064500
//*---------------------------------------------------------------------00064600
//BL320020 EXEC PGM=QU3CSTS,PARM=('&AMB,BG3C321W,&FECHA,&PR07'),        00064700
//         COND=(4,LT)                                                  00064800
//E1DQDAE0 DD DSN=PEBP.QGFD.FIX.A000DAE0.X0000,DISP=SHR                 00064900
//SYSTSIN  DD DISP=(NEW,PASS,DELETE),UNIT=3390,                         00065000
//            SPACE=(CYL,(100,50),RLSE),RECFM=FB,LRECL=100              00065100
//IGZSRTCD DD DUMMY                                                     00065200
//*                                                                     00065300
//*--------------------------------------------------------------------*00065400
//*         EJECUCION DEL PROGRAMA  BG3C321W  ANEXO-13 X CLIENTE       *00065500
//*--------------------------------------------------------------------*00065600
//BL320019 EXEC PGM=IKJEFT01,COND=(4,LT)                                00065700
//S1DQ0000 DD SYSOUT=*                                                  00065800
//SYSOUT   DD SYSOUT=*                                                  00065900
//SYSPRINT DD SYSOUT=*                                                  00066000
//SYSTSPRT DD SYSOUT=*                                                  00066100
//SYSDBOUT DD SYSOUT=Z                                                  00066200
//SYSABOUT DD SYSOUT=Z                                                  00066300
//SYSUDUMP DD SYSOUT=Z                                                  00066400
//TCDF0000 DD DSN=PEB&AMB..TCFD.VSL.TCDF0001,DISP=SHR,                  00066500
//            AMP=('BUFND=80,BUFNI=32')                                 00066600
//*IGH001 --> I                                                         00066610
//E1CORASU DD  DSN=PEBP.BGFD.FIX.SORT.BGDTTAB.D1&FECHA,DISP=SHR         00066700
//*IGH001 <-- F                                                         00066710
//E1DQDAE0 DD DSN=PEB&AMB..QGFD.FIX.A000DAE0.X0000,DISP=SHR             00066800
//E1DQANX0 DD DSN=PEB&AMB..BGFD.FIX.BG32TM07.D1%%ODATE,                 00066900
//            DISP=SHR,DCB=(BUFNO=20)                                   00067000
//*PSDAEMF2-446-INI
//*S1DQAX13 DD  SUBSYS=CDAM                                             00044900
//S1DQAX13 DD DSN=PEB&AMB..BGFD.FIX.BGLS3207.D1%%ODATE,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=3390,
//            SPACE=(CYL,(300,150),RLSE),
//            DCB=(LRECL=174,RECFM=FB,BLKSIZE=0,DSORG=PS)
//*PSDAEMF2-446-FIN
//S2DQTOTL DD DSN=PEB&AMB..BGFD.FIX.BG320007.D1%%ODATE,                 00067200
//            DISP=(NEW,CATLG,DELETE),                                  00067300
//            UNIT=3390,                                                00067400
//            SPACE=(CYL,(100,50),RLSE),                                00067500
//            DCB=(LRECL=163,RECFM=FB,BLKSIZE=0,DSORG=PS)               00067600
//S1DLINC0 DD SYSOUT=*                                                  00067700
//QRLSDB2  DD SYSOUT=*                                                  00067800
//SYSIN    DD DUMMY                                                     00067900
//SYSTSIN  DD DSN=*.BL320020.SYSTSIN,DISP=(OLD,DELETE,DELETE)           00068000
//*--------------------------------------------------------------------*
//*- CREACION DE SYSTSIN PARA BG3C321W - RETENCIONES JUDICIALES       *
//*--------------------------------------------------------------------*
//BL32RJ01 EXEC PGM=QU3CSTS,PARM=('&AMB,BG3C321W,&FECHA,&PRRJ'),
//         COND=(4,LT)
//E1DQDAE0 DD DSN=PEBT.QGFD.FIX.A000DAE0.X0000,DISP=SHR
//SYSTSIN  DD DISP=(NEW,PASS,DELETE),UNIT=3390,
//            SPACE=(CYL,(100,50),RLSE),RECFM=FB,LRECL=100
//IGZSRTCD DD DUMMY
//*--------------------------------------------------------------------*
//*         EJECUCION DEL PROGRAMA BG3C321W - RETENCIONES JUDICIALES   *
//*--------------------------------------------------------------------*
//BL32RJ02 EXEC PGM=IKJEFT01,COND=(4,LT)
//S1DQ0000 DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//SYSDBOUT DD SYSOUT=Z
//SYSABOUT DD SYSOUT=Z
//SYSUDUMP DD SYSOUT=Z
//TCDF0000 DD DSN=PEBP.TCFD.VSL.TCDF0001,DISP=SHR,
//            AMP=('BUFND=80,BUFNI=32')
//E1CORASU DD DSN=PEBP.BGFD.FIX.SORT.BGDTTAB.D1&FECHA,DISP=SHR
//E1DQDAE0 DD DSN=PEBP.QGFD.FIX.A000DAE0.X0000,DISP=SHR
//E1DQANX0 DD DSN=PEBP.BGFD.FIX.ANEXO13.RJEDATA.D1%%ODATE, // BGJPF260
//            DISP=SHR,DCB=(BUFNO=20)
//S1DQAX13 DD DSN=PEBP.BGFD.FIX.BGLS32RJ.D1%%ODATE,        // REP FINAL
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=3390,
//            SPACE=(CYL,(300,150),RLSE),
//            DCB=(LRECL=174,RECFM=FB,BLKSIZE=0,DSORG=PS)
//S2DQTOTL DD DSN=PEBP.BGFD.FIX.BG32RJ00.D1%%ODATE,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=3390,
//            SPACE=(CYL,(100,50),RLSE),
//            DCB=(LRECL=163,RECFM=FB,BLKSIZE=0,DSORG=PS)
//S1DLINC0 DD SYSOUT=*
//QRLSDB2  DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSTSIN  DD DSN=*.BL32RJ01.SYSTSIN,DISP=(OLD,DELETE,DELETE)

//*--------------------------------------------------------------------*00068100
