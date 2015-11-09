$TITLE CGE1
$OFFUPPER

*INTRODUCTION====================================================
$ONTEXT

In this file, CGE1 is implemented in GAMS.

$OFFTEXT

*SETS============================================================

SETS

 AC global set (SAM accounts and other items)
   /AGR-A   agricultural activity
    NAGR-A  non-agricultural activity
    AGR-C   agricultural commodity
    NAGR-C  non-agricultural commodity
    LAB     labor
    CAP     capital
    U-HHD   urban household
    R-HHD   rural household
    TOTAL   total account in SAM   /


 ACNT(AC) all elements in AC except total

*SMS: A is a subset of AC
 A(AC)  activities
        /AGR-A, NAGR-A/
*SMS: C is a subset of AC
 C(AC)  commodities
        /AGR-C, NAGR-C/
*SMS: F is a subset of AC
 F(AC)  factors
        /LAB, CAP/
*SMS: H is a subset of AC
 H(AC)  households
        /U-HHD, R-HHD/
 ;
*SMS:  Alias are used frequently when you are dealing with summations
 ALIAS(AC,ACP); ALIAS(C,CP); ALIAS(F,FP);
 ACNT(AC) = YES; ACNT('TOTAL') = NO; ALIAS(ACNT,ACNTP);
*SMS: ACNT has all members of AC,  but ACNT does not have total.
*SMS: you must assign all variables, before removing specific elements
*SMS: This is a way to delete certain elements of set.You must assign all
*SMS: all variables before you start removing them.  By coding ACNT(AC)=YES
*SMS: you have assigned all of AC to ACNT.  ACNT('TOTAL')=NO removes total from
*SMS: the ACNT set

*PARAMETERS======================================================

PARAMETERS

 ad(A)      efficiency parameter in the production fn for a
 alpha(F,A) share of value-added to factor f in activity a
 beta(C,H)  share of household consumption spending on commodity c
 cpi        consumer price index
 cwts(C)    weight of commodity c in the CPI
 qfs(F)     supply of factor f
 shry(H,F)  share for household h in the income of factor f
 theta(A,C) yield of output c per unit of activity a
;


*VARIABLES=======================================================

VARIABLES

 P(C)      price of commodity c
 PA(A)     price of activity a
 Q(C)      output level for commodity c
 QA(A)     level of activity a
 QF(F,A)   quantity demanded of factor f from activity a
 QH(C,H)   quantity consumed of commodity c by household h
 WF(F)     price of factor f
 YF(H,F)   income of household h from factor f
 YH(H)     income of household h
 ;

*EQUATIONS=======================================================

EQUATIONS

*PRODUCTION AND COMMODITY BLOCK++++++++
 PRODFN(A)      Cobb-Douglas production function for activity a
*SMS: the A in PRODFN(A) means there are two equations represented here b/c
*SMS: there are two elements represented by A
 FACDEM(F,A)    demand for factor f from activity a
*SMS: the F,A in FACDEM(F,A) means there is a matrix of 2x2 equations (i.e. 4)
*SMS: represented here b/c there are four elements represented by F,A
 OUTPUTFN(C)    output of commodity c
 PADEF(A)       price for activity a

*INSTITUTION BLOCK+++++++++++++++++++++
 FACTTRNS(H,F)  transfer of income from factor f to h-hold h
 HHDINC(H)      income of household h
 HHDEM(C,H)     consumption demand for household h & commodity c

*SYSTEM CONSTRAINT BLOCK+++++++++++++++
 FACTEQ(F)      market equilibrium condition for factor f
 COMEQ(C)       market equilibrium condition for commodity c
 PNORM          price normalization
 ;

*PRODUCTION AND COMMODITY BLOCK++++++++

 PRODFN(A)..    QA(A) =E= ad(A)*PROD(F, QF(F,A)**alpha(F,A));

 FACDEM(F,A)..  WF(F) =E= alpha(F,A)*PA(A)*QA(A) / QF(F,A);

 OUTPUTFN(C)..  Q(C) =E= SUM(A, theta(A,C)*QA(A));

 PADEF(A)..     PA(A) =E= SUM(C, theta(A,C)*P(C));


*INSTITUTION BLOCK+++++++++++++++++++++

 FACTTRNS(H,F)..  YF(H,F) =E= shry(H,F)*WF(F)*SUM(A, QF(F,A));

 HHDINC(H)..      YH(H) =E= SUM(F, YF(H,F));

 HHDEM(C,H)..     QH(C,H) =E= beta(C,H)*YH(H)/P(C);


*SYSTEM CONSTRAINT BLOCK+++++++++++++++

 FACTEQ(F)..       SUM(A, QF(F,A)) =E= qfs(F);

 COMEQ('AGR-C')..  Q('AGR-C') =E= SUM(H, QH('AGR-C',H));

 PNORM..           SUM(C, cwts(C)*P(C)) =E= cpi;
*SMS: You only nned tpdrop one of your equations - not from production function
*SMS: in order to put this model into equilibrium (i.e. same number of equations
*SMS: as there are variables.  Walrus' allows eqns to be dropped assuming it will
*SMS: naturally clear.

*MODEL===========================================================

MODEL
 CGE1  Simple CGE model  /ALL/
 ;

*SOCIAL ACCOUNTING MATRIX========================================

*SMS: In a SAM, columns are paying (payments/expenditures) and rows are receiving (receipts)
TABLE SAM(AC,ACP)  social accounting matrix

         AGR-A  NAGR-A  AGR-C  NAGR-C  LAB  CAP  U-HHD  R-HHD
AGR-A                     125
NAGR-A                            150
AGR-C                                               50     75
NAGR-C                                             100     50
LAB         62      55
CAP         63      95
U-HHD                                   60   90
R-HHD                                   57   68
 ;
*SMS: AGR-A must pay 62 to LAB and 63 to CAP
*SMS: U-HHD is receiving 60 from LAB and R-HHD is receiving 57
*SMS: AGR-A and NAGR-A payments to LAB must equal LAB payments to U-HHD & R-HHD

PARAMETER
 tdiff(AC)  column minus row total for account ac;
*This parameter is used to check that the above SAM is balanced.
          SAM('TOTAL',ACNTP) = SUM(ACNT, SAM(ACNT,ACNTP));
*SMS: You are summing the rows here to get the total
          SAM(ACNT,'TOTAL')  = SUM(ACNTP, SAM(ACNT,ACNTP));
*SMS: You are summing the columns here to get the total
          tdiff(ACNT)        = SAM('TOTAL',ACNT) - SAM(ACNT,'TOTAL');
*SMS: you are checking whether the column sum equals the row sum: is it balanced
DISPLAY SAM, tdiff;


*ASSIGNMENTS FOR PARAMETERS AND VARIABLES========================

PARAMETERS
*The following parameters are used to define initial values of
*model variables.
 P0(C), PA0(A), Q0(C), QA0(A), QF0(F,A), QH0(C,H), WF0(F), YF0(H,F),
 YH0(H)
 ;


*PRODUCTION AND COMMODITY BLOCK++++++++

 P0(C)      = 1;
 PA0(A)     = 1;
 WF0(F)     = 1;

 Q0(C)      = SAM('TOTAL',C)/P0(C);
 QA0(A)     = SAM('TOTAL',A)/PA0(A);
 QF0(F,A)   = SAM(F,A)/WF0(F);

 alpha(F,A) = SAM(F,A) / SUM(FP, SAM(FP,A));
 ad(A)      = QA0(A) / PROD(F, QF0(F,A)**alpha(F,A));
 theta(A,C) = (SAM(A,C)/P0(C)) / QA0(A);


*INSTITUTION BLOCK+++++++++++++++++++++

 QH0(C,H)  = SAM(C,H)/P0(C);
 YF0(H,F)  = SAM(H,F);
 YH0(H)    = SAM('TOTAL',H);

 beta(C,H) = SAM(C,H)/SUM(CP, SAM(CP,H));
 shry(H,F) = SAM(H,F)/SAM('TOTAL',F);


*SYSTEM CONSTRAINT BLOCK+++++++++++++++

 cwts(C)  = SUM(H, SAM(C,H)) / SUM((CP,H), SAM(CP,H));
 cpi      = SUM(C, cwts(C)*P0(C));
 qfs(F) = SAM(F,'TOTAL')/WF0(F);


*INITIALIZING ALL VARIABLES++++++++++++

 P.L(C)    = P0(C);
 PA.L(A)   = PA0(A);
 Q.L(C)    = Q0(C);
 QA.L(A)   = QA0(A);
 QF.L(F,A) = QF0(F,A);
 QH.L(C,H) = QH0(C,H);
 YF.L(H,F) = YF0(H,F);
 WF.L(F)   = WF0(F);
 YH.L(H)   = YH0(H);


*DISPLAY+++++++++++++++++++++++++++++++

*DISPLAY
* ad, alpha, beta,  cpi, cwts, qfs, shry, theta,

*P.L, PA.L, Q.L, QA.L, QF.L, QH.L, WF.L, YF.L, YH.L
*;


*SOLVE STATEMENT FOR BASE========================================

*SOLVE CGE1 USING MCP;

*DISPLAY
*P.L, PA.L, Q.L, QA.L, QF.L, QH.L, WF.L, YF.L, YH.L
*;

*Simulated with a 1.1 increase in CPI
*CPI=CPI*2;

*SOLVE CGE1 USING MCP;

*DISPLAY
* P.L, PA.L, Q.L, QA.L, QF.L, QH.L, WF.L, YF.L, YH.L
* ;


*CAPITAL STOCK INCREASE EXAMPLE==================================

Set
    SIM simulations
    /Base base simulation
     CINCR increase in capital stock/

*New parameters to accomodate shock===============================
Parameters

 QFSCAPSIM(SIM) capital supply for simulation sim (experiment parameter)
 QFS(F)     supply of factor f that is being shocked
*QFS('CAP')
*SMS: Should the above be QFS('CAP')?????
;

*Report Parameters===============================================

Parameters

QFSREP(F,SIM)    supply of factor f for simulation sim (value used)
PREP(C,SIM)      demander price for commodity c
PAREP (A,SIM)    price of activity a
QREP(C,SIM)      output level for comoodity c
QAREP(A,SIM)     level of activity a
QFREP(F,A,SIM)   demand for factor f from activity a
QHREP(C,H,SIM)   consumption of commodity c by household h
WFREP(F,SIM)     price of factor f
YFREP(H,F,SIM)   income of house h from factor f
YHREP(H,SIM)     income of household h
SAMREP(SIM,AC,ACP)       SAM computed from model solution
BALCHK(AC,SIM)   column minus row total for account ac in SAM
;


QFSCAPSIM('BASE')=QFS('CAP');
QFSCAPSIM('CINCR')=1.1*QFS('CAP');

LOOP(SIM,
*shocks you want to undertake
*ex: param1=param1sim(sim);

QFS('CAP') = QFSCAPSIM(SIM);

SOLVE CGE1 USING MCP;

      QFSREP(F,SIM) = qfs(F);
      PREP(C,SIM) = P.L(C);
      PAREP(A,SIM) = PA.L(A);
      QREP(C,SIM) = Q.L(C);
      QAREP(A,SIM) = QA.L(A);
      QFREP(F,A,SIM) = QF.L(F,A);
      QHREP(C,H,SIM) = QH.L(C,H);
      WFREP(F,SIM) = WF.L(F);
      YFREP(H,F,SIM) = YF.L(H,F);
      YHREP(H,SIM) = YH.L(H);

*Payments from activities
SAMREP(SIM,F,A) = WF.L(F)*QF.L(F,A);
*Payments from commodities
SAMREP(SIM,A,C) = P.L(C)*theta(A,C)*QA.L(A);
*Payments from factors
SAMREP(SIM,H,F) = YF.L(H,F);
*Payments from households
SAMREP(SIM,C,H) = P.L(C)*QH.L(C,H);

*Pick up the results in your report parameters.

);

*Parameters that report percentage changes
Parameters
QFSREPP(F,SIM)    supply of factor f for simulation sim (%change)
PREPP(C,SIM)      demander price for commodity c (%ch)
PAREPP (A,SIM)    price of activity a (%ch)
QREPP(C,SIM)      output level for commodity c (%ch)
QAREPP(A,SIM)     level of activity a (%ch)
QFREPP(F,A,SIM)   demand for factor f from activity (%ch)
QHREPP(C,H,SIM)   consumption of commodity c by household h (%ch)
YFREPP(H,F,SIM)   income of household h from factor f (%ch)
WFREPP(F,SIM)     price of factor f (%ch)
YHREPP(H,SIM)   income of household h (%ch)
SAMREPP(SIM,AC,ACP) SAM computed from model solution (%ch by cell)
;

QFSREPP(F,SIM)   =100*(QFSREP(F,SIM)/QFSREP(F,'BASE')-1);
PREPP(C,SIM)     =100*(PREP(C,SIM)/PREP(C,'BASE')-1);
PAREPP(A,SIM)    =100*(PAREP(A,SIM)/PAREP(A,'BASE')-1);
QREPP(C,SIM)     =100*(QREP(C,SIM)/QREP(C,'BASE')-1);
QAREPP(A,SIM)    =100*(QAREP(A,SIM)/QAREP(A,'BASE')-1);
QFREPP(F,A,SIM)  =100*(QFREP(F,A,SIM)/QFREP(F,A,'BASE')-1);
QHREPP(C,H,SIM)  =100*(QHREP(C,H,SIM)/QHREP(C,H,'BASE')-1);
WFREPP(F,SIM)    =100*(WFREP(F,SIM)/WFREP(F,'BASE')-1);
YFREPP(H,F,SIM)  =100*(YFREP(H,F,SIM)/YHREP(H,'BASE')-1);
YHREPP(H,SIM)    =100*(YHREP(H,SIM)/YHREP(H,'BASE')-1);
SAMREPP(SIM,AC,ACP)$SAMREP('BASE',AC,ACP)
                 =100*(SAMREP(SIM,AC,ACP)/SAMREP('BASE',AC,ACP)-1);


