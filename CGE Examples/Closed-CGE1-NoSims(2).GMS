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

 A(AC)  activities
        /AGR-A, NAGR-A/

 C(AC)  commodities
        /AGR-C, NAGR-C/

 F(AC)  factors
        /LAB, CAP/

 H(AC)  households
        /U-HHD, R-HHD/
 ;

 ALIAS(AC,ACP); ALIAS(C,CP); ALIAS(F,FP);
 ACNT(AC) = YES; ACNT('TOTAL') = NO; ALIAS(ACNT,ACNTP);


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
 FACDEM(F,A)    demand for factor f from activity a
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


*MODEL===========================================================

MODEL
 CGE1  Simple CGE model  /ALL/
 ;

*SOCIAL ACCOUNTING MATRIX========================================

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


PARAMETER
 tdiff(AC)  column minus row total for account ac;
*This parameter is used to check that the above SAM is balanced.
          SAM('TOTAL',ACNTP) = SUM(ACNT, SAM(ACNT,ACNTP));
          SAM(ACNT,'TOTAL')  = SUM(ACNTP, SAM(ACNT,ACNTP));
          tdiff(ACNT)        = SAM('TOTAL',ACNT) - SAM(ACNT,'TOTAL');

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

DISPLAY
 ad, alpha, beta,  cpi, cwts, qfs, shry, theta,

 P.L, PA.L, Q.L, QA.L, QF.L, QH.L, WF.L, YF.L, YH.L
 ;


*SOLVE STATEMENT FOR BASE========================================

SOLVE CGE1 USING MCP;

DISPLAY
 P.L, PA.L, Q.L, QA.L, QF.L, QH.L, WF.L, YF.L, YH.L
 ;

