$Title TSO-DSO_Bilevel

$ontext
This model was developed by Leandro Lind.
Model presented in the paper titled "TSO-DSO
Interface Flow Pricing: A bilevel study on efficiency
and cost allocation" and L.Lind's PhD Thesis.

Summarised results are captured
and displayed by parameter "report".

Software distributed under MIT License:

Copyright (c) 2024 Leandro Lind

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
$offtext

$onEps
$onImplicitAssign

***********************************************

* CHANGE DATASET HERE (comment unused datasets):

*$set data_set "Data_Stylised"
*$set data_set "Data_102_NoTSOCong"
$set data_set "Data_102_WithTSOCong"

***********************************************

option OptcR = 0, Limrow=50;

OPTION MIP = CPLEX;

SETS
i                   Network nodes
ii(i,i)             Nodes connected by a line
s                   System Operator
dso(s)              DSOs
tso(s)              TSOs
is(s,i)             Relation SO and Node
global_slack(i)     Global slack
dso_slack(s,i)      DSO Slack
subs(i)             Substations
idso(i)             Nodes that belong to DSO(s)
itso(i)             Nodes that belong to TSO(s)
k                   FSPs
it(i,k)             FSP conected to node i
inter(s,i,s)        Set that identifies the interfaces between TSO substation and DSO
ks(k,s)             Set linking FSP k that is connected to DSOs
kdso(k)             FSPs that are connected to DSOs
ktso(k)             FSPs that are connected to TSOs
cl                  Sets for Table on Lines /r, x, fmax/
ck                  Sets for Table on FSPs  /pmin,pmax,bid_dw,bid_up/
ci                  Sets for Table on Nodes /d,dq,DA_Dispatch,imb/
;

Alias(i,j,jj);
Alias(s,ss);

Scalars
SB                  Base Power [MVA]
theta_max           Max phase angle [RAD]
PF                  Power factor [pu]
v_max               Maximum volatage [pu]
v_min               Minimum voltage [pu]
PF_fsp              Typical Power Factor assumed for an FSP [pu]
PFACTOR             Typical Power Factor for the calculation of reactive power flow [pu]
Cap_Inter           Cap price on the interface flow (no regulatory mechanism)
;

Parameters
d(i)
DA_Dispatch(i)
imb(i)
react(i,j)
resis(i,j)
bid_dw(k)      
bid_up(k)
pmax(k)   
pmin(k)   
fmax(i,j) 
fmin(i,j)
subs_pf
DA_DSO
dispatchfsp(k)
transfer_DSO_TSO
ds(i)
dq(i)
DA_DSO_Q(s)
dtl(i,j,cl)
dtk(k,ck)
dti(i,ci)
cost_rebalance(dso)   For fixing the interface price. Not implemented in this version.
report(*,*)       Summirised results. Check last display on the .lst file.
;

Variables
fobj_b_ref
p(k)                
f(i,j)
theta(i)
virtualdemand(s)
p_pos(k)
p_neg(k)
;

Positive Variable
intPrice_var(dso)
;

* ---------------------------- *
*                              *
* START DATA IMPORT FROM EXCEL *
* Please keep the following    *
* folder structure:            *
*                              * 
* ~\TSO-DSO_Bilevel.gms      *
* ~\Input_Data\Data_XXX.xlsx   *
*                              *
* ---------------------------- *


$onecho > tasks.txt
set=s rng=s rdim=1 cdim=0
set=i rng=i rdim=1 cdim=0
set=dso rng=dso rdim=1 cdim=0
set=tso rng=tso rdim=1 cdim=0
set=is rng=is rdim=2 cdim=0
set=global_slack rng=global_slack rdim=1 cdim=0
set=dso_slack rng=dso_slack rdim=2 cdim=0
set=subs rng=subs rdim=1 cdim=0
set=idso rng=idso rdim=1 cdim=0
set=itso rng=itso rdim=1 cdim=0
set=inter rng=inter rdim=3 cdim=0
set=k rng=k rdim=1 cdim=0
set=it rng=it rdim=2 cdim=0
set=ks rng=ks rdim=2 cdim=0
set=kdso rng=kdso rdim=1 cdim=0
set=ktso rng=ktso rdim=1 cdim=0

par=dtl rng=dtl rdim=2 cdim=1
par=dtk rng=dtk rdim=1 cdim=1
par=dti rng=dti rdim=1 cdim=1

par=SB         rng=SB         dim=0
par=theta_max  rng=theta_max  dim=0
par=PF         rng=PF         dim=0
par=v_max      rng=v_max      dim=0
par=v_min      rng=v_min      dim=0
par=PF_fsp     rng=PF_fsp     dim=0
par=PFACTOR    rng=PFACTOR    dim=0
par=Cap_Inter  rng=Cap_Inter  dim=0

par=cost_rebalance rng=Cost_rebal rdim=1 cdim=0

$offecho

$call GDXXRW Input_Data\%data_set%.xlsx trace=3 @tasks.txt
$GDXIN %data_set%.gdx

$LOAD s,i,dso,tso,is,global_slack,dso_slack,subs,idso,itso,inter,k,it,ks,kdso,ktso,SB,theta_max,PF,v_max,v_min,PF_fsp,PFACTOR,Cap_Inter
$LOAD dtl,dtk,dti
$LOAD cost_rebalance

$GDXIN
* -------------------------- *
* END DATA IMPORT FROM EXCEL *
* -------------------------- *


Set
theta_dso(i)        /set.idso, set.subs/
theta_tso(i)        /set.itso, set.subs/
;

react(i,j)      = dtl(i,j,'x');
resis(i,j)      = dtl(i,j,'r');
fmax(i,j)       = dtl(i,j,'fmax');
fmin(i,j)       = -dtl(i,j,'fmax');

pmin(k)         = dtk(k,'pmin');
pmax(k)         = dtk(k,'pmax');
bid_dw(k)          = dtk(k,'bid_dw');
bid_up(k)          = dtk(k,'bid_up');

d(i)            = dti(i,'d');
dq(i)           = dti(i,'dq');
DA_Dispatch(i)  = dti(i,'DA_Dispatch');
imb(i)          = dti(i,'imb');

ii(i,j) $ fmax(i,j)= YES;

DA_DSO_Q(s) = SUM(i $ is(s,i), dq(i))

Parameters
qmax(k)
qmin(k)
fqmax(i,j)
fqmin(i,j)
;

qmax(k) = sqrt(sqr(pmax(k) / PF_fsp) - sqr(pmax(k)));
qmin(k) = -sqrt(sqr(pmin(k) / PF_fsp) - sqr(pmin(k)));

fqmax(i,j) = fmax(i,j);
fqmin(i,j) = - fqmax(i,j);

DA_DSO(s) =  + SUM[i $ is(s,i), d(i)];

*-------------------------------------------------------------------------------
*|                                                                             |
*|                                 BILEVEL                                     |
*|                                                                             |
*-------------------------------------------------------------------------------

Positive variables
MuUPPlus(k)
MuUPZero(k)
MuDWPlus(k)
MuDWZero(k)
MuFpPlus(i,j)
MuFpMinus(i,j)
MuFqPlus(i,j)
MuFqMinus(i,j)
MuVPlus(i)
MuVMinus(i)
MuThetaPlus(i)
MuThetaMinus(i)
;

Scalars
M01P     /10000000/
M02P     /10000000/
M03P     /10000000/
M04P     /10000000/
M05P     /10000000/
M06P     /10000000/
M01D     /100000000/
M02D     /100000000/
M03D     /100000000/
M04D     /100000000/
M05D     /100000000/
M06D     /100000000/  
M07P     /10000000/
M08P     /10000000/
M09P     /10000000/
M10P     /10000000/
M07D     /100000000/
M08D     /100000000/
M09D     /100000000/
M10D     /100000000/  
M11P     /10000000/
M12P     /10000000/
M11D     /100000000/
M12D     /100000000/
;

Variable
ObjValueUpper
LambT1(i,j,tso,dso)
LambT2(dso)
LambT3(i,tso)
LambT4(i,j,tso)
LambT5(i)
Lamb1(i,dso)
Lamb2(i,j,dso)
Lamb3(dso,i)
Lamb4(i,dso)
Lamb5(i,j,dso)
Lamb6(i,j,dso)
Lamb7(kdso,dso)
Lamb8(i,j,dso)
Lamb9(dso,i)
q(k)
fq(j,i)
w(i)
slack_q(i,subs)
slack_f(s)
;


Binary Variables
b01(k)
b02(k)
b03(k)
b04(k)
b05(i,j)
b06(i,j)
b07(i,j)
b08(i,j)
b09(i)
b10(i)
b11(i)
b12(i)
;

Equations
* Upper level constraints
ObjFuncUpper
Cap_price

* DSO eq. + lag. constraints
E_DMND_B_D
E_SUBS_PF
E_DSO_SLACK
E_DMND_B_D_Q
E_SUBS_PF_Q
E_DISTFLOW
E_PF
Lag_pkup_BL
Lag_pkdw_BL
Lag_qk
Lag_fpij
Lag_fqij
Lag_wi
Lag_slackQij

*TSO eq. + lag. constraints
E_DMND_B_T
E_FLUX_B_T
E_CS3T_2
E_SUBS_BALANCE_CS2
E_SLACK_G
Lag_pkup_T
Lag_pkdw_T
Lag_fij_T
Lag_Thetai_T
Lag_virts_T

*Complementarity Cond.
UPPlus_U_CC1
UPPlus_U_CC2
UPPlus_U_CC3
UPPlus_U_CC4
UPZero_U_CC1
UPZero_U_CC2
UPZero_U_CC3
UPZero_U_CC4
DWPlus_U_CC1
DWPlus_U_CC2
DWPlus_U_CC3
DWPlus_U_CC4
DWZero_U_CC1
DWZero_U_CC2
DWZero_U_CC3
DWZero_U_CC4
FpPlus_U_CC1
FpPlus_U_CC2
FpPlus_U_CC3
FpPlus_U_CC4
FpMinus_U_CC1
FpMinus_U_CC2
FpMinus_U_CC3
FpMinus_U_CC4
FqPlus_CC1
FqPlus_CC2
FqPlus_CC3
FqPlus_CC4
FqMinus_CC1
FqMinus_CC2
FqMinus_CC3
FqMinus_CC4
VPlus_CC1
VPlus_CC2
VPlus_CC3
VPlus_CC4
VMinus_CC1
VMinus_CC2
VMinus_CC3
VMinus_CC4
ThetaiPlus_T_CC1
ThetaiPlus_T_CC2
ThetaiPlus_T_CC3
ThetaiPlus_T_CC4
ThetaiMinus_T_CC1
ThetaiMinus_T_CC2
ThetaiMinus_T_CC3
ThetaiMinus_T_CC4
;

ObjFuncUpper..
    ObjValueUpper
    =E=
    + SUM[(tso,ktso) $ ks(ktso,tso), (bid_up(ktso) * p_pos(ktso)) + (bid_dw(ktso) * p_neg(ktso))]
    - [
       - SUM[(i,dso,k) $ (it(i,k) and is(dso,i)), (bid_up(k) * p_pos(k)) + (bid_dw(k) * p_neg(k))]
       + SUM[(dso,idso) $ is(dso,idso), DA_Dispatch(idso) * Lamb1(idso,dso)]
       - SUM[(dso,idso) $ is(dso,idso), d(idso) * Lamb1(idso,dso)]
       + SUM[(idso,j,dso) $ (ii(idso,j) and subs(j) and is(dso,idso)), DA_DSO(dso)*Lamb2(idso,j,dso)]
       - SUM[(dso,theta_dso) $ dso_slack(dso,theta_dso),Lamb3(dso,theta_dso)]
       - SUM[(dso,idso) $ is(dso,idso),Lamb4(idso,dso)*dq(idso)]
       - SUM[(idso,j,dso) $ (ii(idso,j) and subs(j) and is(dso,idso)), DA_DSO_Q(dso)*Lamb5(idso,j,dso)]
       - SUM[kdso, pmax(kdso)*MuUPPlus(kdso)]
       - SUM[kdso, pmin(kdso)*MuDWPlus(kdso)]
       - SUM[(idso,j) $ ii(idso,j), fmax(idso,j)*MuFpPlus(idso,j)]
       + SUM[(idso,j) $ ii(idso,j), fmin(idso,j)*MuFpMinus(idso,j)]
       - SUM[(idso,j) $ ii(idso,j), fqmax(idso,j)*MuFqPlus(idso,j)]
       + SUM[(idso,j) $ ii(idso,j), fqmin(idso,j)*MuFqMinus(idso,j)]
       - SUM[theta_dso, sqr(v_max) * MuVPlus(theta_dso)]
       + SUM[theta_dso, sqr(v_min) * MuVMinus(theta_dso)]
      ]
    ;

Cap_price(dso)..
    intPrice_var(dso) =L= Cap_Inter;

E_DMND_B_T(i,tso)  $ is(tso,i) ..
    + DA_Dispatch(i)
    + SUM[k $ it(i,k), p_pos(k)]
    - SUM[k $ it(i,k), p_neg(k)]
    - SUM[j $ ii(i,j), f(i,j)]
    + SUM[j $ ii(j,i), f(j,i)]
    =E=
    + d(i)
    ;

E_DMND_B_D(i,dso)  $ is(dso,i) ..
    + DA_Dispatch(i)
    + SUM[k $ it(i,k), p_pos(k)]
    - SUM[k $ it(i,k), p_neg(k)]
    - SUM[j $ ii(i,j), f(i,j)]
    + SUM[j $ ii(j,i), f(j,i)]
    - d(i)
    =E=
    0
    ;

E_SUBS_PF(i,subs,dso) $ (ii(i,subs) and is(dso,i))..
    f(i,subs) + DA_DSO(dso) - SUM[k $ ks(k,dso), (p_pos(k) - p_neg(k))] =E= 0
    ;
    
E_FLUX_B_T(i,j,tso)  $ [ii(i,j) and is(tso,i)] ..
    f(i,j) - [ SB * [theta(i) - theta(j)] / react(i,j) ] =E= 0
    ;

E_SLACK_G(i)..
    theta(i) $ global_slack(i) =E= 0;
    ;

Lag_qk(kdso)..
    + SUM[(idso,dso) $ (it(idso,kdso) and is(dso,idso)),Lamb4(idso,dso)]
    + SUM[dso $ ks(kdso,dso),Lamb7(kdso,dso)]
    =E= 0
    ;

Lag_fpij(idso,j) $ ii(idso,j)..
    - SUM[dso $ is(dso,idso),Lamb1(idso,dso)]
    + SUM[dso $ is(dso,j),Lamb1(j,dso)]
    + SUM[dso $ (is(dso,idso) and subs(j)),Lamb2(idso,j,dso)]
    - SUM[dso $ is(dso,idso), 2*Lamb6(idso,j,dso)*resis(idso,j)/SB] 
    + MuFpPlus(idso,j)
    - MuFpMinus(idso,j)
    =E= 0
    ;

Lag_fqij(idso,j) $ ii(idso,j)..
    - SUM[dso $ is(dso,idso),Lamb4(idso,dso)]
    + SUM[dso $ is(dso,j),Lamb4(j,dso)]
    + SUM[dso $ is(dso,idso), Lamb5(idso,j,dso)  $ subs(j)]
    - SUM[dso $ is(dso,idso), 2*Lamb6(idso,j,dso)*react(idso,j)/SB]
    + MuFqPlus(idso,j)
    - MuFqMinus(idso,j)
    =E= 0
    ;

Lag_wi(theta_dso) ..
     + SUM[dso,Lamb3(dso,theta_dso) $ dso_slack(dso,theta_dso)]   
     + SUM[(dso,j) $ (ii(theta_dso,j) and is(dso,theta_dso)), Lamb6(theta_dso,j,dso)]
     - SUM[(dso,j) $ (ii(j,theta_dso) and is(dso,j)), Lamb6(j,theta_dso,dso)]
     + MuVPlus(theta_dso)
     - MuVMinus(theta_dso)
     =E= 0
     ;

Lag_slackQij(idso,j) $ ii(idso,j) ..
     - SUM[dso $ is(dso,idso), Lamb5(idso,j,dso) $ subs(j)]
     =E= 0
     ;

E_CS3T_2(dso)..
    - SUM[i $ is(dso,i), DA_Dispatch(i)]
    + SUM[i $ is(dso,i), d(i)]
    + SUM[i $ is(dso,i), imb(i)]
    - SUM[(i,k) $ (is(dso,i) and it(i,k)), p_pos(k)]
    + SUM[(i,k) $ (is(dso,i) and it(i,k)), p_neg(k)]
    =E=
    virtualdemand(dso)
    ;

Lag_pkup_BL(kdso)..
    + bid_up(kdso)
    + SUM[dso $ ks(kdso,dso), intPrice_var(dso)]
    + SUM[(idso,dso) $ (it(idso,kdso) and is(dso,idso)),Lamb1(idso,dso)]
    - SUM[(idso,j,dso) $ (ii(idso,j) and is(dso,idso) and ks(kdso,dso) and subs(j)),Lamb2(idso,j,dso)]
    - SUM[dso $ ks(kdso,dso),Lamb7(kdso,dso) * PFACTOR]
    + MuUPPlus(kdso)
    - MuUPZero(kdso)
    =E= 0
    ;
    
Lag_pkdw_BL(kdso)..
    + bid_dw(kdso)
    - SUM[dso $ ks(kdso,dso), intPrice_var(dso)]
    - SUM[(idso,dso) $ (it(idso,kdso) and is(dso,idso)),Lamb1(idso,dso)]
    + SUM[(idso,j,dso) $ (ii(idso,j) and is(dso,idso) and ks(kdso,dso) and subs(j)),Lamb2(idso,j,dso)]
    + SUM[dso $ ks(kdso,dso),Lamb7(kdso,dso) * PFACTOR]
    + MuDWPlus(kdso)
    - MuDWZero(kdso)
    =E= 0
    ;

E_DSO_SLACK(dso,i) $ dso_slack(dso,i) ..
    w(i) =E= 1
    ;

E_DMND_B_D_Q(i,dso)  $ [is(dso,i) and not subs(i)] ..
    + SUM[k $ it(i,k), q(k)]
    - SUM[j $ ii(i,j), fq(i,j)]
    + SUM[j $ ii(j,i), fq(j,i)]
    - dq(i)
    =E=
    0
    ;

E_SUBS_PF_Q(i,subs,dso) $ (ii(i,subs) and is(dso,i))..
    fq(i,subs) - slack_q(i,subs) =E= 0
    ;

E_DISTFLOW(i,j,dso)  $ (ii(i,j) and is(dso,i)) ..
    w(i) - w(j) - 2*(resis(i,j)*(f(i,j)/SB) + react(i,j)*(fq(i,j)/SB)) =E= 0;


E_PF(kdso,dso) $ ks(kdso,dso)..
    q(kdso) - PFACTOR*(p_pos(kdso) - p_neg(kdso)) =E= 0;   

Lag_pkup_T(ktso)..
    + bid_up(ktso)
    + SUM[(itso,tso) $ it(itso,ktso),LambT3(itso,tso)]
    + MuUPPlus(ktso)
    - MuUPZero(ktso)
    =E= 0
    ;
    
Lag_pkdw_T(ktso)..
    + bid_dw(ktso)
    - SUM[(itso,tso) $ it(itso,ktso),LambT3(itso,tso)]
    + MuDWPlus(ktso)
    - MuDWZero(ktso)
    =E= 0
    ;
    
Lag_fij_T(itso,j) $ ii(itso,j)..
    - SUM[tso $ is(tso,itso),LambT3(itso,tso)]
    + SUM[tso $ is(tso,j),LambT3(j,tso)]
    + SUM[(tso,dso) $ (is(tso,itso) and ii(itso,j) and subs(j) and inter(tso,j,dso)),LambT1(itso,j,tso,dso)]
    + SUM[tso $ (is(tso,itso) and ii(itso,j)),LambT4(itso,j,tso)]
    + MuFpPlus(itso,j)
    - MuFpMinus(itso,j)
    =E= 0
    ;

Lag_Thetai_T(theta_tso)..
    - SUM[(tso,j) $ (is(tso,theta_tso) and ii(theta_tso,j)), [ SB / react(theta_tso,j)] * LambT4(theta_tso,j,tso)]
    + SUM[(tso,j) $ (is(tso,j) and ii(j,theta_tso)), [ SB / react(j,theta_tso)] * LambT4(j,theta_tso,tso)]   
    + LambT5(theta_tso) $ global_slack(theta_tso)
    + MuThetaPlus(theta_tso)
    - MuThetaMinus(theta_tso)    
    =E= 0
    ;

Lag_virts_T(dso)..
    + SUM[(tso,itso,j) $ (is(tso,itso) and ii(itso,j) and subs(j) and inter(tso,j,dso)),LambT1(itso,j,tso,dso)]
    - LambT2(dso)
    =E=
    0
    ;

E_SUBS_BALANCE_CS2(i,subs,tso,dso) $ [inter(tso,subs,dso) and ii(i,subs) and is(tso,i)]..
    f(i,subs) =E= virtualdemand(dso)
    ;

UPPlus_U_CC1(k)..
    - p_pos(k) + pmax(k) =G= 0;

UPPlus_U_CC2(k)..
    MuUPPlus(k) =G= 0;
    
UPPlus_U_CC3(k)..
    - p_pos(k) + pmax(k) =L= b01(k)*M01P;  

UPPlus_U_CC4(k)..
    MuUPPlus(k) =L= (1-b01(k))*M01D;

UPZero_U_CC1(k)..
    p_pos(k) =G= 0;

UPZero_U_CC2(k)..
    MuUPZero(k) =G= 0;

UPZero_U_CC3(k)..
    p_pos(k) =L= b02(k)*M02P;

UPZero_U_CC4(k)..
    MuUPZero(k) =L= (1-b02(k))*M02D;

DWPlus_U_CC1(k)..
    - p_neg(k) + pmin(k) =G= 0;

DWPlus_U_CC2(k)..
    MuDWPlus(k) =G= 0;

DWPlus_U_CC3(k)..
     - p_neg(k) + pmin(k) =L= b03(k)*M03P;    

DWPlus_U_CC4(k)..
    MuDWPlus(k) =L= (1-b03(k))*M03D;

DWZero_U_CC1(k)..
    p_neg(k) =G= 0;

DWZero_U_CC2(k)..
    MuDWZero(k) =G= 0;
    
DWZero_U_CC3(k)..
    p_neg(k) =L= b04(k)*M04P;

DWZero_U_CC4(k)..
    MuDWZero(k) =L= (1-b04(k))*M04D;

FpPlus_U_CC1(i,j) $ ii(i,j)..
    - f(i,j) + fmax(i,j) =G= 0;

FpPlus_U_CC2(i,j) $ ii(i,j)..
    MuFpPlus(i,j) =G= 0;

FpPlus_U_CC3(i,j) $ ii(i,j)..
    - f(i,j) + fmax(i,j) =L= b05(i,j)*M05P;
 
FpPlus_U_CC4(i,j) $ ii(i,j)..
    MuFpPlus(i,j) =L= (1-b05(i,j))*M05D;

FpMinus_U_CC1(i,j) $ ii(i,j)..
    -fmin(i,j) + f(i,j) =G= 0;

FpMinus_U_CC2(i,j) $ ii(i,j)..
    MuFpMinus(i,j) =G= 0;

FpMinus_U_CC3(i,j) $ ii(i,j)..
    -fmin(i,j) + f(i,j) =L= b06(i,j)*M06P;

FpMinus_U_CC4(i,j) $ ii(i,j)..
    MuFpMinus(i,j) =L= (1-b06(i,j))*M06D;

FqPlus_CC1(idso,j) $ ii(idso,j)..
   - fq(idso,j) + fqmax(idso,j) =G= 0;    

FqPlus_CC2(idso,j) $ ii(idso,j)..
    MuFqPlus(idso,j) =G= 0;

FqPlus_CC3(idso,j) $ ii(idso,j)..
   - fq(idso,j) + fqmax(idso,j) =L= b07(idso,j)*M07P;
   
FqPlus_CC4(idso,j) $ ii(idso,j)..
    MuFqPlus(idso,j) =L= (1-b07(idso,j))*M07D;
    
FqMinus_CC1(idso,j) $ ii(idso,j)..
    -fqmin(idso,j) + fq(idso,j) =G= 0;
    
FqMinus_CC2(idso,j) $ ii(idso,j)..
    MuFqMinus(idso,j) =G= 0;

FqMinus_CC3(idso,j) $ ii(idso,j)..
    -fqmin(idso,j) + fq(idso,j) =L= b08(idso,j)*M08P;   

FqMinus_CC4(idso,j) $ ii(idso,j)..
    MuFqMinus(idso,j) =L= (1-b08(idso,j))*M08D;

VPlus_CC1(theta_dso)..
     -w(theta_dso) + sqr(v_max) =G= 0;
     
VPlus_CC2(theta_dso)..
    MuVPlus(theta_dso) =G= 0;

VPlus_CC3(theta_dso)..
     -w(theta_dso) + sqr(v_max) =L= b09(theta_dso)*M09P;   

VPlus_CC4(theta_dso)..
    MuVPlus(theta_dso) =L= (1-b09(theta_dso))*M09D;
    
VMinus_CC1(theta_dso)..
     - sqr(v_min) + w(theta_dso) =G= 0;

VMinus_CC2(theta_dso)..
     MuVMinus(theta_dso) =G= 0;
     
VMinus_CC3(theta_dso)..
     - sqr(v_min) + w(theta_dso) =L= b10(theta_dso)*M10P;  

VMinus_CC4(theta_dso)..
     MuVMinus(theta_dso) =L= (1-b10(theta_dso))*M10D; 

ThetaiPlus_T_CC1(theta_tso)..
    - theta(theta_tso) + theta_max =G= 0;
    
ThetaiPlus_T_CC2(theta_tso)..
    MuThetaPlus(theta_tso) =G= 0;

ThetaiPlus_T_CC3(theta_tso)..
    - theta(theta_tso) + theta_max =L= b11(theta_tso)*M11P;       

ThetaiPlus_T_CC4(theta_tso)..
    MuThetaPlus(theta_tso) =L= (1-b11(theta_tso))*M11D;

ThetaiMinus_T_CC1(theta_tso)..
    +theta_max + theta(theta_tso) =G= 0;
    
ThetaiMinus_T_CC2(theta_tso)..
    MuThetaMinus(theta_tso) =G= 0;

ThetaiMinus_T_CC3(theta_tso)..
    +theta_max + theta(theta_tso) =L= b12(theta_tso)*M12P;        

ThetaiMinus_T_CC4(theta_tso)..
    MuThetaMinus(theta_tso) =L= (1-b12(theta_tso))*M12D;

MODEL BILEVEL_1
/
ObjFuncUpper
Cap_price

E_DMND_B_D
E_SUBS_PF
E_DSO_SLACK
E_DMND_B_D_Q
E_SUBS_PF_Q
E_DISTFLOW
E_PF
Lag_pkup_BL
Lag_pkdw_BL
Lag_qk
Lag_fpij
Lag_fqij
Lag_wi
Lag_slackQij

E_DMND_B_T
E_FLUX_B_T
E_CS3T_2
E_SUBS_BALANCE_CS2
E_SLACK_G
Lag_pkup_T
Lag_pkdw_T
Lag_fij_T
Lag_Thetai_T
Lag_virts_T

UPPlus_U_CC1
UPPlus_U_CC2
UPPlus_U_CC3
UPPlus_U_CC4
UPZero_U_CC1
UPZero_U_CC2
UPZero_U_CC3
UPZero_U_CC4
DWPlus_U_CC1
DWPlus_U_CC2
DWPlus_U_CC3
DWPlus_U_CC4
DWZero_U_CC1
DWZero_U_CC2
DWZero_U_CC3
DWZero_U_CC4
FpPlus_U_CC1
FpPlus_U_CC2
FpPlus_U_CC3
FpPlus_U_CC4
FpMinus_U_CC1
FpMinus_U_CC2
FpMinus_U_CC3
FpMinus_U_CC4

FqPlus_CC1
FqPlus_CC2
FqPlus_CC3
FqPlus_CC4
FqMinus_CC1
FqMinus_CC2
FqMinus_CC3
FqMinus_CC4
VPlus_CC1
VPlus_CC2
VPlus_CC3
VPlus_CC4
VMinus_CC1
VMinus_CC2
VMinus_CC3
VMinus_CC4

ThetaiPlus_T_CC1
ThetaiPlus_T_CC2
ThetaiPlus_T_CC3
ThetaiPlus_T_CC4
ThetaiMinus_T_CC1
ThetaiMinus_T_CC2
ThetaiMinus_T_CC3
ThetaiMinus_T_CC4
/
;

SOLVE BILEVEL_1 USING MIP MINIMIZING ObjValueUpper;

* REPORTING *

$set mktmodel "Bilevel"

report('%mktmodel%','ACTIV_TOTAL')=
            + SUM[k, (bid_up(k) * p_pos.l(k)) + (bid_dw(k) * p_neg.l(k))]
            ;

report('%mktmodel%','COST_DSO')=
            + SUM[kdso, (bid_up(kdso) * p_pos.l(kdso)) + (bid_dw(kdso) * p_neg.l(kdso))]
            + SUM[(dso,kdso) $ ks(kdso,dso), (p_pos.l(kdso) - p_neg.l(kdso)) * intPrice_var.l(dso)]
            ;

report('%mktmodel%','ACTIV_DSO')=
            + SUM[kdso, (bid_up(kdso) * p_pos.l(kdso)) + (bid_dw(kdso) * p_neg.l(kdso))]
            ;

report('%mktmodel%','TRANSF_TSO')=
            + SUM[(dso,kdso) $ ks(kdso,dso), (p_pos.l(kdso) - p_neg.l(kdso)) * intPrice_var.l(dso)]
            ;
            
report('%mktmodel%','COST_TSO')=
            + SUM[ktso, (bid_up(ktso) * p_pos.l(ktso)) + (bid_dw(ktso) * p_neg.l(ktso))]
            - SUM[(dso,kdso) $ ks(kdso,dso), (p_pos.l(kdso) - p_neg.l(kdso)) * intPrice_var.l(dso)]
            ;

report('%mktmodel%','ACTIV_TSO')=
            + SUM[ktso, (bid_up(ktso) * p_pos.l(ktso)) + (bid_dw(ktso) * p_neg.l(ktso))]
            ;

* ----------------------- *
* CAP AND FLOOR MECHANISM *
* ----------------------- *

Parameter
CapMaxBid
CapMinBid
;

CapMaxBid = sum[k,bid_dw(k)*pmin(k)]/sum[k,pmin(k)]*1.5;
CapMinBid = sum[k,bid_dw(k)*pmin(k)]/sum[k,pmin(k)]*0.5;

Display CapMaxBid, CapMinBid;

Equations
Cap_price_Max
Cap_price_Min
;

Cap_price_Max(dso)..
    intPrice_var(dso) =L= CapMaxBid
    ;

Cap_price_Min(dso)..
    intPrice_var(dso) =G= CapMinBid
    ;


MODEL BILEVEL_2
/
ObjFuncUpper
Cap_price_Max
Cap_price_Min

E_DMND_B_D
E_SUBS_PF
E_DSO_SLACK
E_DMND_B_D_Q
E_SUBS_PF_Q
E_DISTFLOW
E_PF
Lag_pkup_BL
Lag_pkdw_BL
Lag_qk
Lag_fpij
Lag_fqij
Lag_wi
Lag_slackQij

E_DMND_B_T
E_FLUX_B_T
E_CS3T_2
E_SUBS_BALANCE_CS2
E_SLACK_G
Lag_pkup_T
Lag_pkdw_T
Lag_fij_T
Lag_Thetai_T
Lag_virts_T

UPPlus_U_CC1
UPPlus_U_CC2
UPPlus_U_CC3
UPPlus_U_CC4
UPZero_U_CC1
UPZero_U_CC2
UPZero_U_CC3
UPZero_U_CC4
DWPlus_U_CC1
DWPlus_U_CC2
DWPlus_U_CC3
DWPlus_U_CC4
DWZero_U_CC1
DWZero_U_CC2
DWZero_U_CC3
DWZero_U_CC4
FpPlus_U_CC1
FpPlus_U_CC2
FpPlus_U_CC3
FpPlus_U_CC4
FpMinus_U_CC1
FpMinus_U_CC2
FpMinus_U_CC3
FpMinus_U_CC4

FqPlus_CC1
FqPlus_CC2
FqPlus_CC3
FqPlus_CC4
FqMinus_CC1
FqMinus_CC2
FqMinus_CC3
FqMinus_CC4
VPlus_CC1
VPlus_CC2
VPlus_CC3
VPlus_CC4
VMinus_CC1
VMinus_CC2
VMinus_CC3
VMinus_CC4

ThetaiPlus_T_CC1
ThetaiPlus_T_CC2
ThetaiPlus_T_CC3
ThetaiPlus_T_CC4
ThetaiMinus_T_CC1
ThetaiMinus_T_CC2
ThetaiMinus_T_CC3
ThetaiMinus_T_CC4
/
;

SOLVE BILEVEL_2 USING MIP MINIMIZING ObjValueUpper;

* REPORTING *

$set mktmodel "Bilevel_Reg"

report('%mktmodel%','ACTIV_TOTAL')=
            + SUM[k, (bid_up(k) * p_pos.l(k)) + (bid_dw(k) * p_neg.l(k))]
            ;

report('%mktmodel%','COST_DSO')=
            + SUM[kdso, (bid_up(kdso) * p_pos.l(kdso)) + (bid_dw(kdso) * p_neg.l(kdso))]
            + SUM[(dso,kdso) $ ks(kdso,dso), (p_pos.l(kdso) - p_neg.l(kdso)) * intPrice_var.l(dso)]
            ;

report('%mktmodel%','ACTIV_DSO')=
            + SUM[kdso, (bid_up(kdso) * p_pos.l(kdso)) + (bid_dw(kdso) * p_neg.l(kdso))]
            ;

report('%mktmodel%','TRANSF_TSO')=
            + SUM[(dso,kdso) $ ks(kdso,dso), (p_pos.l(kdso) - p_neg.l(kdso)) * intPrice_var.l(dso)]
            ;
            
report('%mktmodel%','COST_TSO')=
            + SUM[ktso, (bid_up(ktso) * p_pos.l(ktso)) + (bid_dw(ktso) * p_neg.l(ktso))]
            - SUM[(dso,kdso) $ ks(kdso,dso), (p_pos.l(kdso) - p_neg.l(kdso)) * intPrice_var.l(dso)]
            ;

report('%mktmodel%','ACTIV_TSO')=
            + SUM[ktso, (bid_up(ktso) * p_pos.l(ktso)) + (bid_dw(ktso) * p_neg.l(ktso))]
            ;

Display report;

execute  'del tasks.txt';
execute  'del %data_set%.gdx';
