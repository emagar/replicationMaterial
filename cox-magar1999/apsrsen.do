version 7

use "C:\data\allpair83.dta", clear

***********************************************************
*Variable Descriptions for Senate Data                    *
*in file  allpair83.dta,                                  *
*January 11, 1999.                                        *
*                                                         *
*Variable Description                                     *
*                                                         *
*pairid   id number of pair of years to which data pertain*
*name     name of senator to which data pertain           *
*icpsr    icpsr id number of senator                      *
*entry    congress of entry to senate                     *
*dd1      change in 1st dimension of D-Nominate score     *
*loser    =1 if senator lost 2d election in pair          *
*dbus     change in business PAC contributions, nominal $ *
*dpct     change in vote percent                          *
*frosh    =1 if senator first elected at 1st election in  *
*            pair                                         *
*dw1      change in 1st dimension of W-Nominate score     *
*dem      =1 if senator a Democrat                        *
*dlog     change in log(vote proportion)                  *
*dbus83   change in bus. PAC contributions, 1983 $        *
*dleader  =+1 if senator gained party leadership position;*
*          -1 if senator lost party leadership position;  *
*           0 otherwise.                                  *
*majgain  =+1 if senator's party gained majority;         *
*          -1 if senator's party lost majority;           *
*           0 otherwise.                                  *
***********************************************************

*Regressions reported in table 2

reg  dbus83 dlog  frosh dw1 dem if y76==1, robust
reg  dbus83 dlog  frosh dw1 dem if y78==1, robust
reg  dbus83  dlog  frosh dw1 dem if y80==1, robust
reg  dbus83  dlog  frosh dw1 dem if y82==1, robust
reg  dbus83  dlog  frosh dw1 dem if y84==1, robust
reg  dbus83  dlog  frosh dw1 dem if y86==1, robust
reg  dbus83  dlog  frosh dw1 dem if y90==1, robust

*Regression reported in Table 3

reg dbus83 dlog  frosh dw1 majgain dleader y78 y80 y82 y84 y86 y90, robust

*Descriptives in Table A-1, Part 3

sum dbus83 dlog dw1 if dbus83~=.

*Descriptives in Table A-1, Part 4

tab majgain if dbus83~=.
tab frosh if dbus83~=.
tab dleader if dbus83~=.

