version 5.0

use "C:\data\apsrhou.dta", clear

*Results reported in Table 1

regress chbus chmaj chrep chsafe chfrosh chprest chvotrec inlead,r
regress chlab chmaj chrep chsafe chfrosh chprest chvotrec inlead,r

*Descriptives in Table A-1, Part 1 

sum chbus chlab chsafe chvotrec 

*Descriptives in Table A-1, Part 2 

tab chmaj 
tab chrep 
tab chfrosh 
tab chprest 
tab inlead

