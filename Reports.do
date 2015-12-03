set more off

* need to install it first
* findit esttab
eststo clear

reg prestige income
eststo

reg prestige income education
eststo

esttab using "prestige.rtf", replace r2 ar2
