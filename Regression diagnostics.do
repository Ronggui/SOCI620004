use Prestige

* the regression of prestige on income and education
regress prestige income education

* standardised coefficents
regress prestige income education, beta

** outliers, influential obs
** you can choose whatever variable name for the first argument
predict fitted, xb
predict hval, hat
* predict hval, leverage
predict resid, residual
predict rstd, rstandard
predict rstud, rstudent
predict cookdistance, cooksd

gen id=_n
twoway (dropline rstud id, mlabel(id))

* inspect leverage and cooksd
gen cookdistancesqrt = sqrt(cookdistance)
twoway (scatter rstud hval [weight=cookdistancesqrt], msymbol(oh))

* leverage-versus-squared-residuals plot
lvr2plot, mlabel(id)

** added-variable plot; aka, partial regression plot
** joint influence of mutiple obs
avplot income
avplots

* you can manually construct the same plot
reg prestige education
predict yr, residual
reg income education
predict xr, residual
twoway (scatter yr xr) (lfit yr xr)

* normality
hist resid
kdensity resid
qnorm resid

regress prestige income education
* Heteroskedasticity
estat hettest
* stardardized residuals
predict resid, rstandard
predict fitted
gen residsq = resid*resid
gen absresid = abs(resid)
twoway (scatter resid fitted)

* can the use a shorthand of rvfplot
rvfplot
* robust std err
regress prestige income education, robust

* linearity
twoway (scatter prestige income)
graph matrix income education prestige
* examine linearity and suggest alternative functional form
cprplot income, lowess

* perfect multicollinearity
gen income_n = income
regress prestige income income_n
* high correlation
drop income_n 
gen income_n = 0.9*income + rnormal()
* vif to detect multicollearity
regress prestige income education
estat vif

* demonstrate the effect of centering
gen incomesq=income*income
reg  prestige education income incomesq
estat vif
sum income
gen incomeC=income-r(mean)
gen incomeCsq=incomeC^2
reg  prestige education  incomeC incomeCsq
estat vif

* omitted variable test
ovtest
* demonstration the effect of omitted variables
gen incomesq=income*income
gen prestige_sim =2.456574+ 1.08039* income+0.8* incomesq+rnormal()
reg  prestige_sim income

regress prestige income education women
* wald test / F test
test income education

regress prestige income education women
estimates store ful
regress prestige women
estimates store constr
lrtest constr ful
