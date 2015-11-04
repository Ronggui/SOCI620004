use Prestige

** slide 4-6
reg  prestige income education
** talk about: ereturn list
reg  income education
predict resid, residual
reg  prestige resid

** slide 11
reg  prestige income education
predict fitted
cor prestige fitted
disp `r(rho)'^2

** slide 9-10
** SST
local ybar = `r(mean)'
gen  yy = (prestige - `ybar')^2
sum yy, detail
return list
local sst = `r(sum)'
display `sst'
** SSR
predict r, residual
gen r2 = r^2
sum r2, detail
local ssr = `r(sum)'
display `ssr'
** SSE
predict yhat
gen yyy = yhat - `ybar'
gen yyy2 = yyy^2
sum yyy2, detail
local sse = `r(sum)'
display `sse'
**
display `sst'
display `sse' + `ssr'
display 1 - `ssr'/`sst'

** slide 12
reg  prestige income education
local sigma = e(rmse)
reg  prestige
local sigmanull = e(rmse)
display 1 - `sigma'^2/`sigmanull'^2

** slide 14
reg  prestige income education
display `e(rank)'
gen newX = 10 + 0.7 * income
** what happens?
reg prestige income education newX

** slide 16-19
reg  prestige income education
cor income education
** both are positive, omitted education and positive bias
** coef of income will become larger
reg  prestige income

** calc sigma^2
reg  prestige income education
local sigmahat2 =`ssr'/(`e(N)' - `e(rank)')
** sigma hat or Root MSE
display sqrt(`sigmahat2')

** slide 24
qui: reg income education
local den = (`e(rss)' + `e(mss)')*(1-`e(r2)')
display sqrt( `sigmahat2' / `den' )
** std. err of income, please check
reg prestige income education
