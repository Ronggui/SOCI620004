use auto

sum mpg,detail

gen n=1
graph bar (sum) n, over(foreign) ytitle("Frequency")

hist mpg
hist mpg, normal

stem mpg

kdensity mpg

twoway (scatter price mpg)
twoway (scatter price mpg) (lfit price mpg)
twoway (scatter price mpg) (lfit price mpg, lcol(blue)) (lowess price mpg,lpat(dash))

graph box mpg
graph box mpg, over(foreign)

qnorm mpg

graph matrix mpg price weight

ladder mpg
qladder mpg
