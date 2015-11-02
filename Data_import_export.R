## read Stata 5-12 data
library(foreign)
dat <- read.dta("path/to/data/file.dta")

## read Stata 13 data
install.packages("readstata13") ## need to install first
library(readstata13)
dat <- read.dta13("path/to/data/file.dta")
