## read xlsx file
# install.packages("openxlsx")
library(openxlsx)
dat <- read.xlsx("path/to/xlsx/file.xlsx", sheet=1)
# use / instead of \ to represent the file path

## read Stata 5-12 data
library(foreign)
dat <- read.dta("path/to/data/file.dta")

## read Stata 13 data
install.packages("readstata13") ## need to install first
library(readstata13)
dat <- read.dta13("path/to/data/file.dta")

## export to Stata
write.dta(dat, file="path/to/data/file.dta")
