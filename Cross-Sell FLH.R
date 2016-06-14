### import cross sell
CrossSellRefined.flh <- read.delim("C:/Users/awelden/Google Drive/MAD Science/Internal Tools/CrossSell/Data/CrossSellThroughMAY2015withFleet.csv", header=TRUE, sep=",")
names(CrossSellRefined.flh)

### import customer file
#install.packages("readxl")
library(readxl)
flh.cust <- read_excel("C:/Users/awelden/Google Drive/MAD Science/Internal Tools/Tableau/Folsom Lake Honda/deduped wtef.xlsx")


### Folsom Lake Honda only
CrossSell2014tocurrent <- CrossSellRefined.flh[CrossSellRefined$MON %in% c(grep("2014", CrossSellRefined$MON, ignore.case=T, value=T), grep("2015", CrossSellRefined$MON, ignore.case=T, value=T),grep("2016", CrossSellRefined$MON, ignore.case=T, value=T)),]

names(flh.cust)
is(CrossSell2014tocurrent )