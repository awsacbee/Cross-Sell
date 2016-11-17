require(xlsx)


FLH <- read.xlsx2("~/Data/Cross Sell/Ad Hoc/FLH Q3 All Sales (1).xlsx", 1)
head(FLH)

names(CrossSellRefined)
names(FLH)

## The intersection of two sets can be defined via match():
## Simple version:
## intersect <- function(x, y) y[match(x, y, nomatch = 0)]
intersect # the R function in base is slightly more careful
intersect(1:10, 7:20)

intersect(FLH$VIN, CrossSellRefined$VIN)


hold <- FLH[intersect(FLH$VIN, CrossSellRefined$VIN),]

names(FLH)

test <- merge(FLH,CrossSellRefined, by.x="VIN", by.y="VIN")
nrow(test)

write.csv(test, "~/Data/Cross Sell/Ad Hoc/FLH Q3 All Sales merged with CrossSell.csv")
anyDuplicated(CrossSellRefined$VIN)

names(CrossSellRefined)