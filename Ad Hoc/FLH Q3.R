#Check again Cross sell

CrossSellNewJune <- read.delim("~/Data/Cross Sell/Data/Raw Data/CA1803 (June).txt", sep="\t")
CrossSellOldJune <- read.delim("~/Data/Cross Sell/Data/Raw Data/CA1124U (June).txt", sep="\t")

CrossSellNewJune$NewUsed <- "New"
CrossSellOldJune$NewUsed <- "Used"

CrossSellNewJuly <- read.delim("~/Data/Cross Sell/Data/Raw Data/CA1803 (July).txt", sep="\t")
CrossSellOldJuly <- read.delim("~/Data/Cross Sell/Data/Raw Data/CA1124U (July).txt", sep="\t")

CrossSellNewJuly$NewUsed <- "New"
CrossSellOldJuly$NewUsed <- "Used"

CrossSellNewAug <- read.delim("~/Data/Cross Sell/Data/Raw Data/CA1803 (Aug).txt", sep="\t")
CrossSellOldAug <- read.delim("~/Data/Cross Sell/Data/Raw Data/CA1124U (Aug).txt", sep="\t")

CrossSellNewAug$NewUsed <- "New"
CrossSellOldAug$NewUsed <- "Used"

CrossSellNewSep <- read.delim("~/Data/Cross Sell/Data/Raw Data/CA1803 (Sep).txt", sep="\t")
CrossSellOldSep <- read.delim("~/Data/Cross Sell/Data/Raw Data/CA1124U (Sep).txt", sep="\t")

CrossSellNewSep$NewUsed <- "New"
CrossSellOldSep$NewUsed <- "Used"

#install.packages("plyr")
library("plyr")
CrossSell <- rbind.fill(CrossSellNewJune, CrossSellOldJune, 
                        CrossSellNewJuly, CrossSellOldJuly, 
                        CrossSellNewAug, CrossSellOldAug, 
                        CrossSellNewSep, CrossSellOldSep)
#Re-save to update master copy
table(CrossSell$MON) # Check for dupes!
table(CrossSell$NewUsed) # Check for dupes!

anyDuplicated(CrossSell)

#Clean up spaces in WGT field
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
CrossSellRefined <- CrossSell
CrossSellRefined$WGT<-trim(CrossSell$WGT)

anyDuplicated(CrossSellRefined)

AuctionVendors <- c("PROSPECT MOTORS", "INSURANCE AUTO AUCTIONS INC", "COPART AUTO AUCTIONS")
CrossSellRefined$RecodedType <- NA

CrossSellRefined$RecodedType <- ifelse( grepl("New",CrossSellRefined$NewUsed), 1, 2)
CrossSellRefined$RecodedType <- ifelse( grepl("F",CrossSellRefined$WGT), 99, CrossSellRefined$RecodedType)
CrossSellRefined$RecodedType <- ifelse(CrossSellRefined$SELLER %in% AuctionVendors, 98, CrossSellRefined$RecodedType)

table(CrossSellRefined$RecodedType ) #1:new, 2:used, 99:fleet, 98:auction vendor

#Determine if County is in CBSA or DMA and replace COUNTYNUM with boolean for CBSA or DMA
CountiesinCBSA <-c(17,61, 67, 113)
CountiesinDMA <-c(5, 9, 11, 17, 57, 61, 63, 67, 77, 91, 95, 99, 101, 109, 113, 115)
CrossSellRefined$Geography <- ifelse(CrossSellRefined$COUNTYNUM %in% CountiesinDMA, 1, 0)
CrossSellRefined$Geography <- ifelse(CrossSellRefined$COUNTYNUM %in% CountiesinCBSA, 2, CrossSellRefined$Geography)

table(CrossSellRefined$Geography) #DMA=1, CBSA=2 - they overlap

#Remove PO Box Zip Codes, replace with Enclosing zips
POBoxZipMatches <- read.csv("~/Data/Cross Sell/Data/POBoxeswithEnclosingZipsSacDMA.txt")
anyDuplicated(CrossSellRefined)

write.csv(CrossSellRefined, "~/Data/Cross Sell/Data/CrossSellThroughMAY2015withFleet3.csv")

rm(CrossSellNewJune, CrossSellOldJune, CrossSellNewJuly, CrossSellOldJuly, CrossSellNewAug, CrossSellOldAug, CrossSellNewSep, CrossSellOldSep)