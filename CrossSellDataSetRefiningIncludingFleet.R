CrossSell <- read.csv("~/Data/Cross Sell/Data/CSMaster.txt")

head(CrossSell)
#Create unique indicator
table(CrossSell$MON) # Jan got duped
CrossSell$DupKey <- paste(CrossSell$VIN, CrossSell$MON, sep="_")

CrossSell = CrossSell[!duplicated(CrossSell$DupKey), ]
table(CrossSell$MON)

names(CrossSell)

#Appending New Month
CrossSell$X <- NULL
CrossSell$X.1 <- NULL

CrossSellNewAppend <- read.delim("~/Data/Cross Sell/Data/CA1803.txt")
CrossSellOldAppend <- read.delim("~/Data/Cross Sell/Data/CA1124U.txt")

CrossSellOldAppend$NewUsed <- "Used"
CrossSellNewAppend$NewUsed <- "New"

#install.packages("plyr")
library("plyr")
CrossSell <- rbind.fill(CrossSell, CrossSellNewAppend, CrossSellOldAppend)
#Re-save to update master copy
table(CrossSell$MON) # Check for dupes!
write.csv(CrossSell, "~/Data/Cross Sell/Data/CSMaster.txt")

#Clean up spaces in WGT field
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
CrossSellRefined <- CrossSell
CrossSellRefined$WGT<-trim(CrossSell$WGT)

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

CrossSellRefined <- merge(CrossSellRefined, POBoxZipMatches, by.x = "ZIP", by.y = "Zip_Code", all.x = TRUE)
CrossSellRefined$NewZips <- NA
CrossSellRefined$NewZips <- ifelse(is.na(CrossSellRefined$Enclosing_Zip) == FALSE , CrossSellRefined$Enclosing_Zip, CrossSellRefined$ZIP)

#Remove Unneeded Variables to save space
CrossSellRefined$Enclosing_Zip <- NULL
CrossSellRefined$VIN <-NULL
CrossSellRefined$REGCLASS <-NULL
CrossSellRefined$S_T <-NULL
CrossSellRefined$PUMPS <-NULL
CrossSellRefined$OWNER <-NULL
CrossSellRefined$ADDRESS <-NULL
CrossSellRefined$LIENHOLDER <-NULL
CrossSellRefined$ODOM_C <-NULL 
CrossSellRefined$ODOMETER <-NULL
CrossSellRefined$ID <-NULL
CrossSellRefined$DupKey <-NULL

#dealers <- CrossSell
#dealers <-unique(dealers$SELLER)
#write.csv(dealers, "dealerlist.csv")

#Combine old and new dealer names
CrossSellRefined$SELLER <- as.character(CrossSellRefined$SELLER)
CrossSellRefined$SELLER[CrossSellRefined$SELLER == "MAITAS NISSAN OF SACRAMENTO"] <- "NISSAN OF SACRAMENTO"

#Add in Lat/Long for Dealers
#ZipCenter <- read.delim("C:/Users/awelden/Google Drive/MAD Science/Internal Tools/CrossSell/Data/Ziplistlatitude.txt", header=TRUE, sep="\t")
ZipCenter <- read.csv("~/Data/Cross Sell/Data/ziplistlatitude.txt", header=TRUE, sep="\t")

CrossSellRefined <- merge(CrossSellRefined, ZipCenter, by.x = "ZIP", by.y = "zip.code", all.x = TRUE)


#Add in Count of Cars sold for mapping test
#DealerLocation <- read.delim("C:/Users/awelden/Google Drive/MAD Science/Internal Tools/CrossSell/Data/DealerLocation.txt", header=TRUE, sep="\t")
DealerLocation <- read.delim("~/Data/Cross Sell/Data/DealerLocation.txt", header=TRUE, sep="\t")

head(DealerLocation)
CrossSellRefined["CarCount"]<-1
CrossSellRefined["RecordType"]<-"CarSale"
head(CrossSellRefined)

DealerLocation["RecordType"] <- "Location"
CrossSellRefined <- merge(CrossSellRefined, DealerLocation, c("RecordType", "Latitude", "Longitude"), all = TRUE)
CrossSellRefined$Geography[CrossSellRefined$RecordType == "Location"] <- 2

#Export Results
#write.csv(CrossSellRefined, "C:/Users/awelden/Google Drive/MAD Science/Internal Tools/CrossSell/Data/CrossSellThroughMAY2015withFleet.csv")
write.csv(CrossSellRefined, "~/Data/Cross Sell/Data/CrossSellThroughMAY2015withFleet.csv")


