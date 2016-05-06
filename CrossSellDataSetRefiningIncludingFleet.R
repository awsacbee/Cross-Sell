CrossSell <- read.delim("C:/Users/awelden/Google Drive/CrossSell/CSMaster.txt", header=TRUE, sep=",")

#Appending New Month
CrossSell$X <- NULL
CrossSellNewAppend <- read.delim("C:/Users/jbaretsky/Desktop/novnew.txt", header=TRUE, sep="\t")
CrossSellOldAppend <- read.delim("C:/Users/jbaretsky/Desktop/novused.txt", header=TRUE, sep="\t")
CrossSellOldAppend$NewUsed <- "Used"
CrossSellNewAppend$NewUsed <- "New"

install.packages("plyr")
library("plyr")
CrossSell <- rbind.fill(CrossSell, CrossSellNewAppend, CrossSellOldAppend)
#Re-save to update master copy
write.csv(CrossSell, "C:/Users/jbaretsky/Documents/Tableau/Automotive/CSMaster.txt", sep=",")

#Clean up spaces in WGT field
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
CrossSellRefined <- CrossSell
CrossSellRefined$WGT<-trim(CrossSell$WGT)

AuctionVendors <- c("PROSPECT MOTORS", "INSURANCE AUTO AUCTIONS INC", "COPART AUTO AUCTIONS")
CrossSellRefined$RecodedType <- NA

CrossSellRefined$RecodedType <- ifelse( grepl("New",CrossSellRefined$NewUsed), 1, 2)
CrossSellRefined$RecodedType <- ifelse( grepl("F",CrossSellRefined$WGT), 99, CrossSellRefined$RecodedType)
CrossSellRefined$RecodedType <- ifelse(CrossSellRefined$SELLER %in% AuctionVendors, 98, CrossSellRefined$RecodedType)


#Determine if County is in CBSA or DMA and replace COUNTYNUM with boolean for CBSA or DMA
CountiesinCBSA <-c(17,61, 67, 113)
CountiesinDMA <-c(5, 9, 11, 17, 57, 61, 63, 67, 77, 91, 95, 99, 101, 109, 113, 115)
CrossSellRefined$Geography <- ifelse(CrossSellRefined$COUNTYNUM %in% CountiesinDMA, 1, 0)
CrossSellRefined$Geography <- ifelse(CrossSellRefined$COUNTYNUM %in% CountiesinCBSA, 2, CrossSellRefined$Geography)

#Remove PO Box Zip Codes, replace with Enclosing zips
POBoxZipMatches <- read.delim("C:/Users/jbaretsky/Documents/Tableau/Automotive/POBoxeswithEnclosingZipsSacDMA.txt", header=TRUE, sep=",")
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

#dealers <- CrossSell
#dealers <-unique(dealers$SELLER)
#write.csv(dealers, "dealerlist.csv")

#Combine old and new dealer names
CrossSellRefined$SELLER <- as.character(CrossSellRefined$SELLER)
CrossSellRefined$SELLER[CrossSellRefined$SELLER == "MAITAS NISSAN OF SACRAMENTO"] <- "NISSAN OF SACRAMENTO"

#Add in Lat/Long for Dealers
ZipCenter <- read.delim("C:/Users/jbaretsky/Documents/Tableau/Automotive/Ziplistlatitude.txt", header=TRUE, sep="\t")
CrossSellRefined <- merge(CrossSellRefined, ZipCenter, by.x = "ZIP", by.y = "zip.code", all.x = TRUE)


#Add in Count of Cars sold for mapping test
DealerLocation <- read.delim("C:/Users/jbaretsky/Documents/Tableau/Automotive/DealerLocation.txt", header=TRUE, sep="\t")
CrossSellRefined["CarCount"]<-1
CrossSellRefined["RecordType"]<-"CarSale"
DealerLocation["RecordType"] <- "Location"
CrossSellRefined <- merge(CrossSellRefined, DealerLocation, c("RecordType", "Latitude", "Longitude"), all = TRUE)
CrossSellRefined$Geography[CrossSellRefined$RecordType == "Location"] <- 2

#Export Results
write.csv(CrossSellRefined, "CrossSellThroughMAY2015withFleet.csv")

