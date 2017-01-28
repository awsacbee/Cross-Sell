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
CrossSellRefined$SELLER[CrossSellRefined$SELLER == "NIELLO ACURA"] <- "NIELLO ACURA - NIELLO VOLVO SACRAMENTO"
CrossSellRefined$SELLER[CrossSellRefined$SELLER == "NIELLO AUDI"] <- "NIELLO AUDI LAND ROVER JAGUAR SACRAMENTO"
CrossSellRefined$SELLER[CrossSellRefined$SELLER == "LASHER AUDI"] <- "ELK GROVE AUDI"
CrossSellRefined$SELLER[CrossSellRefined$SELLER == "ELK GROVE BUICK PONTIAC GMC"] <- "ELK GROVE BUICK GMC"



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

#create dealer groups
Roseville.Auto.Mall <- c("NIELLO ACURA - NIELLO VOLVO SACRAMENTO", 
                         "AUTONATION BMW ROSEVILLE", 
                         "RELIABLE BUICK GMC CADILLAC", 
                         "JOHN L SULLIVAN CHEVROLET", 
                         "AUTONATION CHRY DODGE JEEP RAM ROSEVILLE", 
                         "AUTONATION FIAT ROSEVILLE",
                         "FUTURE FORD OF ROSEVILLE", 
                         "AUTONATION HONDA ROSEVILLE", 
                         "ROSEVILLE MITSUBISHI KIA", 
                         "LEXUS OF ROSEVILLE",
                         "AUTONATION MAZDA SUBARU ROSEVILLE",
                         "ROSEVILLE VOLKSWAGEN",
                         "FUTURE NISSAN",
                         "SACRAMENTO INFINITI",
                         "ROSEVILLE TOYOTA")

Folsom.Auto.Mall <- c("FOLSOM LAKE HYUNDAI", 
                      "FOLSOM LAKE TOYOTA", 
                      "FOLSOM LAKE HONDA",
                      "FOLSOM CHEVROLET",
                      "FOLSOM LAKE FORD",
                      "FOLSOM LAKE KIA",
                      "FUTURE NISSAN OF FOLSOM",
                      "FOLSOM BUICK GMC",
                      "FOLSOM LAKE VOLKSWAGEN",
                      "FOLSOM LAKE CHRYSLER DODGE JEEP RAM")

Elk.Grove.Auto.Mall <- c("MAITA CHEVROLET",
                        "ELK GROVE HONDA",
                        "ELK GROVE KIA",
                        "MAZDA OF ELK GROVE",
                        "NISSAN OF ELK GROVE",
                        "ELK GROVE TOYOTA",
                        "ELK GROVE DODGE",
                        "INFINITI OF ELK GROVE",
                        "ELK GROVE ACURA",
                        "ELK GROVE FORD",
                        "ELK GROVE VOLKSWAGEN",
                        "ELK GROVE BUICK GMC",
                        "ELK GROVE AUDI",
                        "NIELLO BMW ELK GROVE / SACRAMENTO")

                         
CrossSellRefined$Group <- ifelse(trim(CrossSellRefined$SELLER) %in% Roseville.Auto.Mall, "Roseville Auto Mall",
                                 ifelse(trim(CrossSellRefined$SELLER) %in% Folsom.Auto.Mall, "Folsom Auto Mall",
                                        ifelse(trim(CrossSellRefined$SELLER) %in% Elk.Grove.Auto.Mall, "Elk Grove Auto Mall", "Other")))
table(CrossSellRefined$Group)
rm(Roseville.Auto.Mall, Folsom.Auto.Mall, Elk.Grove.Auto.Mall)

#Export Results
#write.csv(CrossSellRefined, "C:/Users/awelden/Google Drive/MAD Science/Internal Tools/CrossSell/Data/CrossSellThroughMAY2015withFleet.csv")

CrossSellRefined$MON.YEAR <- substr(CrossSellRefined$MON, 1, 4)
table(CrossSellRefined$MON.YEAR)
write.csv(CrossSellRefined[CrossSellRefined$MON.YEAR %in% c(2014, 2015, 2016),], "~/Data/Cross Sell/Data/CrossSell2017.csv")

DealerCheck <- CrossSellRefined[CrossSellRefined$Geography == 2 & CrossSellRefined$NewUsed == "New",c("MON", "SELLER", "Geography", "Group", "NewUsed")]
write.csv(DealerCheck, "~/Data/Cross Sell/Data/DealerCheck.csv", row.names = FALSE)


