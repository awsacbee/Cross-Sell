CrossSellRefined$date <- as.Date(as.yearmon(as.character(CrossSellRefined$MON), "%Y%m"), frac = 1) 

#table(CrossSellRefined$SELLER)
Steves.Chevy <- CrossSellRefined[CrossSellRefined$date>="2015-01-31" & (CrossSellRefined$SELLER == "STEVES CHEVROLET BUICK" |
                                                                          CrossSellRefined$SELLER == "AMERICAN CHEVROLET" |
                                                                          CrossSellRefined$SELLER == "SIERRA MOTORS" |
                                                                          CrossSellRefined$SELLER == "ALFRED MATTHEWS BUICK GMC CADILLAC"),]
nrow(Steves.Chevy)
table(Steves.Chevy$SELLER)
#write.csv(Steves.Chevy, "~/Data/Cross Sell/Data/CrossSell.StevesChevy.comp.csv")

#rm(Steves.Chevy, CrossSellRefined,DealerLocation)
