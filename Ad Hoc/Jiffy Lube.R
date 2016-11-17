JiffyLube.1 <- read.xlsx2("~/Data/Jiffy Lube/Originals/201609.xlsx" ,1)

library(plyr)
require(utils)
library(RJSONIO)
library(reshape)

files <- list.files(path = "~/Data/Jiffy Lube/Originals/")
filenames <- paste("~/Data/Jiffy Lube/Originals/", files, sep="")

read_csv_filename <- function(filename){
  ret <- read.csv(filename)
  ret$Source <- filename #EDIT
  ret
}

import.list <- ldply(filenames, read_csv_filename)
head(import.list)

write.csv(import.list, "~/Data/CombinedInfoGroup/NewLeads.AllMarkets.csv")