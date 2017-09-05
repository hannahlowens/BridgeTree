library(BridgeTree);
library(ape);

#Get tree
tree <- read.nexus("~/Dropbox/BridgeTree/TestData/Fish_12Tax_time_calibrated.tre");

#Query databases for names
queryResults <- studyTaxonList(x = tree);

#Query databases for occurrence data
sources <- c("gbif", "bison", "obis", "ala", "inat", "idigbio", "ebird", "ecoengine", "vertnet");
occurrences <- occ(as.character(queryList$`Best Match`), from = sources, limit = 10)
