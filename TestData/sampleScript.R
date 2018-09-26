library(BridgeTree);
library(ape);

#Get tree
tree <- read.nexus("~/Dropbox/BridgeTree/TestData/Fish_12Tax_time_calibrated.tre");

#Query databases for names
myBridgeTreeObject <- studyTaxonList(x = tree, datasources = "NCBI");

#Query GBIF for occurrence data
login <- GBIFLoginManager(user = "******",
                          email = "*****@*****",
                          pwd = "*****");
myBridgeTreeObject <- occQuery(x = myBridgeTreeObject, GBIFLogin = login);

#Get citations
myOccCitations <- occCitation(myBridgeTreeObject);
