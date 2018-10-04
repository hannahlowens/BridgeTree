library(rgbif)

#' @title Download occurrence points from GBIF
#'
#' @description Downloads occurrence points and useful related information for processing within other bridgetree functions
#'
#' @param taxon A single species
#'
#' @param GBIFLogin An object of class \code{\link{GBIFLogin}} to log in to GBIF to begin the download.
#'
#' @return A list containing (1) a dataframe of occurrence data; (2) GBIF search metadata
#'
#' @examples
#' getGBIFpoints(taxon="Gadus morhua");
#'
#' @export

getGBIFpoints<-function(taxon, GBIFLogin = GBIFLogin, GBIFDownloadDirectory = GBIFDownloadDirectory){

  key <- rgbif::name_suggest(q=taxon, rank='species')$key[1]
  occD <- rgbif::occ_download(paste("taxonKey = ", key, sep = ""),
                       "hasCoordinate = true", "hasGeospatialIssue = false",
                       user = GBIFLogin@username, email = GBIFLogin@email,
                       pwd = GBIFLogin@pwd);

  print(paste("Please be patient while GBIF prepares your download for ", taxon, ". This can take some time."))
  while (rgbif::occ_download_meta(occD[1])$status != "SUCCEEDED"){
    Sys.sleep(20);
    print(paste("Still waiting for", taxon, "download preparation to be completed."))
  }

  dir.create(file.path(setwd(GBIFDownloadDirectory), taxon), showWarnings = FALSE);
  res <- rgbif::occ_download_get(key=occD[1], overwrite=TRUE,
                                 file.path(getwd(), taxon));
  occFromGBIF <- rgbif::occ_download_import(res);
  occFromGBIF <- data.frame(occFromGBIF$gbifID, occFromGBIF$species,
                            occFromGBIF$decimalLongitude,
                            occFromGBIF$decimalLatitude, 
                            occFromGBIF$day, occFromGBIF$month,
                            occFromGBIF$year, occFromGBIF$datasetID,
                            occFromGBIF$datasetKey)
  dataService <- rep("BIEN", nrow(occFromGBIF));
  occFromGBIF <- cbind(occFromGBIF, dataService);
  colnames(occFromGBIF) <- c("gbifID", "Species", "Longitude", "Latitude",
                             "CollDay", "CollMonth", "CollYear", "Dataset",
                             "DatasetKey", "DataService")
  occMetadata <- rgbif::occ_download_meta(occD[1])

  #Preparing list for return
  outlist<-list();
  outlist[[1]]<-occFromGBIF;
  outlist[[2]]<-occMetadata;

  return(outlist);
}
