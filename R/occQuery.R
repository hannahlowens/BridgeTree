#' @title Query from Taxon List
#'
#' @description Takes rectified list of specimens from \code{\link{studyTaxonList}} and returns point data from \code{\link{rgbif}} with metadata.
#'
#' @param x An object of class \code{\link{bridgeTreeData}} (the results of a \code{\link{studyTaxonList}} search).
#'
#' @param datasources A vector of occurrence datasources to search. This is currently limited to GBIF, but may expand in the future.
#'
#' @param GBIFLogin An object of class \code{\link{GBIFLogin}} to log in to GBIF to begin the download.
#'
#' @param options A vector of options to pass to \code{\link{occ_download}}.
#'
#' @return The object of class \code{\link{bridgeTreeData}} supplied by the user as an argument, with occurrence data search results, as well as metadata on the occurrence sources queried.
#'
#' @examples
#' ## PLACEHOLDER
#' studyTaxonList(x = phylogeny, datasources = c('NCBI', 'EOL'));
#'
#' ## PLACEHOLDER
#' studyTaxonList(x = c("Buteo buteo", "Buteo buteo hartedi", "Buteo japonicus"), datasources = c('NCBI', 'EOL'));
#'
#' @export

occQuery <- function(x = NULL, datasources = "gbif", GBIFLogin = NULL, options = NULL) {
  #Error check input x.
  if (!class(x)=="bridgeTreeData"){
    warning("Input x is not of class 'bridgeTreeData'. Input x must be result of a studyTaxonList() search.\n");
    return(NULL);
  }

  #Error check input datasources.
  if (!is.vector(datasources) && class(datasources)=="character"){
    warning("Input datasources is not of class 'vector'. Datasources object must be a vector of class 'character'.\n");
    return(NULL);
  }

  #Check to see if the sources input are actually ones used by occQuery
  sources <- c("gbif"); #sources
  if(sum(!datasources %in% sources) > 0){
    warning(paste("The following datasources are not implemented in occQuery(): ", datasources[!datasources %in% sources], sep = ""));
    return(NULL);
  }
  else{
    x@occSources <- sources;
  }

  #If GBIF was selected, check to see if GBIF login information is supplied.
  if ("gbif" %in% datasources && !class(GBIFLogin)=="GBIFLogin"){
    warning("You have chosen GBIF as a datasource, but have not supplied GBIF login information. Please create a GBIFLogin object using GBIFLoginManager.\n");
    return(NULL);
  }

  #Get time stamp for search
  x@occurrenceSearchDate <- as.character(Sys.Date(), format = "%d %B, %Y");

  #Occurrence queries for each species
  queryResults <- x;

  #For GBIF
  searchTaxa <- as.character(queryResults@cleanedTaxonomy$`Best Match`);
  occSearchResults <- vector(mode = "list", length = length(searchTaxa));
  names(occSearchResults) <- searchTaxa;
  for (i in searchTaxa){
    temp <- getGBIFpoints(taxon = i, GBIFLogin = login);
    occSearchResults[[i]] <- temp;
  }
  #BIEN some day

  #Putting results into BridgeTree object
  queryResults@occResults <- occSearchResults;

  return(queryResults);
}
