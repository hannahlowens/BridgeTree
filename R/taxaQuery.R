#' @title Query from Taxon List
#'
#' @description Takes rectified list of specimens from \code{\link{studyTaxonList}} and returns point data from \code{\link{occ}} with metadata.
#'
#' @param x An object of class \code{\link{bridgeTreeData}} (the results of a \code{\link{studyTaxonList}} search).
#'
#' @param datasources A vector of occurrence datasources to search, as implemented in \code{\link{occ}}.
#'
#' @param limit The maximum number of datapoints that should be returned from an \code{\link{occ}} search.
#'
#' @param options A vector of options to pass to \code{\link{occ}}.
#'
#' @return occurrenceData The object of class \code{\link{bridgeTreeData}} supplied by the user as an argument, with occurrence data search results, as well as metadata on the occurrence sources queried and the limit of occurrence point records downloaded.
#'
#' @examples
#' ## PLACEHOLDER
#' studyTaxonList(x = phylogeny, datasources = c('NCBI', 'EOL'));
#'
#' ## PLACEHOLDER
#' studyTaxonList(x = c("Buteo buteo", "Buteo buteo hartedi", "Buteo japonicus"), datasources = c('NCBI', 'EOL'));
#'
#' @export

taxaQuery <- function(x = NULL, datasources = "gbif", limit = 500, options = NULL) {
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

  #Check to see if the sources input are actually ones used by occ()
  sources <- c("gbif", "bison", "obis", "ala", "inat", "idigbio", "ebird", "ecoengine", "vertnet"); #occ() sources
  if(sum(!datasources %in% sources) > 0){
    warning(paste("The following datasources are not implemented in occ(): ", datasources[!datasources %in% sources], sep = ""));
    return(NULL);
  }
  else{
    x@occSources <- sources;
  }

  #Check "limit" input.
  if (!class(limit)=="numeric"){
    warning("Input limit is not of class 'numeric'. Limit value must be class 'numeric'.\n");
    return(NULL);
  }
  else{
    x@occNLimit <- limit;
  }

  #Get time stamp for search
  x@occurrenceSearchDate <- as.character(Sys.Date(), format = "%d %B, %Y");

  #Occurrence queries for each species
  occurrenceData <- x;
  occurrenceResults <- occ(query = unlist(queryResults@cleanedTaxonomy$`Best Match`), from = "gbif", limit = limit, has_coords = T);

  return(occurrenceData);
}
