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
#' @return pointsList A list containing results from each species' \code{\link{occ}} search, including metadata.
#'
#' @examples
#' ## PLACEHOLDER
#' studyTaxonList(x = phylogeny, datasources = c('NCBI', 'EOL'));
#'
#' ## PLACEHOLDER
#' studyTaxonList(x = c("Buteo buteo", "Buteo buteo hartedi", "Buteo japonicus"), datasources = c('NCBI', 'EOL'));
#'

taxaQuery <- function(x = NULL, datasources = "gbif", limit = 500, options = NULL) {
  #Error check input x.
  if (!class(x) == "data.frame"){
    warning("Input x is not of class 'data.frame'. Input x must be result of a studyTaxonList() search.\n");
    return(NULL);
  }

  #Error check input datasources.
  if (!is.vector(datasources) && class(x)=="character"){
    warning("Input datasources is not of class 'data.frame'. Datasources object must be a vector of class 'character'.\n");
    return(NULL);
  }
  #Check to see if the sources input are actually ones used by occ()
  sources <- c("gbif", "bison", "obis", "ala", "inat", "idigbio", "ebird", "ecoengine", "vertnet"); #occ() sources
  if(sum(!datasources %in% sources) > 0){
    warning(paste("The following datasources are not implemented in occ(): ", datasources[!datasources %in% sources], sep = ""));
    return(NULL);
  }

  #Return a thing
  occurrenceData <- "It worked!";
  return(occurrenceData);
}
