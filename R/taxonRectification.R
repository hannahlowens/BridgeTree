#' @title Taxon Rectification
#'
#' @description An internal helper function that takes an input taxonomic name, checks against taxonomic database, returns vector for use in database queries, as well as warnings if the name is invalid.
#'
#' @param taxName A string that, ideally, is a taxonomic name
#'
#' @param datasources A vector of taxonomic datasources implemented in \code{\link{gnr_resolve}}. See \code{\link{http://gni.globalnames.org/} for more information.}
#'
#' @return A string with the closeset match according to \code{\link{gnr_resolve}}, and a list of taxonomic datasources that contain the matching name.
#'
#' @examples
#' #Inputting a taxonomic name and specifying what taxonomic sources you want to search
#' studyTaxonList(x = "Buteo buteo hartedi", datasources = c('NCBI', 'EOL'));
#'
#' @export

taxonRectification <- function(taxName = NULL, datasources = NULL) {
  #Filling in data sources if none have been specified by the function calling this helper.
  if (is.null(datasources)){
    datasources <- taxize::gnr_datasources(todf = T)$title
  }

  #Resolving the user-input taxonomic names
  sources <- taxize::gnr_datasources();
  sourceIDs <- sources$id[sources$title %in% datasources]
  #Protects against an error thrown when giving gnr_resolve a complete list of data sources
  if (nrow(sources) == length(sourceIDs)){
    sourceIDs <- NULL;
  }
  bestNameMatch <- character();
  taxonomicDatabaseMatches <- vector("list");
  temp <- taxize::gnr_resolve(names = taxName, data_source_ids = sourceIDs);
  if (length(temp) == 0){
    bestNameMatch <- append(bestNameMatch, "No match");
    taxonomicDatabaseMatches <- append(taxonomicDatabaseMatches, NA);
    warning(paste(taxName, " is not found in any of the taxonomic data sources specified.", sep = ""));
    }
  else {
    bestMatch <- temp[order(temp$score),]$matched_name[1];
    bestNameMatch <- append(bestNameMatch, bestMatch);
    matchingSources <- temp$data_source_title[temp$matched_name == bestMatch];
    taxonomicDatabaseMatches[[length(bestNameMatch)]] <- paste(matchingSources, collapse = "; ");
  }

  #Building the results table
  resolvedName <- cbind(taxName, bestNameMatch, taxonomicDatabaseMatches);
  names(resolvedName) <- c("Input Name", "Best Match", "Taxonomic Databases w/ Matches");
  resolvedName <- as.data.frame(resolvedName);
  return(resolvedName);
}
