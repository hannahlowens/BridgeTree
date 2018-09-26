library(rgbif)

#' @title Occurrence Citations
#'
#' @description Harvests citations from GBIF for occurrence data
#'
#' @param x An object of class \code{\link{bridgeTreeData}}
#'
#' @return A vector with citations for occurrences
#'
#' @examples
#' myCitations <- occCitation(x = myBridgeTreeObject);
#'
#' @export

occCitation <-function(x = NULL){
  #Error check input x.
  if (!class(x)=="bridgeTreeData"){
    warning("Input x is not of class 'bridgeTreeData'. Input x must be result of a studyTaxonList() search.\n");
    return(NULL);
  }

  #Pull dataset keys from occurrence table
  datasetKeys <- vector(mode = "list");
  for(i in x@occResults){
    datasetKeys <- append(datasetKeys,
                          unlist(as.character(unique(i[[1]]$DatasetKey))));
  }
  datasetKeys <- unique(unlist(datasetKeys))

  #Look up citations on GBIF based on dataset keys
  citationList <- vector(mode = "list");
  for(i in datasetKeys){
    citationList <- append(citationList,
                           rgbif::gbif_citation(i)$citation$citation);
  }

  citationList <- sort(unlist(citationList));

  return(citationList);
}
