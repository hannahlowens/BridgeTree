#' @title Study Taxon List
#'
#' @description Takes input phylogenies or vectors of taxon names, checks against taxonomic database, returns vector for use in spocc queries, as well as warnings if there are invalid names.
#'
#' @param x A phylogeny of class 'phylo' or a vector of class 'character' containing the names of taxa of interest
#'
#' @param datasources A vector of taxonomic datasources implemented in \code{\link{gnr_resolve}}. See \code{\link{http://gni.globalnames.org/} for more information.}
#'
#' @return An object of class \code{\link{BridgeTreeData}} containing the type of inquiry the user has made --a phylogeny or a vector of names-- and a dataframe containing input taxa names, the closeset match according to \code{\link{gnr_resolve}}, and a list of taxonomic datasources that contain the matching name.
#'
#' @examples
#' ## Inputting a phylogeny
#' studyTaxonList(x = phylogeny, datasources = c('NCBI', 'EOL'));
#'
#' ## Inputting a vector of taxon names
#' studyTaxonList(x = c("Buteo buteo", "Buteo buteo hartedi", "Buteo japonicus"), datasources = c('NCBI', 'EOL'));
#'
#' @export

studyTaxonList <- function(x = NULL, datasources = NULL) {
  #Error check inputs (x).
  if (!class(x) == "phylo" & !(is.vector(class(x))&&class(x)=="character")){
    warning("Target input invalid. Input must be of class 'phylo' or a vector of class 'character'.\n");
    return(NULL);
  }
  else if(is.vector(class(x))&&class(x)=="character"){
    targets <- x;
    dataFrom <- "User-supplied list of taxa." #Keeping track of metadata
  }
  else if(class(x) == "phylo"){
    targets <- x$tip.label;
    dataFrom <- "User-supplied phylogeny." #Keeping track of metadata
  }

  #Filling in data sources if none have been specified by the user.
  if (is.null(datasources)){
    datasources <- taxize::gnr_datasources(todf = T)$title
  }

  #Are the user-input databases a vector of class character?
  if (!(is.vector(class(datasources))&&class(datasources)=="character")){
    warning("Datasource input invalid. Input must be a vector of class 'character'.\n");
    return(NULL);
  }

  #Are user-input databases included in the list of data sources for Global Names Resolver?
  sourceList <- taxize::gnr_datasources(todf = T)$title #Populates the list of datasources
  for (db in datasources){
    notInDB <- character()
    if (!(db %in% sourceList)) {
      notInDB <- append(notInDB, db)
    }
  }
  if (length(notInDB) != 0){
    warning(paste("The following sources were not found in Global Names Index source list: ", paste(notInDB, collapse = ', '), sep=""));
  }
  #Remove invalid sources from datasources
  datasources <- datasources[!datasources %in% notInDB];

  #Populating vector of data sources if no valid sources are supplied
  if (length(datasources) == 0){
    warning("No valid taxonomic data sources supplied. Populating default list from all available sources.");
    datasources = taxize::gnr_datasources()$title;
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
  for (name in targets){
    temp <- taxize::gnr_resolve(names = name, data_source_ids = sourceIDs);
    if (length(temp) == 0){
      bestNameMatch <- append(bestNameMatch, "No match");
      taxonomicDatabaseMatches <- append(taxonomicDatabaseMatches, NA);
      warning(paste(name, " is not found in any of the taxonomic data sources specified.", sep = ""));
    }
    else {
      bestMatch <- temp[order(temp$score),]$matched_name[1];
      bestNameMatch <- append(bestNameMatch, bestMatch);
      matchingSources <- temp$data_source_title[temp$matched_name == bestMatch];
      taxonomicDatabaseMatches[[length(bestNameMatch)]] <- paste(matchingSources, collapse = "; ");
    }
  }

  #Building the results table
  resolvedNames <- cbind(targets, bestNameMatch, taxonomicDatabaseMatches);
  colnames(resolvedNames) <- c("Input Name", "Best Match", "Taxonomic Databases w/ Matches");
  resolvedNames <- as.data.frame(resolvedNames);

  #Populating an instance of class bridgeTreeData
  bridgeTreeInstance <- new("bridgeTreeData", userQueryType = dataFrom, taxonomicSources = datasources, cleanedTaxonomy = resolvedNames);
  return(bridgeTreeInstance);
}
