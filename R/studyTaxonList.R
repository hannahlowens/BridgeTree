#' @title Study Taxon Vector
#'
#' @description Takes input phylogenies or vectors of taxon names, checks against taxonomic database, returns vector for use in spocc queries, as well as warnings if there are invalid names.
#'
#' @param x
#'
#' @return nameVector
#'
#' @examples
#' ## Inputting a phylogeny
#' studyTaxonList(x = phylogeny);
#'
#' ## Inputting a vector of taxon names
#' studyTaxonList(x = c("Buteo buteo", "Buteo buteo hartedi", "Buteo japonicus"));
#'

studyTaxonList <- function(x) {
  #Error check inputs.
  if (!class(x) == "phylo" & !(is.vector(class(x))&&class(x)=="character")){
    warning("Target input invalid. Input must be of class 'phylo' or a vector of class 'character'.\n\n");
    return(NULL);
  }
  else if(is.vector(class(x))&&class(x)=="character"){
    targets <- x;
  }
  else if(class(x) == "phylo"){
    targets <- x$tip.label;
  }
  return(print("And I can get to the next step."));
}

#input tree/vector
#is it a tree?
#is it a vector with characters?

#Error: inappropriate data type

#is it a valid name?

#Return list of names searchable with spocc()
#  Print warning RE: invalid names (How to check?)
