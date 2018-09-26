library(BIEN)

#' @title Download occurrence points from BIEN
#'
#' @description Downloads occurrence points and useful related information for processing within other bridgetree functions
#'
#' @param taxon A single plant species or vector of plant species
#'
#' @return A list containing (1) a dataframe of occurrence data; (2) a list containing: i notes on usage, ii bibtex citations, and iii aknowledgement information.
#'
#' @examples
#' getBIENpoints(taxon="Acer rubrum");
#'
#' @export
getBIENpoints<-function(taxon){
  occs<-BIEN::BIEN_occurrence_species(species = taxon,cultivated = T,
                                  only.new.world = F, native.status = T,
                                  collection.info = T,natives.only = F);

  occs<-occs[which(!is.na(occs$latitude) & !is.na(occs$longitude)),];

  #Fixing dates
  occs <-occs[which(!is.na(occs$date_collected)),];
  occs$date_collected <- as.Date(occs$date_collected);
  yearCollected <- as.numeric(format(occs$date_collected, format = "%Y"))
  monthCollected <- as.numeric(format(occs$date_collected, format = "%m"))
  dayCollected <- as.numeric(format(occs$date_collected, format = "%d"))
  occs <- cbind(occs, dayCollected, monthCollected, yearCollected)

  #Get data sources
  datasources<-unique(occs$datasource_id[!is.na(occs$datasource_id)]);
  query<-paste("WITH a AS (SELECT * FROM datasource where datasource_id in (",
              paste(shQuote(datasources, type = "sh"),collapse = ', '),"))
             SELECT * FROM datasource where datasource_id in (SELECT proximate_provider_datasource_id FROM a) OR datasource_id in (SELECT datasource_id FROM a);");
  sources <- BIEN:::.BIEN_sql(query);

  suppressWarnings(occs<-merge(x = occs,y = sources,by.x = "datasource_id",
                               by.y = "datasource_id"));

  outdata<-occs[c('scrubbed_species_binomial',
                  'longitude','latitude','dayCollected', 'monthCollected',
                  'yearCollected', 'datasource','dataset','datasource_id')];
  dataService <- rep("BIEN", nrow(outdata));
  outdata <- cbind(outdata, dataService);
  outdata$license<-"CC BY-NC-ND";
  outdata$database<-"BIEN";

  #Merge sources with occs
  citations <- BIEN::BIEN_metadata_citation(occs);

  outlist<-list();
  outlist[[1]]<-outdata;
  outlist[[2]]<-citations;

  return(outlist);
}
