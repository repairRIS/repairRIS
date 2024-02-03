#' Repair RIS abstracts using Open Alex search on DOIs
#' 
#' @description Takes an RIS file and repairs the abstracts 
#' by performing a DOI search of Open Alex. 
#' @param input A file path to an RIS file.
#' @param file_name The name of the input file
#' @param path The destination path for the repaired file
#' @return An RIS formatted text file.
#' @export
#' @examples 
#' \dontrun{
#' file <- file.choose() #select the RIS file to repair
#' new_ris <- repair_ris(file)
#' }
repair_ris <- function(input,
                       file_name = 'export.RIS',
                       path = ''){
  
  results <- search_openAlex(input) #search OA for dois
  print(paste0(length(results), ' potential matches were found on OpenAlex'))
  
  #reconstruct abstracts
  abstracts <- data.frame(doi = NULL, abstract = NULL) #reconstruct abstracts
  for (i in 1:length(results)){
    tryCatch({
      ab_new <- reconstruct_abstract(results[[i]])
      doi_new <- names(results[i])
      row <- data.frame(doi = doi_new, abstract = ab_new)
    }, error=function(e){ab_new <<- NA})
    abstracts <- rbind(abstracts, row)
  }
  abstracts <- abstracts[!duplicated(abstracts),] #remove duplicate entries
  print(paste0(nrow(abstracts), ' of these records possessed abstracts'))
  
  #replace all abstracts with those from Open Alex
  output <- merge(input, abstracts, by.x = "doi", 
                  by.y = "doi", all.x = TRUE, all.y = FALSE)
  output <- output %>% #merge two abstract columns, replacing NAs with partial abstracts
    mutate(abstract.y = coalesce(abstract.y, abstract.x))
  output$abstract.x <- output$abstract.y #replace original partial abstract column with new data
  names(output)[names(output) == 'abstract.x'] <- 'abstract' #rename
  output <- subset(output, select = -c(abstract.y)) #remove merged abstract column
  print(paste0('Your data have been repaired to include these ', nrow(abstracts), ' complete abstracts'))
  
  #rebuild RIS file
  ris <- build_ris(output, save=FALSE)
  print('Your RIS file has been built')
  
  #save reparied RIS file to source location
  write.table(ris, file = paste0(path, file_name, '_REPAIRED', '.ris'), row.names = FALSE, col.names = FALSE)
  print('Your RIS file has been saved to the file location as: [filename]_REPAIRED.ris')
  
  return(ris)
  
}
