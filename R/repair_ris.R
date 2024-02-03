library(synthesisr)
library(dplyr)
source('search_openAlex.R')
source('build_ris.R')

file <- file.choose() #select the RIS file to repair

file_name <- sub('.*\\/', '', file) #set the file name
path <- gsub(file_name, '', file) #set the file path

input <- read_refs(file) #read in the RIS file as a df

dois <- input$doi #set the doi column
doisLIST <- data.frame(ids=dois, type=rep('doi', length(dois))) #create an input dataframe with identifier type
doisLIST <- na.omit(doisLIST) #remove NAs

results <- search_openAlex(doisLIST) #search OA for dois

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

#replace all abstracts with those from Open Alex
output <- merge(input, abstracts, by.x = "doi", 
      by.y = "doi", all.x = TRUE, all.y = FALSE)
output <- output %>% #merge two abstract columns, replacing NAs with partial abstracts
  mutate(abstract.y = coalesce(abstract.y, abstract.x))
output$abstract.x <- output$abstract.y #replace original partial abstract column with new data
names(output)[names(output) == 'abstract.x'] <- 'abstract' #rename
output <- subset(output, select = -c(abstract.y)) #remove merged abstract column

#rebuild RIS file
ris <- build_ris(output, save=FALSE)
write.table(ris, file = paste0(path, file_name, '_REPAIRED', '.ris'), row.names = FALSE, col.names = FALSE)

