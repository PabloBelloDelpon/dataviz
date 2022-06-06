####--- Query the google scholar profiles API

lookup_scholar_profile <- function(name,family_name,field,key){
  field_label <- paste0("label:",field)
  
  ###--- Query the API
  query <- 
    GET(url = "https://serpapi.com/search.json?",
        query = list(
          engine = "google_scholar_profiles",
          mauthors = paste(name,family_name,field_label),
          api_key = key))
  
  
  ###--- Extract data
  data <- fromJSON(rawToChar(query$content))
  data$search_parameters
  data <- as_tibble(data$profiles)
  data <- data[1,] # Keep only first result
}



#################################
###--- Some sociologists don't have sociology as field but instead specific
# sub-fields like economic sociology. So don't directly query sociology,
# instead look through "interests" to find anything with the word
# sociology on it. 

lookup_scholar_profile2 <- function(name,family_name,field,key){

  ###--- Query the API
  query <- 
    GET(url = "https://serpapi.com/search.json?",
        query = list(
          engine = "google_scholar_profiles",
          mauthors = paste(name,family_name),
          api_key = key))
  
  
  ###--- Extract data
  data <- fromJSON(rawToChar(query$content))
  data$search_parameters
  data <- as_tibble(data$profiles)
  
  if(nrow(data) > 0 & "interests" %in% colnames(data) == TRUE){
    ###--- Look for "sociology" in interests
    soc_labels <- apply(data,1,function(x){
      
      
      labels <- unlist(x$interests)
      labels <- labels[str_which(names(labels),"title")]
      labels <- labels[str_which(labels,regex(field,ignore_case = TRUE))]
      labels <- paste(labels,collapse = ", ")
    }) 
    
    data <- 
      data %>% 
      bind_cols(soc_labels = soc_labels) %>% 
      mutate(rank = row_number()) %>% 
      filter(nchar(soc_labels) > 0)  # Filter sociologists
    
    if(nrow(data) > 0){
      data <- data %>% filter(rank == max(rank)) # Filter highest ranked results 
    }
  }
 return(data)
}
