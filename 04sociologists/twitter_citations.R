
###--- Libraries
library(scholar)
library(rtweet)
library(tidyverse)
library(httr)
library(jsonlite)
library(ggpmisc)
source("helper_functions.R")
#auth_as("create_token") # Authentica with rtweet

###--- Files and paths
output_twitter <- "04sociologists/sociologists_twitter.RDS"
output_scholar <- "04sociologists/sociologists_scholar.RDS"


###--- TWITTER 
  ###--- Get lists subscribed to (familyunequal)
  list <- rtweet::lists_users(user = "familyunequal")
  list <- list[str_which(list$name,"Sociologists"),]
  list_members <- rtweet::lists_members(list$list_id)
  
  ###--- Regexes to clean up names
  remove <- c("PhD") # Common words to remove
  regex1 <- "\\w+\\."  # Match any word that ends with a period
  regex2 <- "\\w{3,}+ (\\w|-|'){1,}+" # Extract names
  
  ###--- Apply regexes
  names <- list_members$name
  names <- str_remove(names, "PhD")
  names <- str_remove_all(names,regex1)
  names <- str_remove(names,"[[:punct:]]")
  names <- str_extract_all(names,"(\\w|-|'){2,}+")

  ###--- Put data together and save
  names <- as_tibble(plyr::ldply(names, rbind))
  names <- 
    names %>% 
    rename_with( ~ paste0("name_", .x)) %>% 
    mutate(name = list_members$name)
  
  names <- 
    names %>% 
    left_join(list_members)
  
  #saveRDS(names,file = output_twitter)
  

###--- GOOGLE SCHOLAR
  rm(list = ls())
  gc()
  output_twitter <- "04sociologists/sociologists_twitter.RDS"
  output_scholar <- "04sociologists/sociologists_scholar.RDS"
  twitter_data <- readRDS(output_twitter)
  
  ###--- API keys
  #my_key <- "5840e4fbb17dffa1a1f444b675848d46c0eef1f8a3edd5a342aadf3a3358005d"
  other_key <- "5840e4fbb17dffa1a1f444b675848d46c0eef1f8a3edd5a342aadf3a3358005d"
  
  scholar_res <- list() 
  
  for(i in 1:nrow(twitter_data[1:100,])){
    
    first_name <- twitter_data[i,"name_1"]
    second_name <- twitter_data[i,"name_2"]
    field <- "Sociology"
    
    scholar_res[[i]] <- lookup_scholar_profile2(name = first_name,
                                  family_name = second_name,
                                  field = field,
                                  key = other_key)
    
    if(i %% 10 == 0){print(i)}
    
  }
 

  names(scholar_res) <- twitter_data[1:100,]$name
  res <- bind_rows(scholar_res,.id = "twitter_name")
  #saveRDS(res,output_scholar)

###--- Plot  data
  rm(list = ls())
  gc()
  output_twitter <- "04sociologists/sociologists_twitter.RDS"
  output_scholar <- "04sociologists/sociologists_scholar.RDS"
  twitter_data <- readRDS(output_twitter)
  scholar_data <- readRDS(output_scholar)

  twitter_data <- 
    twitter_data %>% 
    select(followers = followers_count,name)
  
  scholar_data <- 
    scholar_data %>% 
    select(name,citations = cited_by)
  
  tbl <- 
    twitter_data %>% 
    left_join(scholar_data) %>% 
    drop_na() 
  

  tbl %>% 
    ggplot(aes(citations,followers)) +
    geom_point() +
    stat_poly_line() +
    stat_poly_eq() +
    theme_minimal() 
    
