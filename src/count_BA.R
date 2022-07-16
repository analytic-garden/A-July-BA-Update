#' count_BA
#' collapse and count BA.1-5 entries in metafile.
#' Optionally make a barplot of counts.
#'            
#' @param meta_data - a dataframe or tibble from GISAID metafile.
#' @param host - character, host species
#' @param country - character, country of origin, if NULL, all countries.
#' @param plot - logical, should a plot be drawn.
#' @param log - logical, plot log(counts)?
#' @param title - character, optional plot title
#'
#' @return a data frame with columns Collection.date, Pango_lineage, and Count.
#' 
count_BA <- function(meta_data,
                     host = 'Human', 
                     country = 'USA',
                     plot = TRUE,
                     log = FALSE,
                     title = NULL) {
  require(tidyverse)
  
  ################### Helper Function ###################
  #' select_BA - select rows containing Pango.lineage starting with variant
  #'
  #' @param df - dataframe, result of filtering by host and BA lineage
  #' @param variant - character, BA.x where right now x is 1:5
  #'
  #' @return a dataframe of selected variants with columns Collection.date, Pango_lineage, and Count 
  #' 
  select_BA <- function(df, variant) {
    df2 <- df %>% 
      filter(str_detect(Pango.lineage, paste('^', variant, sep = ''))) %>% 
      mutate(Pango_lineage = {{ variant }}) %>%
      select(Collection.date, Pango_lineage) %>% 
      group_by(Collection.date, Pango_lineage) %>% 
      count() %>%
      rename(Count = n) %>%
      select(Collection.date, Pango_lineage, Count) %>%
      drop_na() %>%
      ungroup()
    
    return(df2)
  }
  
  ###################
  
  # filter BA lineages and get country from Location.
  df_BA <- meta_data %>% 
    filter(Host == {{ host }}) %>%
    filter(str_detect(Pango.lineage, '^BA')) %>%
    select(Collection.date, Location, Pango.lineage) %>% 
    separate(Location, c("Region", "Country", "State"), sep = "\\s*/\\s*", extra = "drop", fill = "right") %>%
    select(-c("Region", "State")) %>%
    mutate(Collection.date = as.Date(Collection.date, format = "%Y-%m-%d"))

  if(! is.null(country)) {
    df_BA <- df_BA %>%
      filter(Country == {{ country }})
  }
  
  # get individual BA types
  df_BA_1 <- select_BA(df_BA, 'BA.1')
  df_BA_2 <- select_BA(df_BA, 'BA.2')
  df_BA_3 <- select_BA(df_BA, 'BA.3')
  df_BA_4 <- select_BA(df_BA, 'BA.4')
  df_BA_5 <- select_BA(df_BA, 'BA.5')
  
  df <- bind_rows(df_BA_1, df_BA_2, df_BA_3, df_BA_4, df_BA_5)
  
  if(plot) {
    p <- ggplot(df)
    if(log) 
      p <- p + geom_bar(aes(x = Collection.date, y = log(Count), color = Pango_lineage, fill = Pango_lineage),
                        stat = "identity")
    else
      p <- p + geom_bar(aes(x = Collection.date, y = Count, color = Pango_lineage, fill = Pango_lineage),
                        stat = "identity")
    if(! is.null(title)) {
      p <- p + ggtitle(title)
    }
    
    print(p)
  }
  
  return(df)
}
