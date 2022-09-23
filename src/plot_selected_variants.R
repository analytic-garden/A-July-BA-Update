#' plot_selected_variants
#'    Barplot of selected SARS-CoV-2 variants
#'
#' @param metadata - dataframe or tibble, GISAID metatable.
#' @param selected_variants - a vector of Pango lineage strings.
#' @param host - character, String indicating host species.
#' @param country - character, country of interest. If NULL, all countries are included.
#' @param title - character, optional plot title
#'
#' @return - a dataframe
#'    containing the columns
#'    Collection.date, Pango.lineage, Count
#' 
plot_selected_variants <- function(metadata,
                                   selected_variants,
                                   host = 'Human', 
                                   country = 'USA',
                                   title = NULL) {
  require(tidyverse)
  
  # filter by host, lineage, and location
  df_BA <- metadata %>% 
    filter(Host == {{ host }}) %>%
    select(Collection.date, Location, Pango.lineage) %>% 
    separate(Location, c("Region", "Country", "State"), sep = "\\s*/\\s*", extra = "drop", fill = "right") %>%
    select(-c("Region", "State")) %>%
    mutate(Collection.date = as.Date(Collection.date, format = '%Y-%m-%d'))
  
  if(! is.null(country)) {
      df_BA <- df_BA %>%
      filter(Country == {{ country }})
  }
  
  # select lineages and count them
  df_BA <- df_BA %>%
    filter(Pango.lineage %in% {{ selected_variants }}) %>% 
    group_by(Collection.date, Pango.lineage) %>% 
    count() %>% 
    rename(Count = n) %>% 
    drop_na()
  
  p <- ggplot(df_BA) + 
    geom_bar(aes(x = Collection.date, y = Count, color = Pango.lineage), fill = 'white', stat = 'identity') 
  if(! is.null(title))
    p <- p + ggtitle(title)
  print(p)

  return(df_BA)
}