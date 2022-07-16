#' county_variants
#'    Count variants for a given US state and counties.
#'
#' @param meta_data - a dataframe or tibble from GISAID metafile.
#' @param host - character, host species.
#' @param state - character, US state, default New York
#' @param counties - vector of character, counties within state,
#'                   default 'Albany', 'Columbia', 'Rensselaer', 'Saratoga', 'Schenectady'
#' @param start - character, start date for search in %Y-%m-%d format, default 2022-01-01
#' @param end - character, end date for search in %Y-%m-%d format, default current date
#' @param plot - logical, should a barplot of variant counts be made?, default TRUE
#' @param title - character, optional title for plot
#' @param min_count - integer, ignore lineages with count < min_count for plot, default 10
#'
#' @return dataframe with columns Pango.lineage, Count, County
#' 
#' @requires dates in correct format and start < end
#' 
#' @note invalid states, counties, or host will produce no output, with no warning

county_variants <- function(meta_data,
                            host = 'Human',
                            state = 'New York',
                            counties = c('Albany', 'Columbia', 'Rensselaer', 'Saratoga', 'Schenectady'),
                            start = "2022-01-01",
                            end = NULL,
                            plot = TRUE,
                            title = NULL,
                            min_count = 10) {
  require(tidyverse)
  
  if(is.null(end)) {
    end <- Sys.Date()
  }
  
  # filter date range and state
  df <- meta_data %>%
    filter(Host == {{ host }}) %>%
    filter(Collection.date >= as.Date({{ start }}) & Collection.date <= as.Date({{ end }})) %>%
    filter(str_detect(Location, {{ state }})) %>%
    select(Collection.date, Pango.lineage, Location)

  df_county_variants <- data.frame()    
  for(county in counties) {
    county_name <- paste(county, "County")
    
    # get county data and count Pango lineages for the county.
    # add the county to dataframe
    df_county <- df %>% 
      filter(str_detect(Location, {{ county_name }})) %>% 
      select(Collection.date, Pango.lineage) %>%
      group_by(Pango.lineage) %>%
      count() %>%
      rename(Count = n) %>%
      mutate(County = {{ county}})
    
    df_county_variants <- bind_rows(df_county_variants, df_county)
  }
  
  if(plot) {
    # plot counts > min_count
    p <- df_county_variants %>%
      filter(Count > {{ min_count}}) %>%
      ggplot() + 
        geom_bar(aes(x = Pango.lineage, y = Count, fill = County), stat = 'identity', width = 0.5) +
        labs(caption = paste('Minimum Count =', {{ min_count }}, '\n',
                             start, 'to', end)) +
        xlab('Pango Lineange')
    
    if(! is.null(title)) {
      p <- p + ggtitle(title)
    }
    
    print(p)
  }
  
  return(df_county_variants)
}