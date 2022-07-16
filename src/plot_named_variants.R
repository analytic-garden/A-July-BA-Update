#' plot_named_variants
#' Draw plots of daily counts and percentage of counts for variants of interest.
#'
#' @param meta_table   dataframe or tibble, GISAID variant table read by read.csv.
#' @param host         character, String indicating host species.
#' @param country      character, country of interest. If NULL, all countries are included.
#' @param plot_counts  logical, should daily counts be plotted?
#' @param plot_percent logical, should daily percent counts be plotted?
#' @param log          logical, should counts be plotted on log scale? Ignored if plot_counts = FALSE.
#' @param title        character, an optional title string for plots.
#'
#' @return A data frame containing columns: Collection.date, Variant, Count, Total, and Pct
#' 
plot_named_variants <- function(meta_table, 
                                 host = "Human", 
                                 country = "USA", 
                                 plot_counts = TRUE,
                                 plot_percent = TRUE,
                                 log = FALSE, 
                                 title = NULL) {
  require(tidyverse)
  require(pals)

###################################### Helper Functions ####################################  
  
#' plot_pct
#' Plot daily percent of each variant vs date.
#' 
#' @param dfp A dataframe, see below.
#'
#' @return NULL
  plot_pct <- function(dfp) {
    p <- ggplot(dfp) + 
      geom_bar(aes(x = Collection.date, y = Total_pct, color = Variant, fill = Variant),
               stat = "identity") +
      ylab('Percent of total counts') 

      # geom_bar(aes(x = Collection.date, y = Pct, color = Variant, fill = Variant),
      #         stat = "identity")
      # geom_point(aes(x = Collection.date, y = Pct, color = Variant))

    if(! is.null(title)) {
      p <- p + ggtitle(paste(title, "Percent of Daily Counts", sep = " "))
    }
    
    print(p)
    
    return(NULL)
  }
  
#' plot_cnts
#' Plot daily counts of the variants.
#'
#' @param dfp A data frame, see below
#'
#' @return NULL
  plot_cnts <- function(dfp) {
    p <- ggplot(dfp) 
    if(! log) {
      p <- p +
        geom_bar(aes(x = Collection.date, y = Count, color = Variant, fill = Variant),
                 stat = "identity")
        # geom_point(aes(x = Collection.date, y = Count, color = Variant))
    } else {
      p <- p + 
        geom_bar(aes(x = Collection.date, y = log(Count), color = Variant, fill = Variant),
                 stat = "identity")
      # geom_point(aes(x = Collection.date, y = log(Count), color = Variant)) 
    }
    
    if(! is.null(title)) {
      p <- p + ggtitle(title)
    }
    
    print(p)
    
    return(NULL)
  }
  
###########################################################################################
  
  # make sure Collection.data is a date
  meta_table <- meta_table %>%
    mutate(Collection.date = as.Date(Collection.date, format = "%Y-%m-%d")) %>% 
    filter(Collection.date >= as.Date("2019-01-01"))
    
  # Filter by host. Remove invalid dates and empty lineages.
  # Reformat Location into Country
  df <- meta_table %>% 
    filter(Host == {{ host }}) %>%
    # filter(Variant != "") %>%
    filter(str_detect(Variant, '^VOC')) %>%
    # mutate(Variant = ifelse(Variant == "", "Wuhan", Variant)) %>%
    select(Collection.date, Location, Variant) %>% 
    separate(Location, c("Region", "Country", "State"), sep = "\\s*/\\s*", extra = "drop", fill = "right") %>%
    select(-c("Region", "State")) %>%
    separate(Variant, c("V", "Name"), extra = "drop", fill = "right") %>%
    unite(Variant, V:Name, sep = " ", remove = TRUE) %>%
    mutate(Variant = as.factor(Variant))
  
  # Filter by country and lineages
  # Count by date and lineage
  df2 <- df
  if(! is.null(country)) {
    df2 <- df2 %>%
      filter(Country == {{ country }})
  }
  
  # get overall counts
  df_count <- df2 %>% 
    group_by(Collection.date) %>% 
    count() %>% 
    rename(Total_counts = n) 
  
  df2 <- df2 %>% 
    group_by(Collection.date, Variant) %>%
    count() %>%
    rename(Count = n)
  
  # get totals by date and lineage
  dfp2 <- 
    df2 %>% 
    group_by(Collection.date) %>% 
    summarise(Total = sum(Count)) 
  # try using df_counts to get overall pct
  
  dfp3 <- data.frame(Collection.date = as.Date(integer()),
                     Pango.lineage = character(),
                     Count = integer(),
                     Total = integer(),
                     Pct = double(),
                     Total_counts = integer(),
                     Total_pct = double())
  
  # construct a data frame of the counts of lineages and their percents
  for(var in unique(df2$Variant)) {
    dfp4 <- 
      df2 %>% 
      filter(Variant == {{ var }}) %>% 
      full_join(dfp2) %>% 
      mutate(Pct = Count / Total) %>% 
      drop_na()
    
    dfp4 <- dfp4 %>%
      inner_join(df_count) %>%
      mutate(Total_pct = Count / Total_counts) %>% 
      drop_na()
    
    dfp3 <- rbind(dfp3, dfp4)
  }
  
  dfp3 <- ungroup(dfp3)
  
  if(plot_percent) {
    plot_pct(dfp3)
  }
  
  if(plot_counts) {
    plot_cnts(df2)  
  }
  
  return(dfp3)
}