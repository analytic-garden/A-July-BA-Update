#' plot_county_hosp - plot case daily hospitalizations for selected counties in NY
#'
#' @param df - a dataframe from https://health.data.ny.gov/api/views/jw46-jpb7/rows.csv?accessType=DOWNLOAD
#'             if Null, data is downloaded from url.
#' @param counties - a list of county names. Capital District counties are default.
#' @param date - an option date string. If NULL, today's date is used. 
#'
#' @return a data frame with columns
#'    date
#'    county - selected counties
#'    daily - number hospitalized
#'    
#' @requires
#' counties must be in NY or an error is thrown.

plot_county_hosp <- function(df = NULL,
                             counties = c('Albany', 'Columbia', 'Rensselaer', 'Saratoga', 'Schenectady'),
                             date = NULL) {
  require(tidyverse)
  require(grid)
  require(gridExtra)
  require(zoo)
  require(rlist)
  
  if(is.null(date)) {
    date = Sys.Date()
  }
  
  if(is.null(df)) {
    url <- 'https://health.data.ny.gov/api/views/jw46-jpb7/rows.csv?accessType=DOWNLOAD'
    df <- read_csv(url)
  }
  
  df <- df %>% 
    filter(`Facility County` %in% toupper({{ counties }}))
  df$'As of Date' <- as.Date(df$'As of Date', format = '%m/%d/%Y')
  
  df2 <- data.frame(Date = NULL,
                    county = NULL,
                    daily = NULL)

  plot_list <- list()
  for(county in counties) {
    temp <- df %>% 
      filter(`Facility County` == toupper({{ county }})) %>%
      group_by(`As of Date`) %>% 
      summarise(Hospitalized = sum(`Patients Currently Hospitalized`)) %>%
      rename(Date = `As of Date`) %>%
      mutate(county = {{ county }})
    
    p <- ggplot(temp, aes(x = Date, y = Hospitalized)) + 
      geom_line() +
      labs(title = paste('Daily Hospitalizations', county, 'County'),
           y = 'Hospitalizations',
           x = 'Date')
    plot_list <- list.append(plot_list, p)
    
    df2 <- bind_rows(df2, temp)
  }
  
  grid.arrange(grobs = plot_list, ncol=2, top = textGrob(date))
  
  p2 <- ggplot(df2) +
    geom_line(aes(x = Date, y = Hospitalized, color = county)) +
    ggtitle(paste('Daily Hospitalizations', date))
  print(p2)
  
  return(df2)
}