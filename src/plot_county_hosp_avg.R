#' plot_county_hosp_avg - plot moving average of COVID-19 hospitalizations
#'
#' @param df - a dataframe from https://health.data.ny.gov/api/views/jw46-jpb7/rows.csv?accessType=DOWNLOAD
#'             if Null, data is downloaded from url.
#' @param counties - a list of county names. Capital District counties are default.
#' @param lab - number of days for moving average. Default is 7
#' @param date - an option date string. If NULL, today's date is used. 
#'
#' @return a data frame with columns
#'    date
#'    Hospitalized - daily COVID-19 hospitalizations
#'    county - current county
#'    avg - moving average
#'    
#' @requires
#' counties must be in NY or an error is thrown.

plot_county_hosp_avg <- function(df = NULL,
                             counties = c('Albany', 'Columbia', 'Rensselaer', 'Saratoga', 'Schenectady'),
                             lag = 7,
                             date = NULL) {
  require(tidyverse)
  require(grid)
  require(gridExtra)
  require(zoo)
  require(rlist)
  
  if(is.null(date)) {
    date = Sys.Date()
  }
  
  state <- 'New York'

  if(is.null(df)) {
    url <- 'https://health.data.ny.gov/api/views/jw46-jpb7/rows.csv?accessType=DOWNLOAD'
    df <- read_csv(url)
  }
  
  df <- df %>% 
    filter(`Facility County` %in% toupper({{ counties }}))
  df$'As of Date' <- as.Date(df$'As of Date', format = '%m/%d/%Y')
  
  df2 <- data.frame()
  plot_list <- list()
  for(county in counties) {
    # calculate moving average
    temp <- df %>% 
      filter(`Facility County` == toupper({{ county }})) %>%
      group_by(`As of Date`) %>% 
      summarise(Hospitalized = sum(`Patients Currently Hospitalized`)) %>%
      rename(Date = `As of Date`) %>%
      mutate(county = {{ county }}) %>%
      mutate(avg = rollapply(Hospitalized, lag, mean, align='right', fill = NA)) %>%
      mutate(avg = ifelse(avg >= 0, avg, 0))
    
    p <- ggplot(temp, aes(x = Date, y = avg)) + 
      geom_line() +
      labs(title = paste('Hospitalizations', lag, 'Day Moving Average ', county, 'County'),
           y = 'Hospitalizations',
           x = 'Date')
    plot_list <- list.append(plot_list, p)
    
    df2 <- bind_rows(df2, temp)
  }
  
  # plot individual counties
  grid.arrange(grobs = plot_list, ncol=2, top = textGrob(date))
  
  # plot all counties on one plot
  p2 <- ggplot(df2) +
    geom_line(aes(x = Date, y = avg, color = county)) +
    ggtitle(paste('Hospitalizations', lag, 'Day Moving Average ', date))
  print(p2)
  
  return(df2)
}