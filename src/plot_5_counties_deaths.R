#' plot_5_counties_deaths
#'    Plot moving average of county daily deaths.
#'
#' @param df - a dataframe from https://health.data.ny.gov/api/views/jw46-jpb7/rows.csv?accessType=DOWNLOAD
#'             if Null, data is downloaded from url.
#' @param counties - a list of county names. Capital District counties are default.
#' @param lag 
#'
#' @return
#'     A data frame with columns:
#'     countyFIPS - FIPS location of county
#'     County Name
#'     State
#'     StateFIPS - FIPS location of state
#'     Date - As of this date
#'     Deaths - number of reported deaths
#' 
plot_5_counties_deaths <- function(df = NULL,
                                   counties = c('Albany', 'Columbia', 'Rensselaer', 'Saratoga', 'Schenectady'),
                                   lag = 7) {
  require(tidyverse)
  require(rlist)
  require(grid)
  require(gridExtra)
  require(zoo)
  
  counties = c('Albany County', 'Columbia County', 'Rensselaer County', 'Saratoga County', 'Schenectady County')

  df <- read_csv('https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv')
  df2 <- df %>%
    pivot_longer(cols = contains('-'), names_to = 'Date', values_to = 'Deaths')
  df2$Date <- as.Date(df2$Date, format = '%Y-%m-%d')
  
  plot_list <- list()
  for(county in counties) {
    df3 <- df2 %>%
      filter(State == 'NY' & `County Name` == county) %>%     # backticks around County Name
      mutate(Daily = pmax(0, Deaths - lag(Deaths))) %>% 
      mutate(Avg = rollapply(Daily, lag, mean, align='right', fill = NA))
    
    p <- ggplot(df3, aes(x=Date, y=Avg)) + 
      geom_line() +
      labs(title = paste('Deaths ', lag, '-day Moving Average ', county, sep = ''),
           y = 'Deaths')
    
    plot_list <- list.append(plot_list, p)
  }
  
  grid.arrange(grobs = plot_list, ncol=2, top = textGrob(Sys.Date()))
  
  return(df2)
}