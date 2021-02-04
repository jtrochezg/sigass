# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("googlesheets4", "googleAuthR","googledrive","RGoogleAnalytics",
                "shiny","nlme","Hmisc","blme","cluster","knitr","kml","ggplot2","dplyr",
                "rmarkdown","agricolae","lubridate","plotly","shinythemes","shinyjs")


install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))