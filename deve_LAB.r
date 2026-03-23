library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gridExtra)
library(grid)
library(tibble)
library('skimr')
#library(DataExplorer)
library(png) 

startlabtime=Sys.time()
print(startlabtime)

showBasicInfo=function()
{
    cat(paste0("\033[91m", Sys.info()["nodename"], "\033[0m\n"))
  
    options(repr.plot.width=8, repr.plot.height=12)
    cat(paste0("Working Directory: ", getwd(), '  \nR Verison:',R.version.string))
    cat("\nRemark: Use R.4.3.3 for FUJITSU\n")
}