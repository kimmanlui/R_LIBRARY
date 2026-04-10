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

loadImage <- function(imageFile = "./RDATA/2025newStudy.RData") {
    if (!file.exists(imageFile)) {
    message("File not exists")
    return(invisible(NULL))
    }
    loaded <- load(imageFile, envir = .GlobalEnv) # return names loaded
    file_info <- file.info(imageFile)
    mod_time <- file_info$mtime
    if ("hsi_data" %in% loaded && exists("hsi_data", envir = .GlobalEnv)) {
    n <- nrow(get("hsi_data", envir = .GlobalEnv))
    } else {
    n <- NA
    }
    message(sprintf("Modified time: %s, Nrow for hsi_data: %s", mod_time, n))
    invisible(loaded)
}

