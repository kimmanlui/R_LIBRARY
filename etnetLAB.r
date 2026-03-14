library(httr)
library(RCurl)
library(XML)
#library(rvest)
#library(RMySQL)
#library(dbx)
library(stringr)

sleep_until <- function(target_time, ignoreAfter=1) {
  # Get the current time in Hong Kong Time (HKT)
  current_time_hkt <- with_tz(Sys.time(), tzone = "Asia/Hong_Kong")  # Assuming system time is in UTC

    
  # Convert the target time (local time) to a POSIXct object in HKT
  target_time_today <- as.POSIXct(paste(as.Date(current_time_hkt), target_time), tz = "Asia/Hong_Kong")
  
  # If the target time has already passed today, set it for tomorrow
  if (target_time_today < current_time_hkt) {
    if (ignoreAfter==1) 
    {
       return(cat(paste0("The current time is after ",target_time,". No Sleep!!!\n")) )
    }
    target_time_today <- target_time_today + days(1)
  }
  
  # Calculate the sleep duration in seconds
  sleep_duration <- as.numeric(difftime(target_time_today, current_time_hkt, units = "secs"))
 
  # Sleep for the calculated duration

  cat(paste("Sleeping for", sleep_duration, "seconds until", target_time_today, "\n"))
  Sys.sleep(sleep_duration)
  cat("Woke up!\n")
}


gethsi_rt=function()
{
  hsi_rt=getET_ohlc()
  # Convert date to a Date object if it's not already
  hsi_rt$time <- Sys.time()
  hsi_rt$date <- Sys.Date()
  # Add Weekday column (1 for Mon, 2 for Tue, ..., 7 for Sun)
  hsi_rt <- hsi_rt %>%
  mutate(Weekday = as.integer(format(date, "%u")))
  hsi_rt <- hsi_rt %>%
  mutate(bd = round((c - o) / (h - l),2))  # Adjust column names if needed
  hsi_rt <- hsi_rt %>%
  mutate(bp = round( ((c + o)/2 -l)  / (h - l),2))  # Adjust column names if needed
  return(hsi_rt)
}


getET_ohlc=function () 
{
    library(rvest)
    base_url <- "http://www.etnet.com.hk/www/eng/futures/index.php"
    doc <- read_html(base_url)
    futures_quote <- doc %>% html_nodes(".FuturesQuoteBox") %>% 
        html_nodes(".FuturesQuoteContent") %>% .[[3]] %>% html_nodes(".FuturesQuoteOthers") %>% 
        .[[1]] %>% html_text()
    digits <- str_extract_all(futures_quote, "\\d{1,3}(?:,\\d{3})*")[[1]]
    cleaned_text <- gsub("[^0-9,.]", " ", futures_quote)

    hsi_quote  <- doc %>% html_nodes(".FuturesQuoteBox") %>% 
        html_nodes(".FuturesQuoteContent") %>% .[[3]] %>% html_text()
    hsi_digits <- str_extract_all(hsi_quote, "\\d{1,3}(?:,\\d{3})*")[[1]]
    print(hsi_digits[1])
   #  cleaned_hsi_text <- gsub("[^0-9,.]", " ", futures_quote)
   #print(hsi_quote)
    hsi_price = as.numeric(gsub(",", "", hsi_digits[1]))
    digits <- unlist(strsplit(cleaned_text, " "))
    digits <- digits[digits != ""]
    result = as.numeric(gsub(",", "", digits))
    return(data.frame(o = round(result[2]), h = round(result[3]), l = round(result[4]), 
        c = hsi_price))
}



extractRecord=function(rec, db, col_name=c('Weekday', 'bd','bp'), type=c(0,1,1))
{
    if (nrow(rec)>1) stop('rec should have 1 row')
    for (i in 1:length(col_name))
    {
        if (type[i]==1) {
            col_name_A=paste0(col_name[i],'_1')
            col_name_B=paste0(col_name[i],'_2')
            col_name_A_OLD=paste0(col_name[i],'_LOWER')
            col_name_B_OLD=paste0(col_name[i],'_UPPER')
            #print(col_name_A)
            #print(col_name_B)
            #print(col_name_A_OLD)
            #print(col_name_B_OLD)
            #print(names(db))
            
            if ( col_name_A_OLD %in% names(db)) db <- db %>% rename(!!col_name_A := all_of(col_name_A_OLD))
            if ( col_name_B_OLD %in% names(db)) db <- db %>% rename(!!col_name_B := all_of(col_name_B_OLD))
            #print(col_name_B_OLD)
        }
    }
    if ('Day' %in% names(db)) db = db %>%  rename('Weekday' ='Day'  )
    if (!all(col_name %in% names(rec))) {  stop("Some col_name(s) not in rec")}
    for (i in 1:length(col_name))
    {
        if (type[i]==1) {
            col_name_A=paste0(col_name[i],'_1')
            col_name_B=paste0(col_name[i],'_2')
            

             if (!(col_name_A %in% names(db))) { stop("ERROR CHECK col_name_A TYPE=1")}
             if (!(col_name_B %in% names(db))) { stop("ERROR CHECK col_name_B TYPE=1")}
        } else
        {
            if (!(col_name[i] %in% names(db))) { stop("ERROR CHECK TYPE=0")}
        }
    }
    for (i in 1:length(col_name))
    {
        test_cond=''
        if (type[i]==1) {
            col_name_A=paste0(col_name[i],'_1')
            col_name_B=paste0(col_name[i],'_2')
            rec_col=paste0("rec[1,'",col_name[i],"']")
            text_cond=paste0( col_name_A, " <= ",rec_col," & ", rec_col, " <= ",  col_name_B)
        } else
        {
           rec_col=paste0("rec[1,'",col_name[i],"']") 
           text_cond=paste0( col_name[i], ' == ', rec_col)
        }
        print(text_cond) 
        db <- db %>% filter(  eval(parse(text = text_cond )) )
    }
    return(db)
}

mail_blastula=function (subject = subject, attachmentList = NULL, addr = myaddress, 
    body = NULL, attachFileList = NULL, adhoc = "", creds_file = "/Users/admin/Documents/dropbox/jupyter/creds/netvigator", 
    timestamp = FALSE, from = "no.reply.blastula@netvigator.com", retry = 1) 
{
    require(blastula)
    # Check if the file exists
    if (!file.exists(creds_file)) {
      creds_file <- "c:\\Users\\admin\\Documents\\dropbox\\jupyter\\creds\\netvigator"  
    }
    if (!file.exists(creds_file)) {
      stop("creds_file problem")
    }
    mdbody = NULL
    if (timestamp == TRUE) 
        mdbody = c(mdbody, as.character(Sys.time()))
    if (!is.null(attachmentList)) {
        for (i in 1:length(attachmentList)) {
            mdbody = c(mdbody, "![ABC](", attachmentList[1], 
                ")")
        }
    }
    if (!is.null(body)) {
        if (class(body) == "list") {
            for (i in 1:length(body)) {
                ele = body[[i]]
                if (inherits(ele, "formattable")) {
                    print('formattable')
                  mdbody <- c(mdbody, gsub('\n', '', as.htmlwidget(ele)[[1]]$html)  )
                }
                else if (inherits(ele, "data.frame")) {
                  mdbody = c(mdbody, knitr::kable(ele, format = "html"))
                }
                else if (class(ele) == "character") {
                  print((grepl("^(.+)/([^/]+)png$", ele)))
                  if (grepl("^(.+)/([^/]+)png$", ele) && file.exists(ele)) {
                    mdbody = c(mdbody, "![ABC](", ele, ")")
                  }
                  else {
                    mdbody = c(mdbody, ele)
                  }
                }
                else mdbody = c(mdbody, ele)
            }
        }
        else {
            mdbody = c(mdbody, body)
        }
    }
    email <- compose_email(body = md(mdbody))
    if (!is.null(attachFileList)) {
        for (i in 1:length(attachFileList)) {
            print(attachFileList[[i]])
            email = email %>% add_attachment(file = attachFileList[[i]], 
                filename = basename(attachFileList[[i]]))
        }
    }
    actualtry = 0
    callstatus = -1
    while (callstatus == -1 && actualtry <= retry) {
        callstatus = tryCatch({
            smtp_send(email = email, from = from, to = addr, 
                subject = subject, credentials = creds_file(creds_file))
            1
        }, error = function(cond) return(-1), warning = function(cond) return(0))
        actualtry = actualtry + 1
    }
    return(actualtry)
}

mail_blastula_old=function (subject = subject, attachmentList = NULL, addr = myaddress, 
    body = NULL, attachFileList = NULL, adhoc = "", creds_file = "/Users/admin/Documents/dropbox/jupyter/creds/netvigator", 
    timestamp = FALSE, from = "no.reply.blastula@netvigator.com", retry = 1) 
{
    require(blastula)
    # Check if the file exists
    if (!file.exists(creds_file)) {
      creds_file <- "c:\\Users\\admin\\Documents\\dropbox\\jupyter\\creds\\netvigator"  
    }
    if (!file.exists(creds_file)) {
      stop("creds_file problem")
    }
    mdbody = NULL
    if (timestamp == TRUE) 
        mdbody = c(mdbody, as.character(Sys.time()))
    if (!is.null(attachmentList)) {
        for (i in 1:length(attachmentList)) {
            mdbody = c(mdbody, "![ABC](", attachmentList[1], 
                ")")
        }
    }
    if (!is.null(body)) {
        if (class(body) == "list") {
            for (i in 1:length(body)) {
                ele = body[[i]]
                if (inherits(ele, "data.frame")) {
                  mdbody = c(mdbody, knitr::kable(ele, format = "html"))
                }
                else if (class(ele) == "character") {
                  print((grepl("^(.+)/([^/]+)png$", ele)))
                  if (grepl("^(.+)/([^/]+)png$", ele) && file.exists(ele)) {
                    mdbody = c(mdbody, "![ABC](", ele, ")")
                  }
                  else {
                    mdbody = c(mdbody, ele)
                  }
                }
                else mdbody = c(mdbody, ele)
            }
        }
        else {
            mdbody = c(mdbody, body)
        }
    }
    email <- compose_email(body = md(mdbody))
    if (!is.null(attachFileList)) {
        for (i in 1:length(attachFileList)) {
            print(attachFileList[[i]])
            email = email %>% add_attachment(file = attachFileList[[i]], 
                filename = basename(attachFileList[[i]]))
        }
    }
    actualtry = 0
    callstatus = -1
    while (callstatus == -1 && actualtry <= retry) {
        callstatus = tryCatch({
            smtp_send(email = email, from = from, to = addr, 
                subject = subject, credentials = creds_file(creds_file))
            1
        }, error = function(cond) return(-1), warning = function(cond) return(0))
        actualtry = actualtry + 1
    }
    return(actualtry)
}

