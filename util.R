
# Note: you need to revise the path location 
# for mail() and shot() on your machine
# ************* for windows Sys.setlocale("LC_CTYPE","chinese")
#rm(list = ls(all = TRUE))  

#  "C://Users//admin//cache//"
#  /Users/admin/Documents/Dropbox/_data/

require("mailR")
library(RJDBC)
globalsaveversion     = 2  #for save
globalDefaultServer   ="99.myftp.org"
globalDefaultEmailAddr="nohongkong.com@gmail.com"  #for mail2
globalDefaultEmailAddr="cskmlui@hotmail.com"  #for mail2

utilVersion="... util.R (R verion 30 Jan 2021) loaded"

#testdf=data.frame(a=c(1,2,3,4,5,6,7,8,9,10,11), b=c(5,2,4,5,1,1,1,1,1,1,0))  #test_data

library(RCurl)
library(XML)
library(rvest)
library(quantmod)  
library("zoo")
library("xts")
library(stringr)

# if (require("RODBC")==FALSE) library(quantmod)

dateRange=function(tmpdf=masterData[[1]]) 
{  #deepsky
  if (class(tmpdf)[1] %in% c('xts','zoo')) return ( paste(index(head(tmpdf,n=1)), '~',index((tail(tmpdf,n=1)))) )
  if ('date' %in% names(tmpdf))
  {
    return ( paste(head(tmpdf$date,n=1), '~',(tail(tmpdf$date,n=1))) )
  } else
    return ( paste(rownames(head(tmpdf,n=1)), '~',rownames((tail(tmpdf,n=1)))) )
}

dateRangeAll=function(stockcodeL)
{
  for (scode in stockcodeL)  print(paste0(scode,":",dateRange(masterData[[scode]])))
}

lastrun=function() print(paste0("-- Last Run ",Sys.time()," --"))
asc=function(s) return(s [ order(s$date, decreasing=FALSE),])
desc=function(s) return(s [ order(s$date, decreasing=TRUE),])    

arrange.vars.head=function (data, colvec) 
{
    ncolv = 1
    for (i in length(colvec):1) {
        nVec = ncolv
        names(nVec) = colvec[i]
        data = arrange.vars(data, nVec)
    }
    return(data)
}

arrange.vars.last=function (data, colvec) 
{
    ncolv = ncol(data)
    for (i in 1:length(colvec)) {
        nVec = ncolv
        names(nVec) = colvec[i]
        data = arrange.vars(data, nVec)
    }
    return(data)
}
arrange.vars.first=arrange.vars.head
mv.head=arrange.vars.head
mv.tail=arrange.vars.last


help.short=function()
{
  print("min      ==> varMN")
  print("max      ==> varMX")
  print("index    ==> varDX")
  print("df       ==> varDF")
  print("factor   ==> varF")
  print("list     ==> varL")
  print("raw data ==> rdata")
  print("date     ==> dei") 
  print("date     ==> varD")
  print("nor      ==> varNR")
  print("mean     ==> varAV")
  print("matrix   ==> varMTX")
  print("prob     ==> varPB")
  print("freq     ==> varFQ")
  print("info     ==> varIFO")
  print("prediction ==> pd")
  print("probabilty ==> pb")
  print("rs       ==> result")
  print("smy      ==> summary")
  print("total    ==> TT")
} 

help.tidy=function()
{  
  messy <- data.frame(
    name = c("Wilbur", "Petunia"), a = c(67, 80), b = c(56, 90))
  print(messy)
  print("dat=messy %>%  gather(drug, heartrate, c(2:3))")
  dat=messy %>%  gather(drug, heartrate, c(2:3))
  print(dat)
  print("dat2= dat %>% spread(drug, heartrate)")
  dat2= dat %>% spread(drug, heartrate)
  print(dat2)
}


help.plot=function()
{ print("pch: 19_solid_circle    20_bullet")
  print("     21_filled_circle   22_filled_square")
  print("     23_filled_diamond  24_filled triangle_pnt-up")
  print("     25_filled_triangle_pnt_down")
  print("plot(c(1,2,3,5),type='l',lty=2, lwd=5)")
  print("plot(c(1,2,3), c(1,2,3)) # first plot ")
  print("par(new = TRUE)")
  print("plot(c(5,6,7), c(7,6,5) , type = 'l', axes = FALSE, bty = 'n', xlab = '', ylab = ''")
  print("axis(side=4, at = pretty(range(c(7,6,5))))") 
}

help.time=function()
{
  batch=c(60,120,180,240,300,360,420)
  x=data.frame(batch=batch, stamp=batchToStamp(batch))
  return(x)
}

help.do.call=function()
{
  L=list(c(1,2,3), c(4,5,6))
  lapply(L, sum) 
  list( sum( L[[1]]) , sum( L[[2]])) #same as above
  do.call(sum, L) 
  sum( L[[1]], L[[2]]) #same as above
}

help.reshape=function()
{
  d <- data.frame(sample = LETTERS[1:2], cat = letters[1:6], count = c(1:6))
  print(d)
  nn<-reshape(d,timevar="cat",idvar="sample",direction="wide")
  print(nn)
  nn[is.na(as.matrix(nn)) ] <- 0
  print(nn)
}


#### __GET_INFO__ ####
getAllStock=function(updated=TRUE)
{
  hsic=getHSIComponent()
  hscei=getHSCEI()
  nonhsi=getNONHSIComponent()
  allstk=unique(c( hsic, hscei, nonhsi))
  takeout=NULL
  if (updated==TRUE)
  { #k=1
    for (k in 1:length(allstk)) 
    {
      s=loadData(allstk[k])
      if (index(tail(s,n=1))!=u.Sys.Date()) takeout=c(takeout,-k)
    }
  }
  if (!is.null(takeout)) allstk=allstk[takeout]
  return (allstk)
}

getDJI=function()
{
  retV=c("DD" ,"AXP","MMM","CAT","PFE" ,"CVX" ,"BA" ,"UNH" ,"MRK","CSCO",
         "HD" ,"DIS","V"  ,"TRV","UTX" ,"INTC","IBM","JPM" ,"MCD","WMT" ,
         "GS" ,"NKE","XOM","KO" ,"MSFT","VZ"  ,"JNJ","AAPL","GE" , "PG")
  return(retV)
}
  

getHSCEI=function()
{
  c("1398.HK" ,"0939.HK" ,"3988.HK" ,"0857.HK" ,"0386.HK",
    "2628.HK" ,"2318.HK" ,"1288.HK" ,"1088.HK" ,"3968.HK",
    "2601.HK" ,"0728.HK" ,"3328.HK" ,"1988.HK" ,"0998.HK",
    "2328.HK" ,"0489.HK" ,"0914.HK" ,"2883.HK" ,"2333.HK",
    "1211.HK" ,"1359.HK" ,"0902.HK" ,"1800.HK" ,"1339.HK",
    "0916.HK" ,"0168.HK" ,"1099.HK" ,"3323.HK" ,"1336.HK",
    "0358.HK" ,"2238.HK" ,"6030.HK" ,"6837.HK" ,"0390.HK",
    "2338.HK" ,"1898.HK" ,"1066.HK" ,"1171.HK" ,"0753.HK")
} 
# 

getHSIComponentFromWiki=function(refresh=FALSE)
{
  store="/Users/admin/Documents/Dropbox/_data/Store_HSIComponentFromWiki.RData"  
  if (refresh==TRUE)
  {
    require(rvest)    
    url <- "https://en.wikipedia.org/wiki/Hang_Seng_Index"
    page =  read_html(url) %>% 
      html_nodes('ul') %>%
      html_nodes('li') 
    code=NULL
    for (i in 1:length(page))
    {
      if (regexpr( '<li>\\d\\d\\d\\d' , page[i]  )==1)
      {
        code=c(code, paste0(substr(page[i], 5, 8),'.HK') )  
      }
    }
    save(code, file=store)
  } else
  {
    load(store)  
  }
  return(code)
}

getHKComponentFromWeb=function(refresh=FALSE, debug=0)
{
  store="/Users/admin/Documents/Dropbox/_data/Store_HKComponentFromWeb.RData"  
  if (refresh==TRUE)
  {
    url <- "https://www.tradingview.com/markets/stocks-hong-kong/market-movers-large-cap/"
    page =  read_html(url) 
    tbls <- html_nodes(page, "table")
    wtble=html_table(tbls[1], fill = TRUE)
    w.df=wtble[[1]]
    stockCode=NULL
    for (i in 1:nrow(w.df))
    {
      gFlag='N'
      str=w.df[i,1]
      ostr=w.df[i,1]
      aCh=substr(str,2, 2)
      aaCh=substr(str,3, 3)
      if (grepl("[^0-9]", aCh) && grepl("[^0-9]", aaCh)) {
        gFlag='Y'
        str = substr(str, 3, nchar(str )) 
      }
      candid=as.numeric(gsub("([0-9]+).*$", "\\1", str  ))
      if (is.na(candid))
      {
        candid=as.numeric(gsub("([0-9]+).*$", "\\1", ostr  ))
      }
      if (debug==1) print(paste0(aCh, ':',gFlag,':',candid,':',ostr))
      str=paste0('0000',candid,'.HK')
      n=7
      fstr=substr(str,(nchar(str)+1)-n,nchar(str))
      if (debug==1) print(fstr)
      stockCode=c(stockCode, fstr)
    }
    save(stockCode, file=store)  
  } else
  {
    load(store)  
  }
  return(stockCode)
}

getNONHSIFromWeb=function(refresh=FALSE)
{
  highCap=getHKComponentFromWeb(refresh=refresh)
  hsiComponent=getHSIComponentFromWiki(refresh=refresh)
  highCapButHSI=highCap[!(highCap %in% hsiComponent)]
  return(highCapButHSI)
}


getHSIComponent=function(refresh=FALSE)
{
  if (refresh==TRUE)
  {
    odbcConnector="odbcmysql"
    sqlQ="select * from hsicomponent where enddate>='2020-02-02'"
    myconn <-odbcConnect(odbcConnector)
    stock <- sqlQuery(myconn, sqlQ)  
    close(myconn)
    code=stock$icode
    ycode=convertYahooCode(code)
    u.save("Dropbox/_data/hsicomponent.RData", ycode)
  } else
  {
    u.load("Dropbox/_data/hsicomponent.RData")
  }
  return(ycode)
}

getCBBCComponent=function()
{
  cbbc=getCBBC()
  hsi=getHSIComponent()
  return (intersect(cbbc, hsi))
}  

getCBBCComponent.OLD=function()
{
  s=c("0001.HK", "0005.HK", "0013.HK", "0016.HK", "0027.HK",
      "0941.HK", "0992.HK", "1088.HK", "0857.HK", "0883.HK", 
      "0914.HK", "0939.HK", "0762.HK", "0688.HK", "0388.HK",
      "0386.HK", "0175.HK", "0135.HK")
  return(s)
}

getCBBC=function()
{
  CBBC=c(1,5,13,16,27,             135,175,386,388,688, 
         700,762,857,883,914,      939,941,992,1088,1288,
         1299,1359,1398,1928,1988, 2318,2333,2628,2822,2823, 
         3323,3800,3888,3968,3988, 6863)
  return(convertYahooCode(CBBC))
}

getNONHSIComponent=function(n=50, refresh=FALSE)
{
  if (refresh==TRUE)
  {
    odbcConnector="odbcmysql"
    sqlQ="select * from nonhsicomponent where enddate>='2020-02-02'"
    myconn <-odbcConnect(odbcConnector)
    stock <- sqlQuery(myconn, sqlQ)  
    close(myconn)
    code=stock$icode
    ycode=convertYahooCode(code)
    u.save("Dropbox/_data/nonhsicomponent.RData", ycode)
  } else
  {
    u.load("Dropbox/_data/nonhsicomponent.RData")
  }
  ycode=sort(ycode)
  if (n>length(ycode)) n=length(ycode)
  return(ycode[1:n])
}


getNONHSIComponent_OLD=function(n=50)
{
  nonhsi=c( "2888.HK", "1128.HK", "1913.HK", "1972.HK", "0291.HK", 
            "0880.HK", "1929.HK", "2282.HK", "0392.HK", "0656.HK", 
            "0384.HK", "0020.HK", "1114.HK", "2688.HK", 
            "0010.HK", "0270.HK", "0960.HK", 
             "0813.HK", "3311.HK", "0659.HK", "0087.HK", 
            "3333.HK", "0486.HK", "1169.HK", "0241.HK", "0257.HK", 
            "0460.HK", "0669.HK", "0371.HK", "0551.HK", "2313.HK", 
            "1680.HK", "0127.HK", "0069.HK", "0142.HK", "0014.HK", 
                       "0200.HK", "0683.HK", "1093.HK", "0522.HK", 
            "1910.HK", "0966.HK", "0410.HK", "2038.HK", "2378.HK", "1199.HK")
  nonhsi=sort(nonhsi)
  if (n>length(nonhsi)) n=length(nonhsi)
  return(nonhsi[1:n])
}

checkNONHSIComponent=function()
{
  standardNon=sort(getNONHSIComponent())
  hfn=sort(unique(rtdata(table="highfreq_nonhsi")$icode))
  hfn=convertYahooCode(hfn)
  setdiff(standardNon, hfn)
}


getIndex=function() 
{ return( c("^HSI","^SSEC", "^N225", "^DJI", "^GSPC",  "^GDAXI","^FCHI" )) }  #"^FTSE"

getCapSize=function() #see getCapSize_RARE.R
{
  u.load("Dropbox/_data/compInfo.RData")
  allCompany=na.omit(allCompany)
  allCompany=allCompany[ order(allCompany$cap, decreasing=TRUE), c(1,3)]
  for (i in 1:nrow(allCompany))
  {
    allCompany$code[i]=convertYahooCode(allCompany$code[i])  
  }
  return(allCompany)
}

getSSEC=function() 
{
  link="http://hq.sinajs.cn/list=sh000001"
  tbl=read.table(link, header=FALSE, sep=",",encoding="UTF-8")
  txt=toString(tbl[1,1])
  txt=strsplit(txt,",")[[1]]
  names(sample)
  tradeTime=as.POSIXct(paste(txt[31], txt[32]))
  last=as.numeric(txt[4])
  open=as.numeric(txt[2])
  high=as.numeric(txt[5])
  low=as.numeric(txt[6])
  vol=as.numeric(txt[9])/1000
  yesday=as.numeric(txt[3])
  chgV=last-yesday
  chgPC=(chgV/yesday)*100
  retV=data.frame(tradeTime,last,chgV, chgPC, open, high, low,vol)
  names(retV)=c("Trade Time","Last","Change","% Change","Open","High","Low","Volume")
  return(retV)
}

limitDataSet=function(asset_all=NULL, to=NULL)
{
  print("Use only for debug")
  dirPath="default"
  if (dirPath=="default")
  {
    dirPath=getPath()
  }
  
  if (is.null(asset_all)) asset_all=c("^HSI", "^SSEC", getHSIComponent())    
  
  clearMemory()
  
  for (i in 1 : length(asset_all))
  {
    s=loadData(asset_all[i])
    s=subset(s, index(s)<=to)
    
    stockCode=tolower(asset_all[i])
    stockC=paste(dirPath, stockCode, ".RData", sep="") 
    print(paste("save the data to",stockC))
    save(s,file=stockC , version=globalsaveversion)  
  }  
  clearMemory()
}

getGlobalCache=function()
{
  if (is.windows()) 
  {
    return ("C://Users//admin//cache//")
  }
  return ("/Users/admin/cache/")
}
#### __IS_KIND_OF_MACHINE__ ####
isMac=function() is.mac()
isCloud=function() is.cloud()
isPolyu=function() is.polyu()
isLenovo=function() is.lenovo()
isASUS=function() is.asus()
isWindows=function() is.windows()
is.darwin=function() { if (Sys.info()['sysname']=="Darwin") return(TRUE) else return(FALSE) }
is.mac=function() { drive="/Users/admin"; return(tolower(substr(Sys.info()[1],1,3))!="win" && file.exists(drive)) }
#is.mac=function() { if (Sys.info()['sysname']=="Windows") return(FALSE)}
is.cloud=function() { drive="/net/nas20/rcloud_data"; return(file.exists(drive))}
is.polyu=function() { drive="C://Users//polyu";  return(file.exists(drive))}
is.lenovo=function() { drive="C://Users//homa";  return(file.exists(drive))}
is.asus=function() { drive="C://Users//ASUS";  return(file.exists(drive))}
is.windows=function() { drive="C://Users";  return(tolower(substr(Sys.info()[1],1,3))=="win" && file.exists(drive))}
is.linux=function() { return(tolower(substr(Sys.info()[1],1,3))=="lin") }



isOnline <- function(url="www.baidu.com") {
  execString=paste("ping -c 1 ", url)
  if (isWindows()) execString=paste("ping -n 1 ", url)
  (tryCatch(system(execString, intern=TRUE),
            error = function(c) "0",
            warning = function(c) "0",
            message = function(c) "0"
  ) != 0 )[1]
}

getHostName=function()
{
  hnam =as.character((Sys.info()["nodename"]))
  return (substr(hnam,1,10))
}

#### __BOOTSTRAP__ ####
simpleBoot=function(x,width,FUN)
{ #  simpleBoot(df[,8], 10, FUN=bootsmean)
  if (class(x)!="numeric") stop("error, x should be numeric vector")
  rp=rollapply(x,FUN=FUN, width=width, align="right")
  nrowdiff=length(x)-nrow(rp) 
  someNA=rep(NA,nrowdiff)
  return (data.frame(bd=c(someNA,rp[,1]),
                     bu=c(someNA,rp[,2])))
} 

getBootstrapBand=function(code="^hsi", data=NULL,
                          from="2013-01-01", to=Sys.Date(),width=10)
{
  # util.R
  s=data
  if (!any( class(s)=="xts"))
  {
    s=loadData(code)
    s=subset(s, v!=0)
    s=subset(s, as.Date(from)<=index(s) & index(s)<=to)
  }
  b=xtsToDf(s)
  DX=4; 
  rp=rollapply(b[,DX],FUN=bootsmean, width=width, align="right")
  nrowdiff=nrow(b)-nrow(rp)
  df=data.frame(date=b$date, bu=c(rep(NA,nrowdiff),rp[,2]),bl=c(rep(NA,nrowdiff),rp[,1])) 
  retV=dfToXts(df)
  return(retV) 
}

bootsmean=function(x)
{ 
  #eg  bootsmean(c(10,20,30,40,50,23,45,67,89,23))
  #eg  bootsmean(testdf[,1])
  #    util.R
  require(boot)
  if (all(is.na(x))==TRUE) return(c(NA, NA))
  minV=min(x, na.rm=TRUE)
  maxV=max(x, na.rm=TRUE)
  if (class(x)=="numeric" && minV==maxV)   return( c(minV, minV))
  #if (class(x)=="numeric" && !(any(x!=x[1])))   return( c(x[1], x[1]))
  bmean <- function(x, d) return(mean(x[d], na.rm=TRUE))
  #x = c(10,20,30,40,50,23,45,67,89,23)
  b=boot(x , statistic=bmean , R=50)
  ci = boot.ci(b, type="basic")
  ci$basic[4:5]
}


#### __SHORTHAND__ ####

u.Version=function(fileString, v=1)
{
  ver=paste("v",v,sep="")  
  return(gsub("vX", ver, fileString))
}

assert = function(cond , messageWhenFalse) checkIf( !cond , messageWhenFalse)

checkIf=function(cond, messageText)
{
  # same as if then but improve readability
  if (cond) stop(messageText)
}

cpu=function() return(system("top -bn2 | grep 'Cpu(s)'",intern=TRUE)[2])


#print(twolines("xxxxxxxxxxxxdkjfkjfkdjkfjkdjfkdjkfjdkjfkdjfkdjkfjdkjfkdjkfjdkjfkdjfkdjkfjdfjdkfjdkjfkdjfdjkddfdfdfdfdfdfdxxxxxxxddddddddddddddddd"))
twolines=function( x , nAt=50)
{
  retV=x
  if (nAt <= nchar(x))
  {
    #print(nchar(x))
    retV=paste(substr(x, 1, nAt), "\n", substr(x, nAt+1, nchar(x)), sep = "")
    #print(retV)
  } 
  return (retV)
}

inc=function(x=NULL, recycleAt=99999) 
{
  if (!is.null(x)) 
  {
    globalCnt<<-x    
    return(globalCnt)
  }
  globalCnt <<- (globalCnt+1);
  retV=globalCnt %% recycleAt
  if (retV==0 ) retV=recycleAt
  return(retV)
}  

randomString <- function(n=1, lenght=12, letterOnly=TRUE)
{
  randomString <- c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    sym=c(0:9, letters, LETTERS)
    if (letterOnly) sym=LETTERS
    randomString[i] <- paste(sample(sym,
                                    lenght, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}



batchR=function(executeFilename=NULL, tmpNum, background=TRUE, paraLFromBatchR=NULL, n=15, force=FALSE, SSH=FALSE, sshLoginString=NULL, sshPort="2003")
{
  if (is.null(executeFilename)) 
  {
    system(paste("tail -n ", n,  " /Users/admin/temp/tmp0/result.Rout",sep=""))
  } else
  { 
    if (tmpNum==0 && force==FALSE)
    {
      sysCmd="grep 'elapsed' /Users/admin/temp/tmp0/result.Rout > end.proc.txt"
      system(sysCmd)
      fileSize=file.info("end.proc.txt")$size 
      if (fileSize==0) stop("** ERROR ** Prevous Process Not Yet Finished")
    }    
    if (tmpNum != 0 )
    {
      tmpNum=tmpNum %% 9   
      if (tmpNum == 0) tmpNum= 9
    }
    getDropboxDir()
    # e.g. batchR("FS2/h2o_FS.R", 1)
    workdir=paste("/Users/admin/temp/tmp",tmpNum,sep="")
    if (!file.exists(workdir)) stop (paste(workdir, " NOT Exist"))
    script=getDropboxDir(executeFilename)
    if (!file.exists(script)) stop (paste(script, " NOT Exist"))
    myR=getDropboxDir("batch/myR.sh")
    if (background==FALSE) myR=getDropboxDir("batch/myR_no_background.sh")
    if (!file.exists(myR)) stop (paste(myR, " NOT Exist"))
    paraLFromBatchR[["executeFilename"]]=executeFilename
        
    paraFile=paste(workdir,"/exeFilename.RData",sep="")
    save(paraLFromBatchR, file=paraFile , version=globalsaveversion)
    command=paste(myR, workdir, script)
    if (SSH==FALSE) system(command)
    
    
    if (SSH==TRUE)  
    {
      #workdir="/Users/admin/temp/tmpdc"
      paraFile.dc=paste(workdir,"/exeFilename.RData",sep="")
      
      #paraFile.dc="/Users/admin/Documents/Dropbox/exeFilename.RData"
      #save(paraLFromBatchR, file=paraFile.dc, version=globalsaveversion)
      command=paste(myR, workdir, script)
      
      sshcommand= paste("scp -P ", sshPort," ", paraFile," ",sshLoginString,":",paraFile.dc, sep="")
      system( sshcommand )
      x.print(workdir)
      # Sys.sleep(120)
      sshcommand=paste("ssh -l kimman -f ",sshLoginString," -p", sshPort, command)
      system( sshcommand )
    }
  }
}


batchRDC=function(n=10) system(paste("tail -n ", n,  " /Users/admin/temp/tmpdc/result.Rout",sep=""))
batchR1=function(n=10) system(paste("tail -n ", n,  " /Users/admin/temp/tmp1/result.Rout",sep=""))
batchR2=function(n=10) system(paste("tail -n ", n,  " /Users/admin/temp/tmp2/result.Rout",sep=""))
batchR3=function(n=10) system(paste("tail -n ", n,  " /Users/admin/temp/tmp3/result.Rout",sep=""))
batchR4=function(n=10) system(paste("tail -n ", n,  " /Users/admin/temp/tmp4/result.Rout",sep=""))
batchR5=function(n=10) system(paste("tail -n ", n,  " /Users/admin/temp/tmp5/result.Rout",sep=""))
batchR6=function(n=10) system(paste("tail -n ", n,  " /Users/admin/temp/tmp6/result.Rout",sep=""))
batchR7=function(n=10) system(paste("tail -n ", n,  " /Users/admin/temp/tmp7/result.Rout",sep=""))
batchR8=function(n=10) system(paste("tail -n ", n,  " /Users/admin/temp/tmp8/result.Rout",sep=""))
batchR9=function(n=10) system(paste("tail -n ", n,  " /Users/admin/temp/tmp9/result.Rout",sep=""))


assignVfromL=function(l , varN, defaultV=NULL)
{
  # myL=list(abc=123)
  # assignVfromL(myL, "abc",1)
  # assignVfromL(myL, "xyz",1)
  nam=names(l)
  if (any(varN==nam)) return( l[[varN]])
  return(defaultV)
}




am.subset=function(x) 
{
  return(subset(x, 30<=batch & batch <=180))
}

pm.subset=function(x) 
{
  return(subset(x, 240<=batch & batch <=420))
}

is.any.infinite=function(dat)
{
  for (i in 1:nrow(dat))
  {
    for (j in 1:ncol(dat))
    {
      if(is.infinite(dat[i,j])) return(TRUE)
    }
    
  }
  return (FALSE)
}

isnot.na=function(...) 
{ !is.na(...) }

isnot.null=function(...) 
{ !is.null(...)}

is.betweenE <- function(x, a, b) {
  r=range(a,b)
  r[1] <= x & x <= r[2]
}

is.between <- function(x, a, b) {
  r=range(a,b)
  r[1] < x & x < r[2]
}


dir.exists <- function(d) {
  de <- file.info(pth)$isdir
  ifelse(is.na(de), FALSE, de)
}


uniPath=function(...)
{
  x=paste(...,sep="")
  y=x
  #pth="/Users/admin/Documents/"
  pth="/Users/admin/Documents"
  pthEnd="/"
  if (isWindows()) 
  {
    pth="C://Users//admin//Documents"
    pthEnd="//"
    #pth="C://Users//homa//"
    y=gsub( "/", "//", x)
  }
  pfile=paste(pth,pthEnd,y,sep="")
  if (!file.exists(pfile) &&  length(grep(".RData", pfile))==0 ) 
  {
    pfile=paste("http://99.myftp.org/~admin/",x,sep="")
  }
  return (pfile)
}

u.png=function(...) { png(..., width = 1020, height =700) }

internet.source=function(file="jdbc.R", internet=FALSE)
{
  fullFile=paste("/Users/admin/Documents/Dropbox/weblib/R/_shared/", file, sep="")
  internetFile=paste("http://", globalDefaultServer, "/weblib/R/_shared/", file, sep="")
  if (internet==TRUE) fullFile="xxxxx"
  if (file.exists(fullFile))
  {
    source(fullFile) 
  } else
  {
    source(internetFile) 
  }
}


u.file.exists=function(x)
{
  if (file.exists(x)==FALSE) return (FALSE)
  return( !file.info(x)$isdir )
}

u.chartSeries=function(x)
{
  if ("xts" %in% class(x))
  {
    if (isnot.null(x$Open)) oDX=which(names(x)=="Open")
    if (isnot.null(x$Close)) cDX=which(names(x)=="Close")
    if (isnot.null(x$High)) hDX=which(names(x)=="High")
    if (isnot.null(x$Low)) lDX=which(names(x)=="Low")
    if (isnot.null(x$open)) oDX=which(names(x)=="open")
    if (isnot.null(x$close)) cDX=which(names(x)=="close")
    if (isnot.null(x$high)) hDX=which(names(x)=="high")
    if (isnot.null(x$low)) lDX=which(names(x)=="low")
    if (isnot.null(x$o)) oDX=which(names(x)=="o")
    if (isnot.null(x$c)) cDX=which(names(x)=="c")
    if (isnot.null(x$h)) hDX=which(names(x)=="h")
    if (isnot.null(x$l)) lDX=which(names(x)=="l")
    
    tmp=x[, c(oDX,hDX,lDX,cDX)]
    names(tmp)=c("open","high","low","close")
    chartSeries(tmp)
  }
}

u.tail=function(...,n=1)  tail(n=n,...)

u.source=function(file=NULL,...) 
{ 
  if (is.null(file))
  {
    print("eg: u.source('Dropbox/R_work/FS2/vX/mapCandleVLib.R')")
    
  } else
  {
    source(uniPath(file), ...)
  }
}

c.source=function(file,...)
{
  if (isOnline())
  {
    
    httpBase=paste("http://",globalDefaultServer,"/weblib/getfile.php?file=",sep="")
    url=paste(httpBase, file,sep="")
    #print(url)
    temp=GET(url)
    if (  length( grep("File not found", as.character(temp)   ))==0  ) source(url)
    else stop("error: it is online but problem with c.source")
  } else
  {
    source(uniPath(file), ...)
  }
}

u.colnames=function(x, idx=10, newcolname="sh")
{
  tmpname=names(x)
  for (i in length(idx))
  {
    tmpname[idx[i]]=newcolname[i]
  }
  names(x)=tmpname
  return(x)
}

changeColName=function(df, old, new)
{
  existnames=names(df)
  existnames[existnames==old]=new
  names(df)=existnames
  return(df)
}

u.rbind=function(a1,a2=NULL,a3=NULL,a4=NULL,a5=NULL,a6=NULL,
                 a7=NULL,a8=NULL, a9=NULL)
{
  a1=as.data.frame(a1)  
  if (!is.null(a2)) {a2=as.data.frame(a2) ;names(a2)=names(a1); a1=rbind(a1,a2)}
  if (!is.null(a3)) {a3=as.data.frame(a3) ;names(a3)=names(a1); a1=rbind(a1,a3)}
  if (!is.null(a4)) {a4=as.data.frame(a4) ;names(a4)=names(a1); a1=rbind(a1,a4)}
  if (!is.null(a5)) {a5=as.data.frame(a5) ;names(a5)=names(a1); a1=rbind(a1,a5)}
  if (!is.null(a6)) {a6=as.data.frame(a6) ;names(a6)=names(a1); a1=rbind(a1,a6)} 
  if (!is.null(a7)) {a7=as.data.frame(a7) ;names(a7)=names(a1); a1=rbind(a1,a7)}
  if (!is.null(a8)) {a8=as.data.frame(a8) ;names(a8)=names(a1); a1=rbind(a1,a8)}
  if (!is.null(a9)) {a9=as.data.frame(a9) ;names(a9)=names(a1); a1=rbind(a1,a9)}  
  return( a1)
}


u.load=function(file,...) {
  envir = parent.frame()
  load(file=uniPath(file),envir=envir, ...)
}

u.save=function(file,...) {  
  envir = parent.frame()
  save(file=uniPath(file),version=globalsaveversion, envir=envir, ...) 
}

u.save.image=function() {
  save.image(file=uniPath("Dropbox/_data/image.RData"))
}

u.load.image=function(e=parent.frame()) {
  load(file=uniPath("Dropbox/_data/image.RData"), envir =e )  
}

System.out.print=function(...) { print(paste(...,sep="")) }
System.out.println=function(...) { print(paste(...,sep="")) }


sprint=function(..., sep= " ")
{
  pV=''
  argnames <- sys.call()
  args <- list(...)
  ttl=length(argnames)
  for (i in 2:ttl)
  {
    #print(as.character(argnames[i]))
    #print(class(args[i-1]))
    if (as.character(argnames[i])==as.character(args[i-1])) 
    {
      pV=paste0(pV,args[i-1], sep)
    } else    
      pV=paste0(pV,argnames[i],"=",args[i-1],sep)
  }    
  print(pV)
  #print(argnames[2])
  #print(args)   
}


u.print=function(...) { print(paste(...,sep="")) }

x.print=function(..., envir=parent.frame())
{
  ## Get the data objects passed
  models <- list(...)
  nmodels <- length(models)
  ## Get names of models:
  modelnames <- as.character(substitute(c(...))[-1])
  
  ##### much code omitted here #####
  retV=NULL
  for (systemUsedDex in 1:length(modelnames))
  {
    text=modelnames[systemUsedDex]
    val=eval(parse(text=text),envir=envir)
    if (length(val)>1)  val=paste(val, collapse=", ")
    if (systemUsedDex!=1) retV=paste(retV, "; ",sep="")
    retV=paste(retV, text,"=",val,sep="")
  }
  print(retV)
} 

u.p=function(...) {u.print(...)}
x.p=function(...) {x.print(...)}
n=function(...) {names(...)}
h=function(...) {head(...)}
r=function(...) {round(...)}
p=function(...) {paste(...)}
pr=function(...) {print(...)}
u.pr=function(...) {u.print(...)}
nr=function(...) { nrow(...)}
nc=function(...) { ncol(...)}

u.rm=function() { rm(list = ls(all = TRUE))  }

u.win.graph=function(...)
{
  if (isWindows()) 
  {
    win.graph(...)
  }
}

u.weekdays=function(v)
{ 
  v=weekdays(v)
  for (i in 1:length(v))
  {
    if (v[i]=="Monday")    v[i]="0"  
    if (v[i]=="Tuesday")   v[i]="1"  
    if (v[i]=="Wednesday") v[i]="2"  
    if (v[i]=="Thursday")  v[i]="3"  
    if (v[i]=="Friday")    v[i]="4"  
    if (v[i]=="Saturday")  v[i]="5"  
    if (v[i]=="Sunday")    v[i]="6"  
  }
  return(strtoi(v))
}

#### __TOOL FOR PARALLEL__ ####
waitFile=function(files, sleepSec=30, waitHowLongMin=45)
{
  checkFile=function(files, alreadyWait=0)
  {
    for (k in 1:length(files))
    {
      if (file.exists(files[k])==FALSE) 
      {
        print(paste(alreadyWait,"min: wait for", files[k]))
        return (FALSE)
      }
    }
    return(TRUE)
  }
  iter= (waitHowLongMin * 60)/sleepSec
  for (i in 1:iter)
  {
    haveWaited=round(((i-1)*sleepSec)/60,1)
    if (checkFile(files, alreadyWait=haveWaited)==FALSE)   Sys.sleep(sleepSec)
  }
  if (checkFile(files)==FALSE) return (FALSE)
  return (TRUE)
}


#### __TOOL FOR PLOT__ ####
rescale_OLD=function(x, rg, ptsOfx=NULL)
{
  if (length(rg)==1) {rg[2]=rg[1]; rg[1]=0}
  mnx=min(x); mxx=max(x);
  slope=(rg[2]-rg[1])/(mxx-mnx)
  #x=(x-mnx)*slope+rg[1]
  if (is.null(ptsOfx)) return ((x-mnx)*slope+rg[1])
  else return( (ptsOfx-mnx)*slope+rg[1] )
}



rescale <- function(x, rg, ptsOfx = NULL) {
  if (length(rg) == 1) {
    rg[2] <- rg[1]
    rg[1] <- 0
  }
  mnx <- min(x, na.rm = TRUE)
  mxx <- max(x, na.rm = TRUE)
  slope <- (rg[2] - rg[1])/(mxx - mnx)
  if (is.null(ptsOfx))
    return((x - mnx) * slope + rg[1]) 
  else return((ptsOfx - mnx) * slope + rg[1])
}


getReferLine=function()
{
  dc=rgb(255/255,200/255,200/255)
  uc=rgb(200/255,255/255,200/255)
  
  refer=list()
  refer[[1]]=as.Date(c("2014-03-07","2014-03-21", "2014-04-16", "2014-05-12"))
  refer[[2]]=c(dc,uc,dc,uc)
  refer[[3]]=as.Date(c("2014-06-09"))
  
  return(refer)
}


#### __TOOL__ ####

# importing packages/libraries
getIP=function()
{
  library(httr)
  library(jsonlite)
  
  # storing url in a variable
  # you can also pass the url directly in GET()
  url1 = "https://api.ipify.org?format=json"
  
  # requesting raw data from URL using GET()
  # and storing in data variable.
  data <- GET(url1)
  
  # converting raw data to char format.
  rawdata <- rawToChar(data$content)
  
  # converting char data to json format.
  jsondata <- fromJSON(rawdata)
  return(jsondata$ip)
}

dayhigh=function(da, day=20)
{
  # see ABC.R getABC
  #da=s[[stkDx]]
  #day=20
  if ( !("xts"  %in% class(da)) ) return(NULL); 
  maxr=nrow(da)
  
  da$hhx = rep(0,maxr )
  da$llx = rep(0,maxr )
  
  
  for (i in (0:(maxr-day+1)))
  {
    # i=0
    t =da[  (maxr-i-day+1):(maxr-i), ]
    t$temp=0
    t$temp=t$h
    ttp=-1 * as.numeric(t$temp)
    temp=rank( ttp )
    da[(maxr-i),'hhx']=last(temp,1)
    
    t$temp=t$l
    ttp=-1 * as.numeric(t$temp)
    temp=day-rank( ttp )+1
    da[(maxr-i),'llx']=last(temp,1)
  }
  return(da)
}


oneform=function(s)
{
  l="l";  h="h"; c="c"; o="o"; f="f"
  if (!("f" %in% colnames(s))) s$f=0
  for ( i in nrow(s):2)  # i=nrow(s)
  {
    if (as.numeric(s[i-1, l]) > s[i,h]) s[i,f]= -9
    if (as.numeric(s[i-1, h]) < s[i,l]) s[i,f]= +9
    if (as.numeric(s[i-1, h]) >= s[i,h] && as.numeric(s[i-1, l]) <= s[i,l] && s[i,o] <= s[i,c] ) s[i,f]= +7  
    if (as.numeric(s[i-1, h]) >= s[i,h] && as.numeric(s[i-1, l]) <= s[i,l] && s[i,o] >  s[i,c] ) s[i,f]= -7  
    if (as.numeric(s[i-1, h]) <= s[i,h] && as.numeric(s[i-1, l]) >= s[i,l] && s[i,o] <= s[i,c] ) s[i,f]= +4
    if (as.numeric(s[i-1, h]) <= s[i,h] && as.numeric(s[i-1, l]) >= s[i,l] && s[i,o] >  s[i,c] ) s[i,f]= -4
    if (as.numeric(s[i-1, h]) <= s[i,h] && as.numeric(s[i-1, l]) <= s[i,l] && as.numeric(s[i-1, h]) >= s[i,l] ) s[i,f]= +1
    if (as.numeric(s[i-1, h]) >= s[i,h] && as.numeric(s[i-1, l]) >= s[i,l] && as.numeric(s[i-1, l]) <= s[i,h] ) s[i,f]= -1
  }
  return (s)
}

oneform=function(s)
{
  l="l";  h="h"; c="c"; o="o"; f="f"
  if (!("f" %in% colnames(s))) s$f=0
  for ( i in nrow(s):2)  # i=nrow(s)
  {
    if (as.numeric(s[i-1, l]) > s[i,h]) s[i,f]= -9
    if (as.numeric(s[i-1, h]) < s[i,l]) s[i,f]= +9
    if (as.numeric(s[i-1, h]) >= s[i,h] && as.numeric(s[i-1, l]) <= s[i,l] && s[i,o] <= s[i,c] ) s[i,f]= +7  
    if (as.numeric(s[i-1, h]) >= s[i,h] && as.numeric(s[i-1, l]) <= s[i,l] && s[i,o] >  s[i,c] ) s[i,f]= -7  
    if (as.numeric(s[i-1, h]) <= s[i,h] && as.numeric(s[i-1, l]) >= s[i,l] && s[i,o] <= s[i,c] ) s[i,f]= +4
    if (as.numeric(s[i-1, h]) <= s[i,h] && as.numeric(s[i-1, l]) >= s[i,l] && s[i,o] >  s[i,c] ) s[i,f]= -4
    if (as.numeric(s[i-1, h]) <= s[i,h] && as.numeric(s[i-1, l]) <= s[i,l] && as.numeric(s[i-1, h]) >= s[i,l] ) s[i,f]= +1
    if (as.numeric(s[i-1, h]) >= s[i,h] && as.numeric(s[i-1, l]) >= s[i,l] && as.numeric(s[i-1, l]) <= s[i,h] ) s[i,f]= -1
  }
  return (s)
}


ifDataUpdated=function(assetCode=c("0001.hk", "0002.hk", "3333.hk","3988.hk","^hsi"))
{
  clearMemory()
  for (i in 1:length(assetCode))
  {
    checkIf( !(index(tail(loadData(assetCode[i]),n=1)) ==u.Sys.Date()) , paste("error: data",assetCode[i],"not updated"))
  }
  return(TRUE)
}

u.range=function(ts)
{
  paste0("Date Range for Data: ", paste(range(ts),collapse=" ~ ") )
  if ("xts" %in% class(ts)) paste0("Date Range for Data: ", paste(range(index(ts)),collapse=" ~ ") )
  if ("zoo" %in% class(ts)) paste0("Date Range for Data: ", paste(range(index(ts)),collapse=" ~ ") )
}


getDxFromName=function(dat, colname="xxx") which(names(dat)==colname)

arrange.vars <- function(data, vars){   #### Usuage arrange.vars( df, c("Out"=2))
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}

u.exists=function(x,defaultV="NULL")
{
  varN=deparse(substitute(x))
  varEval=paste(varN,"<<-",defaultV)
  print(varEval)
  if (exists(varN)) return(TRUE)
  eval(parse(text=varEval))
}


# eg. x=c(0,0,1,2.5,3,3.5,4.5,5,6,4,5,6,6,7)
# grouper(x)
grouper=function(x, rg=c(1,2,3,4,5), v=c(-3,-2,-1,1,2,3))
{
  maxX=max(x)
  dx=list()
  if (!(length(rg)+1)==length(v)) stop("error : len(rg)+1 should be len(v)")
  for (i in 1:length(rg)) dx[[i]]=(x<rg[i])
  x=rep(v[6],length(x))
  for (i in length(rg):1) x[dx[[i]]]=v[i]
  return(x)
}

#x=c(-0.007763263 , -0.018619846 , 0.0813269413 )
#sameSign(x)
sameSign=function(x)
{
  retV=FALSE
  len=length(x)
  if (all((x[1:len]<0)==TRUE)) retV=TRUE
  if (all((x[1:len]>0)==TRUE)) retV=TRUE
  return(retV)
}


trim <- function (x) 
{
  gsub("^\\s+|\\s+$", "", x)
}

last.na.nrow=function(x) 
{
  for (i in nrow(x):1)
  {
    if (any(is.na(x[i]))==FALSE) return (nrow(x)-i) 
  }
  return (nrow(x))
}

u.na.omit=function(x, value)
{
  x[is.na(as.matrix(x))] <- value
  return(x)
}


u.runif=function(n=10, df=data.frame(min=c(0,0.6, 0.7), max=c(0.3,0.7, 1), p=c(0.3,0.2,0.5)))
{
  if (sum(df$p)!=1) stop("Error: sum(df$p) should be one")
  df$cs=cumsum (df$p)
  df2=data.frame(firstR=runif(n,0,1))
  df2$min=-1
  df2$max=-1
  for (i in 1:nrow(df))
  { #i=1
    df2$min =ifelse (df2$firstR > df$cs[i] , df$min[i+1] , df2$min)
    df2$max =ifelse (df2$firstR > df$cs[i] , df$max[i+1] , df2$max)
  }
  df2$min =ifelse (df2$firstR <= df$cs[1] , df$min[1] , df2$min)
  df2$max =ifelse (df2$firstR <= df$cs[1] , df$max[1] , df2$max)
  df2$secondR=-1
  for (i in 1:nrow(df2))
  {
    df2$secondR[i]=runif(1, df2$min[i], df2$max[i])  
  }
  return(df2$secondR)
}

vlookup=function(x=c(0.9,2,3,2,1), lookup=data.frame(c(0,1.2,2,3), c(1.1,2.1,3.1,4.1), c("D","C","B","A")),
                 col_index=3)
{
  retV=""
  for (i in 1:length(x))
  {  #i=1
    for (k in 1:nrow(lookup)) 
    { 
      if (k!=nrow(lookup) && lookup[k,1]<= x[i] && x[i]<lookup[k,2]) 
      {
        retV[i]<-as.character(lookup[k,col_index])
      } else if (k==nrow(lookup) && lookup[k,1]<= x[i] && x[i]<=lookup[k,2]) 
      {
        retV[i]<-as.character(lookup[k,col_index])
      }
    }
  }
  return (retV)
}


background=function(file, smtp="xxx")
{
  file.exists(file)
  seq=getNow()
  outfile=paste("outfile",seq,".txt",sep="")
  cmd=paste(getDropboxPath(),"_zEXEC/command/r.sh", sep="")
  if (smtp=="polyu")  cmd=paste(getDropboxPath(),"_zEXEC/command/r_polyu.sh", sep="")
  stringCommand=paste(cmd, file, seq, "&")
  system(stringCommand)
  return(seq)
}

wapply <- function(x, width, by = NULL, FUN = NULL, ...)
{
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  
  lenX <- length(x)
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
  
  OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
  OUT <- base:::simplify2array(OUT, higher = TRUE)
  return(OUT)
}

substrRight= function(x, n)
{
  substr(x, nchar(x)-n+1, nchar(x))
}

u.substr=function(x, start, stops)
{ 
  enddot=""
  nc=nchar(x)
  if (stops+2 <=nc) enddot=".."
  else if ( (stops+1) == nc) enddot="."
  else enddot=""
  paste(substr(x, start, stops-nchar(enddot)) ,enddot,sep="")
}

rbindlist.df=function(...)
{
  require(data.table)
  return (data.frame(rbindlist(list(...))))
}

is.na.data.frame=function (x) 
{
  if (class(x)!="data.frame") stop("x is not a data.frame")
  y <- do.call("cbind", lapply(x, "is.na"))
  if (.row_names_info(x) > 0L) 
    rownames(y) <- row.names(x)
  any(y)
}

#getDigit(234.34, 1)==4
#getDigit(234.34, 2)==3
#getDigit(234.34, 3)==2
#getDigit(234.34, 4)==0
#getDigit(234.54, -1)==5
#getDigit(234.5432, -2)==4
#getDigit(234.5439, -3)==3
#getDigit(234.5439, -4)==9
#getDigit(234.5439, -5)==9

getDigit=function(x, digit=0)
{ # x=234.34
  if (digit==0) return (x)
  sign=sign(x)
  x=abs(x)
  if (digit>0)
  { 
    x=floor(x)
    x=x/(10^digit) 
    x=x%%1
    x=floor(x*10+0.000001)
  }
  if (digit<0)
  {    # x=234.5432 ; digit=2
    digit=abs(digit)
    for (i in 1:digit)
    {
      x=x%%1  
      x=x*10   
    }
    x=floor(x+0.000001)
  }
  return( sign*x)
}



#d=data.frame(a=c(1,2,3,4,2,2), b=c(1,2,3,6,5,6), d=c(1,2,3,2,5,6))
#is.na.data.frame(d)

na.fix=function(x, rowS=1, colS=2, default=NULL,method="simple")
{
  if (!(method %in% c("simple", "linear"))) stop("error: na.fix")
  numFixed=0
  for (i in rowS:nrow(x))
  {
    for (j in colS:ncol(x)) 
    {
      if ( is.na(x[i,j]) ) 
      {
        if (i==1) x[i,j]=x[(i+1),j]
        
        if (i!=1 && "simple"==method)
        {
          numFixed= numFixed+1
          if (is.null(default)) 
          {
            if (i!=1) x[i,j]=x[(i-1),j] #use previous value
          }
          else x[i,j]=default
        }
        if (i!=1 && "linear"==method)
        {
          ii=i
          while( is.na(x[ii,j]) && ii<=nrow(x) ) ii=ii+1
          x.print(x[ii,j])
          startPt=as.numeric(x[i-1,j])
          endPt=as.numeric(x[ii,j])
          numInterval=ii-(i-1)
          x.print(numInterval)
          increV=(endPt-startPt)/numInterval
          for (iii in i:(ii-1))   x[iii,j]=x[iii-1,j]+increV
          numFixed= numFixed + numInterval -1
        }
      }
    }
  }
  return (list(x, numFixed))
}



isSubset=function(A, B)
{
  nrowA=nrow(A)
  if (nrowA>1) 
  {
    stop("ERROR: This version of subset not support. Check code!" )
  }
  nrowB=nrow(B)
  ncolB=ncol(B)
  for (i in 1:nrowB)
  {
    counter=0
    for (k in 1:ncolB)
    {
      if (A[1,k]==B[i,k]) counter=counter+1
    }
    if (counter==ncolB) return (TRUE)
  }
  return (FALSE)
}

xts.subset=function(x, from=NULL, to=NULL)
{ if (is.null(to)) {rg=range(index(x)); to=rg[2]}
  if (is.null(from)) from=rg[1]
  subset(x, from<=index(x) & index(x)<=to)
}

self=function(x)  #utility
{
  if (length(x)==1) return(x[1])
  
  pos=strtoi(x[1])+1
  return(x[pos])
}

removeFile=function(dir=NULL, ext=".png")
{
  beforeDir=getwd()
  if (is.null(dir)==FALSE) setwd(dir)
  file=list.files()
  if (length(file)!=0)
  {
    for (i in 1:length(file))
      if (substrRight(file[i],4)==ext) file.remove(file[i])
  }
  setwd(beforeDir)
}

renameList=function(x, old, new)
{
  # e.g. tp=data.frame(a=1, b=2, oldColName=3)
  #      names(tp)=renameList(tp, "oldColName","newColName" )
  idx=which(names(x)==old)
  allNames=names(x) 
  allNames[idx]=new
  return(allNames)
}

addColumn=function(x, colVectorIndex, ind ,na.rm = FALSE ) 
{ # for dataframe ; not for xts
  # 22 July
  #Example of dataframe
  #   a=data.frame(); a=rbind(a, data.frame(a=1, b=2, c=3))
  #   a=rbind(a, data.frame(a=2, b=3, c=8)); a=rbind(a, data.frame(a=3, b=4, c=9))
  #   addColumn(a, c(2,3), -1)
  #   addColumn(a, c(2,3), -1, na.rm=TRUE)
  for (i in 1:length(colVectorIndex))
  {
    colName=colVectorIndex[i]
    if (is.numeric(colName))
    {
      xc=colName
      colName=colnames(x[xc])
    }
    xc=which(colnames(x)==colName)
    if (ind<0)
    {
      newColName=paste(colName,"N", abs(ind), sep="")
      x[newColName]=c(x[ind,xc],rep(NA,abs(ind)))
    }
    if (ind>0)
    {
      newColName=paste(colName,"P", abs(ind), sep="")
      x[newColName]=c(rep(NA,abs(ind)), x[c(1: (nrow(x)-ind)),xc])
    }    
  }
  if ( TRUE==na.rm ) { x=subset(x, !is.na( x[ , newColName]))}
  return (x)
} 


getLongColumnName=function(xts)
{
  colnames(xts)[1]="Open"
  colnames(xts)[2]="High"
  colnames(xts)[3]="Low"
  colnames(xts)[4]="Close"
  colnames(xts)[5]="Volume"
  colnames(xts)[6]="Adjusted"
  return(xts)
}

getownCloudDir=function(x="") {
  path=getownCloudPath()
  if (file.exists(path)) return(paste(path,x,sep=""))
  stop("error: no cloud directory found")  
}

getownCloudPath=function() {
  path="/Users/admin/ownCloud/"
  if (file.exists(path)) return(path)
  path="/home/kimman/ownCloud/"
  if (file.exists(path)) return(path)
  stop("error: no cloud directory found")
}

getDropboxDir=function(x="") {
  path=getDropboxPath()
  if (file.exists(path)) return(paste(path,x,sep=""))
  if (file.exists("/Users/admin/Documents/Dropbox")) return("/Users/admin/Documents/Dropbox/")
  stop("error: no cloud directory found")
}

getDropboxPath=function()
{
  dirPath=""
  if (file.exists("/Users/admin/Documents/Dropbox")) dirPath="/Users/admin/Documents/Dropbox/"
  if (isMac()) 
  {
    # dirPath="/Users/admin/Documents/_shared/"
    dirPath="/Users/admin/Documents/Dropbox/"
    if (isCloud()) dirPath=""
  }
  else   
  {   
    polyuMachine="C://Documents and Settings//Administrator//Desktop//public//Dropbox//"
    notebookMachine="C://Users//homa//Dropbox//" 
    asusMachine="C://Users//homa//Dropbox//" 
  }
  return (dirPath)
}

getPath=function()
{
  if (is.windows()) return("c://Users//admin//Documents//Dropbox//_data//")
  dirPath="/Users/admin/Documents/Dropbox/_data/"
  if (isMac()) 
  {
    # dirPath="/Users/admin/Documents/_shared/"
    dirPath="/Users/admin/Documents/Dropbox/_data/"
    if (isCloud()) dirPath=""
  }
  else   
  {   
    #polyuMachine="C://Documents and Settings//Administrator//Desktop//public//_shared"
    #notebookMachine="C://Users//homa//Desktop//public//_shared"
    polyuMachine="C://Documents and Settings//Administrator//Desktop//public//Dropbox//_data"
    notebookMachine="C://Users//homa//Dropbox//_data" 
    asusMachine="C://Users//homa//Dropbox//_data" 
    if (file.exists(polyuMachine))
    {
      dirPath=paste(polyuMachine,"//",sep="")
    } else if (file.exists(notebookMachine))
    {
      dirPath=paste(notebookMachine,"//",sep="")
    } else if (file.exists(asusMachine))
    {
      dirPath=paste(asusMachine,"//",sep="")
    }else
    {
      #dirPath=""
    }
  }
  return (dirPath)
}

renameCol=function(x, oldNames, newNames)
{
  allNames=names(x)
  for (i in 1:length(oldNames))
  {
    idx=which(allNames==oldNames[i])
    allNames[idx]=newNames[i]
  }
  return(allNames)
}
# k=data.frame(a=c(1,2), b=c(2,3),c=c(2,3))
# names(k)=renameCol(k, c("a", "c"), c("x.a", "x.c"))


addXtsCol=function(t,colname="new_col",default=0)
{
  #eg x=getSymbols("^hsi",auto.assign=FALSE,from="2014-01-01")
  #   head(addXtsCol(x, colname="new_col", default=1) ,n=2)
  cn=c(colnames(t),colname)
  t$new=0
  colnames(t)=cn
  return (t)
}

addXtsCol2=function(x.ts, colname, v)
{
  x.ts$ttxxttxx=v
  tmpNam=names(x.ts)
  dx=which(tmpNam=="ttxxttxx")
  tmpNam[dx]=colname
  names(x.ts)=tmpNam
  return (x.ts)
}

addXtsLagCol=function(x.ts, extendedCol="c", lagV=c(1))
{
  for (i in index(extendedCol))
  {
    for (lg in index(lagV))
    {
      tmpCol=lag(x.ts[, extendedCol[i]], lagV[lg])
      tmpColName=paste(extendedCol[i],lagV[lg], sep="")
      x.ts=addXtsCol2(x.ts, tmpColName, tmpCol)
    }
  }
  return(x.ts)
}


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

is.defined=function(...) isDefined(...)
isDefined=function(nameObj)
{
  varName=ls(name = ".GlobalEnv" )
  if (any(nameObj==varName) ) return(TRUE)
  return (FALSE)
}

getLastDate=function(x, dateIndex=NULL) {return (u.last(x=x,dateX=dateIndex))}
getLastDate_OLD=function(x, dateIndex=NULL)
{
  if ("xts" %in% class(x) ) 
  {
    return (index(tail(x, n=1) ))
  }
  if ("data.frame" %in% class(x) )
  {
    dx=dateIndex
    if (is.null(dx)) dx=which(names(x)=="date")
    hd=as.Date(head(x[,dx],n=1))
    tl=as.Date(tail(x[,dx],n=1))
    return (max(hd, tl))
  }
  stop("error: not know how to get last date")
} 

shot=function()
{
  if (isMac()) pngFile="/Users/admin/Pictures/screenShot.png"
  else if (isASUS()) pngFile="C://Users//ASUS//Pictures//screenShot.png"
  else pngFile="C://Users//homa//Pictures//screenShot.png"
  dev.copy(png, pngFile)
  dev.off()
  return (pngFile)
}

dg_old=function(funName)
{
  #funName="getCorr"
  dput(getAnywhere(funName)$objs[[1]], file="debugCache.txt")
  funLine=readLines("debugCache.txt")
  cnt=1;
  for (lineNum in length(funLine):5)
  {
    if (!( substr(funLine[lineNum], 5,5)=="}" ||
             substr(funLine[lineNum], 5,8)=="else" ||
             substr(funLine[lineNum], 5,5)==" " ))
      #if (substr(funLine[lineNum], 5,7)=="for" || 
      #    substr(funLine[lineNum], 5,6)=="if"  ||
      #    substr(funLine[lineNum], 5,9)=="while" )
    {
      msg=paste("print('debug ",cnt,":",funLine[lineNum],"')",sep="")
      cnt=cnt+1
      funLine=insert.at (funLine, lineNum-1, msg)
    }
  }    
  writeLines(funLine, "debugCache2.txt")
  return(dget("debugCache2.txt"))
}

insert.at <- function(a, pos, ...){
  dots <- list(...)
  stopifnot(length(dots)==length(pos))
  result <- vector("list",2*length(pos)+1)
  result[c(TRUE,FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos+1)))
  result[c(FALSE,TRUE)] <- dots
  unlist(result)
}

dg=function(funName)
{
  #funName="getCorr"
  funName=deparse(substitute(funName))
  funName=gsub("\"","",funName)
  
  dput(getAnywhere(funName)$objs[[1]], file="debugCache.txt")
  funLine=readLines("debugCache.txt")
  cnt=1;
  for (lineNum in length(funLine):5)
  {
    fL=funLine[lineNum]
    if (!( substr(fL, 5,5)=="}" ||
             substr(fL, 5,8)=="else" ))
      #if (substr(funLine[lineNum], 5,7)=="for" || 
      #    substr(funLine[lineNum], 5,6)=="if"  ||
      #    substr(funLine[lineNum], 5,9)=="while" )
    {
      if (substr(fL, 5,5)!=" ")
      {
        msg=paste("print('dg_1 ",cnt,":",fL,"')",sep="")
        cnt=cnt+1
        funLine=insert.at (funLine, lineNum-1, msg)
      }  
      if ( substr(fL, 5,5)==" " &&
             !(substr(fL, 9,9)==" " || 
                 substr(fL, 9,12)=="else"))
      {  fL='batchR, sep = ""))'
         if ( gregexpr( "\\(", fL)[[1]][1]==gregexpr( "\\)", fL)[[1]][1] &&
                length(gregexpr( "\\(", fL)[[1]])==
                length(gregexpr( "\\)", fL)[[1]]))
         {
           msg=paste("print('dg_2 ",cnt,":",funLine[lineNum],"')",sep="")
           cnt=cnt+1
           funLine=insert.at (funLine, lineNum-1, msg)
         }
      }
    }
  }    
  writeLines(funLine, "debugCache2.txt")
  return(dget("debugCache2.txt"))
}

#### __ERROR HANDLING__ ####
handle=function(FUN, ..., errorValue=-999)
{
  FUN <- match.fun(FUN)
  tryCatch(FUN(...), error = function(e) {print(paste("handle:",e)); return(errorValue)}, finally=function() {retV} ) 
}

averageRun=function(FUN, ..., repeater=2, errorV=-999)
{
  avgV=NULL
  for (i in 1: repeater)
  {
    thisPerf=NA
    tmp=NULL  #revised on 10/11/2013
    tmp=handle(FUN, ..., errorValue=errorV)
    if (is.null(tmp)) return(errorV)
    if (class(tmp)=="list") thisPerf=tmp[[1]][1]
    if (class(tmp)!="list" && -999==tmp) thisPerf=tmp
    avgV=c(avgV, thisPerf)
    print(paste(timelog(),"averageRun(", i,"):",  thisPerf) )
  }
  if (is.null(avgV)) return (errorV)
  if (all(is.na(avgV))) return (errorV)
  print(avgV)
  tmp=mean(avgV, na.rm=TRUE)
  if (is.na(tmp)) return (errorV)
  return(tmp)
} 

#### __EXTERNAL COMMAND__ ####
synCode=function(...)
{
  if (isMac()) 
  {
    command="/Users/admin/Documents/command/cloudbackup_warrant.sh"
    system(command) 
  }
}  

tips=function(...)
{
  mail(..., tips=TRUE, addr=globalDefaultEmailAddr)
}

say=function(text="", voice="female", wait=0, useSayCommand=FALSE)
{ 
  if (wait>0) u.Sys.sleep(wait)
  if (useSayCommand==TRUE)
  {
    if (voice=="female")  
    {
      fullcommand=paste("say -v Victoria -r 130 ",text,sep="")
    } else fullcommand=paste("say -r 130 ",text,sep="")
    system(fullcommand)
    return (0)
  }
  
  if (voice!="female")
  { 
    command="C://Users//admin//Documents//Dropbox//StockAnalysis//app//speakquote//malesay.bat"
    if (!file.exists(command)) command="/Users/admin/Documents/StockAnalysis/app/speakquote/malesay.sh"
  }
  else
  {
    command="C://Users//admin//Documents//Dropbox//StockAnalysis//app//speakquote//femalesay.bat"
    if (!file.exists(command)) command="/Users/admin/Documents/StockAnalysis/app/speakquote/femalesay.sh" 
  }
  if (file.exists(command))
  {
    commandString=paste(command," ", text)
    system(commandString) 
  }
}


mail=function(subject="hello world", pic=NULL, body=c(0,0,0,0), tips=FALSE, addr=globalDefaultEmailAddr,smtp="",sendername="")
{
  # sendername is no longer used
  attachment=list(pic)
  mail2(subject=subject,attachment=attachment, body=body, tips=tips, addr=addr, smtp=smtp)
}





mail2_OLD=function(subject="hello world", attachment=list(), pic=NULL, pic2=NULL, pic3=NULL, pic4=NULL, body=c(0,0,0,0), tips=FALSE, addr=globalDefaultEmailAddr,smtp="netvigator",senderAddr="no.reply@netvigator.com", engine="java")
{
  force=0
  if (smtp=="auto")
  {
    smtp="gmail"
  } else
  {
    smtp="netvigator" 
  } 
  if (force==1) smtp="gmail"
  
  #if (engine=="java") subject=substr(subject, 1, 56)
  fileL=NULL
  body=na.omit(body)
  if (is.darwin() || is.linux() ) 
  { 
    if (all(body==c(0,0,0,0))) body=data.frame(hostname=getHostName(),time=timelog())
    #fileL="/Users/admin/Documents/_shared/tmp_email.txt"
    fileL=paste0(getwd(),"/tmp_email.txt")
    dailymail_exe="/Users/admin/Documents/Dropbox/StockAnalysis/app/StockMail/dailymail_two.sh "
    mailtips_exe="/Users/admin/Documents/Dropbox/StockAnalysis/app/StockMail/mailtips_two.sh "
  }
  if (isWindows() )
  {
    if (all(body==c(0,0,0,0))) body=c(from=3,t=timelog())
    fileL="C://Users//admin//temp//tmp_email.txt" 
    dailymail_exe="C://Users//admin//Documents//Dropbox//StockAnalysis//app//StockMail//dailymail_two.bat "
    mailtips_exe ="C://Users//admin//Documents//Dropbox//StockAnalysis//app//StockMail//mailtips_two.bat "  
  }
  
  if (is.null(fileL)) stop("cannot recognize the machine")
  
  cat(subject, file=fileL,sep="\n")
  if (class(body)=="matrix" || class(body)=="data.frame")  
  {
    write.table(format(body,justify="none"), file="tmp.txt")
    cmd=paste("cat tmp.txt >> ", fileL, sep="")
    if (isWindows() )
    {
      zz <- file("tmp.txt","w")
      write(zz, fileL, append=TRUE)
      
    } else
    {
      system(cmd)
    }
    file.remove("tmp.txt")
    #write.table(format(body,justify="none"), file=fileL, append=TRUE)
  } else cat(body, file=fileL, append=TRUE)
  commandString=paste(dailymail_exe, smtp, senderAddr, addr, fileL)
  if (TRUE==tips) commandString=paste( mailtips_exe , smtp, senderAddr, addr, fileL)
  if ("py"==engine) commandString=paste( "python", paste0(getDropboxDir(),"AP/py/stockmail.py") , smtp, senderAddr, addr, fileL)
  if (is.null(pic)==FALSE && u.file.exists(pic))  commandString=paste(commandString, " ", pic)
  if (is.null(pic2)==FALSE && u.file.exists(pic2))  commandString=paste(commandString, " ", pic2)
  if (is.null(pic3)==FALSE && u.file.exists(pic3))  commandString=paste(commandString, " ", pic3)
  if (is.null(pic4)==FALSE && u.file.exists(pic4))  commandString=paste(commandString, " ", pic4)
  if (is.null(fileL)==FALSE && length(fileL)>0)
  {
    if (length(attachment)!=0)
    {
      for ( i in 1:length(attachment))
      {
        if (is.null(attachment[[i]])==FALSE && u.file.exists(attachment[[i]])) commandString=paste(commandString, " ", attachment[[i]])
      }
    }
  }
  
  
  if (smtp=='gmail')
  {
    print('smtp')
    body=as.character(body)
    body=paste(body, collapse=" ")
    if (!file.exists('/d/credential.RData')) {print("no credential.RData"); return("ERROR"); }
    #save(gmail_username, gmail_password, file ='credential.RData')
    load('/d/credential.RData')
    attach.files=NULL
    if (is.null(pic)==FALSE && u.file.exists(pic))    attach.files=c(attach.files, pic)
    if (is.null(pic2)==FALSE && u.file.exists(pic2))  attach.files=c(attach.files, pic2)
    if (is.null(pic3)==FALSE && u.file.exists(pic3))  attach.files=c(attach.files, pic3)
    if (is.null(pic4)==FALSE && u.file.exists(pic4))  attach.files=c(attach.files, pic4)
    if (length(attachment)>0) 
    { 
      for (i in 1:length(attachment)) 
      {
        if (u.file.exists(attachment[[i]])) attach.files=c(attach.files, attachment[[i]]) 
      }
    }
    #attach.files = c("./download.log", "upload.log",
    if (is.null(attach.files))
    {
      print("no attach.files")
      #print(gmail_username)
      #print(gmail_password)
      #print(addr)
      #print(senderAddr)
      #print(body)
      #print(subject)
      send.mail(from = 'nohongkong.com@gmail.com',
                to = c(addr),
                replyTo = c( paste0("KL <",senderAddr,">")),
                subject = subject,
                body = body,
                smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = gmail_username , passwd = gmail_password , ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
    } else
    {
      send.mail(from = 'nohongkong.com@gmail.com',
                to = c(addr),
                replyTo = c( paste0("KL <",senderAddr,">")),
                subject = subject,
                body = body,
                smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = gmail_username , passwd = gmail_password , ssl = TRUE),
                authenticate = TRUE,
                html = TRUE,
                send = TRUE,
                attach.files = attach.files)
    }
  } else
  { # NOT GMAIL
    print(commandString)
    system(commandString)
  }
}



mail2=function(subject="hello world", attachment=list(), pic=NULL, pic2=NULL, pic3=NULL, pic4=NULL, body=c(0,0,0,0), tips=FALSE, cc='NONE', addr=globalDefaultEmailAddr,smtp="netvigator",senderAddr="no.reply@netvigator.com", engine="java")
{
  if (engine=="emayili")  
  {
    print(engine)
    mail_emayili(subject=subject, attachmentList=attachment, body=body, cc=cc, addr=addr, senderAddr=senderAddr, engine=engine)
    return(1)
  }  
  if (cc!='NONE' && engine!='JAVA') 
  {
    print("cc is only for java")
  }
  force=0
  if (smtp=="auto")
  {
    smtp="gmail"
  } else
  {
    smtp="netvigator" 
  } 
  if (force==1) smtp="gmail"
  
  #if (engine=="java") subject=substr(subject, 1, 56)
  fileL=NULL
  body=na.omit(body)
  ccFlag=0
  if (is.darwin() || is.linux() ) 
  { 
    if (all(body==c(0,0,0,0))) body=data.frame(hostname=getHostName(),time=timelog())
    #fileL="/Users/admin/Documents/_shared/tmp_email.txt"
    fileL=paste0(getwd(),"/tmp_email.txt")
    dailymail_exe="/Users/admin/Documents/Dropbox/StockAnalysis/app/StockMail/dailymail_cc.sh "
    mailtips_exe="/Users/admin/Documents/Dropbox/StockAnalysis/app/StockMail/mailtips_cc.sh "
  }
  if (isWindows() )
  {
    if (all(body==c(0,0,0,0))) body=c(from=3,t=timelog())
    fileL="C://Users//admin//temp//tmp_email.txt" 
    dailymail_exe="C://Users//admin//Documents//Dropbox//StockAnalysis//app//StockMail//dailymail_cc.bat "
    mailtips_exe ="C://Users//admin//Documents//Dropbox//StockAnalysis//app//StockMail//mailtips_cc.bat "  
    ccFlag=1
  }
  
  if (is.null(fileL)) stop("cannot recognize the machine")
  
  cat(subject, file=fileL,sep="\n")
  if (class(body)=="matrix" || class(body)=="data.frame")  
  {
    write.table(format(body,justify="none"), file="tmp.txt")
    cmd=paste("cat tmp.txt >> ", fileL, sep="")
    if (isWindows() )
    {
      zz <- file("tmp.txt","w")
      write(zz, fileL, append=TRUE)
      
    } else
    {
      system(cmd)
    }
    file.remove("tmp.txt")
    #write.table(format(body,justify="none"), file=fileL, append=TRUE)
  } else cat(body, file=fileL, append=TRUE)
  
  commandString=paste(dailymail_exe, smtp, senderAddr, addr, cc,  fileL)
  if (ccFlag==1) commandString=paste(dailymail_exe, smtp, senderAddr, addr, cc,  fileL)
  
  if (TRUE==tips && ccFlag==1) commandString=paste( mailtips_exe , smtp, senderAddr, addr, cc, fileL)
  if (TRUE==tips && ccFlag==0) commandString=paste( mailtips_exe , smtp, senderAddr, addr,     fileL)

  if ("py"==engine && ccFlag==1) commandString=paste( "python", paste0(getDropboxDir(),"AP/py/stockmail.py") , smtp, senderAddr, addr, fileL)
  if ("py"==engine && ccFlag==0) commandString=paste( "python", paste0(getDropboxDir(),"AP/py/stockmail.py") , smtp, senderAddr, addr, fileL)
  if (is.null(pic)==FALSE && u.file.exists(pic))  commandString=paste(commandString, " ", pic)
  if (is.null(pic2)==FALSE && u.file.exists(pic2))  commandString=paste(commandString, " ", pic2)
  if (is.null(pic3)==FALSE && u.file.exists(pic3))  commandString=paste(commandString, " ", pic3)
  if (is.null(pic4)==FALSE && u.file.exists(pic4))  commandString=paste(commandString, " ", pic4)
  if (is.null(fileL)==FALSE && length(fileL)>0)
  {
    if (length(attachment)!=0)
    {
      for ( i in 1:length(attachment))
      {
        if (is.null(attachment[[i]])==FALSE && u.file.exists(attachment[[i]])) commandString=paste(commandString, " ", attachment[[i]])
      }
    }
  }
  
  
  if (smtp=='gmail')
  {
    print('smtp')
    body=as.character(body)
    body=paste(body, collapse=" ")
    if (!file.exists('/d/credential.RData')) {print("no credential.RData"); return("ERROR"); }
    #save(gmail_username, gmail_password, file ='credential.RData')
    load('/d/credential.RData')
    attach.files=NULL
    if (is.null(pic)==FALSE && u.file.exists(pic))    attach.files=c(attach.files, pic)
    if (is.null(pic2)==FALSE && u.file.exists(pic2))  attach.files=c(attach.files, pic2)
    if (is.null(pic3)==FALSE && u.file.exists(pic3))  attach.files=c(attach.files, pic3)
    if (is.null(pic4)==FALSE && u.file.exists(pic4))  attach.files=c(attach.files, pic4)
    if (length(attachment)>0) 
    { 
      for (i in 1:length(attachment)) 
      {
        if (u.file.exists(attachment[[i]])) attach.files=c(attach.files, attachment[[i]]) 
      }
    }
    #attach.files = c("./download.log", "upload.log",
    if (is.null(attach.files))
    {
      print("no attach.files")
      #print(gmail_username)
      #print(gmail_password)
      #print(addr)
      #print(senderAddr)
      #print(body)
      #print(subject)
      send.mail(from = 'nohongkong.com@gmail.com',
                to = c(addr),
                replyTo = c( paste0("KL <",senderAddr,">")),
                subject = subject,
                body = body,
                smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = gmail_username , passwd = gmail_password , ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
    } else
    {
      send.mail(from = 'nohongkong.com@gmail.com',
                to = c(addr),
                replyTo = c( paste0("KL <",senderAddr,">")),
                subject = subject,
                body = body,
                smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = gmail_username , passwd = gmail_password , ssl = TRUE),
                authenticate = TRUE,
                html = TRUE,
                send = TRUE,
                attach.files = attach.files)
    }
  } else
  { # NOT GMAIL
    print(commandString)
    system(commandString)
  }
}

mail_emayili=function (subject = "hello world", attachmentList = list(), pic = NULL, 
                       pic2 = NULL, pic3 = NULL, pic4 = NULL, body = c(0, 0, 0, 
                                                                       0), tips = FALSE, cc = "NONE", addr = globalDefaultEmailAddr, 
                       smtp = "netvigator", senderAddr = "no.reply@netvigator.com", 
                       engine = "emayili") 
{
  require(emayili)
  if (engine == "emayili") {
    body = paste(body, collapse = " ")
    if (length(attachmentList) > 1) body=paste0(body,"\n\nonly one attachment is allowed")
    if (smtp=='gmail')
    {
      smtphost='smtp.gmail.com'
      login = "redmine.ace.uic@gmail.com"
      port = 587
      passwd = "KIMman123"
    }
    if (smtp=='netvigator') 
    {
      smtphost='smtp.netvigator.com'
      login = "pollyleung108"
      port = 25
      passwd = "Net@l1ttlepr1nce"
    }
    smtpobj <- server(host = smtphost, port = port, username = login, 
                      password = passwd)
    email <- envelope()
    email <- email %>% from(senderAddr) 
    email <- email %>% to(addr)
    if (cc != "NONE") 
      email <- email %>% cc(cc)
    email <- email %>% subject(subject)
    email <- email %>% text(body)
    if (length(attachmentList) != 0) {
      email <- email %>% attachment(path = attachmentList[[1]])
    }
    smtpobj(email, verbose = TRUE)
  }
}

tips2=function(...) mail2(..., tips=TRUE, senderAddr="tips", addr="cskmlui@hotmail.com")


writeFile=function(text="", filename="email.txt")
{
  fileConn<-file("email.txt")
  writeLines(text, fileConn)
  close(fileConn)
}

play=function(file=NULL, numPlay=1)
{
  #v3
  if (is.null(file)==TRUE) return()
  result=grep(".mp3", file)
  if (length(result)==0) file=paste(file,".mp3",sep="")
  if (!file.exists(file)) 
  {
    tmp=paste("Dropbox/DCIM/music/",file,sep="")
    file=uniPath(tmp)
    if (!file.exists(file)) stop("error : mp3 file does not exists")
  }
  play_exe=NULL
  if (isMac()) 
  {
    play_exe="/Users/admin/Documents/StockAnalysis/app/PlayMP3/play.sh "
  }
  if (isPolyu() || isLenovo() || isASUS())
  {
    play_exe="C://Users//admin//Documents//Dropbox//StockAnalysis//app//PlayMP3//play.bat "
  }
  if (is.null(play_exe)) stop("cannot recognize the machine")
  
  commandString=paste(play_exe, file, numPlay)
  system(commandString)
}

speak=function(text)
{
  #v3
  text=paste("\"", text,"\"",sep="")
  play_exe=NULL
  if (isMac()) 
  {
    play_exe="/Users/admin/Documents/StockAnalysis/app/PlayMP3/play.sh "
  }
  if (isPolyu() || isLenovo() || isASUS())
  {
    play_exe="C://Users//admin//Documents//Dropbox//StockAnalysis//app//TTS//TTS.bat "
  }
  if (is.null(play_exe)) stop("cannot recognize the machine")
  
  commandString=paste(play_exe, text)
  system(commandString)
}

#### __TEMP DATA MANAGEMENT__ ####


housekeepPara=function()
{
  fileLocation=uniPath("Dropbox/_data/para.RData")
  fileNotFound=FALSE
  if (file.exists(fileLocation)==FALSE)  fileNotFound=TRUE
  if (TRUE==fileNotFound ) stop("(UNEXPECTED) Paramenter File Not Found")
  load(file=fileLocation)
  totalRecord=length(paraKey)
  for (i in totalRecord: 1) 
  {
    if (is.null(paraExp[[i]]) || Sys.Date()>paraExp[[i]] ) 
    {
      paraKey[[i]]  =NULL 
      if (isnot.null(paraExp[[i]])) paraExp[[i]]=NULL
      paraValue[[i]]=NULL
    }
  }
  save(paraKey, paraValue, paraExp, file=fileLocation, version=globalsaveversion)
}


getCache=function(key="text")
{
  if (isDefined("machineUseCache")==FALSE) machineUseCache <<- list()
  return( machineUseCache[[key]])
}

addCache=function(key="text", value="")
{
  oldValue=getCache(key)
  newValue=paste0(oldValue,value)
  setCache(key,value=newValue)
}

setCache=function(key="text", value=NULL, reset=FALSE , PK=NULL)
{
  if (is.null(value)) stop("value cannot be NULL")
  
  if (!is.null(PK))
  {
    dbV=getCache(key)
    nrowdbV=nrow(dbV)
    if (is.null(nrowdbV))  setCache(key, value, PK=NULL)
    else 
    {
      if (!isSubset(value[PK],dbV[PK])) dbV=rbind(dbV, value)
      setCache(key, dbV,PK=NULL)
      return (nrow(dbV)-nrowdbV)
    }
  }
  
  
  if (isDefined("machineUseCache")==FALSE || TRUE==reset) 
  {
    machineUseCache <<- list()
  } 
  machineUseCache[[key]] <<- value
  return(1) #mean one row added
}
 

getPara=function(key, filename="para.RData", fileLocation=NULL)
{
  #if (isDefined("machineUseRedis") && machineUseRedis==TRUE) return(redisGet(key))
  
  #fileLocation="para.RData"
  if (is.null(fileLocation)) fileLocation=uniPath(paste("Dropbox/_data/",filename,sep=""))
  else fileLocation=paste(fileLocation,filename,sep="")
  fileNotFound=FALSE
  if (file.exists(fileLocation)==FALSE)  fileNotFound=TRUE
  if (TRUE==fileNotFound ) return (NULL)
  load(file=fileLocation)
  totalRecord=length(paraKey)
  if (totalRecord==0) return(NULL)
  for (i in 1: totalRecord)
    if (key == paraKey[[i]] && Sys.Date()<=paraExp[[i]] ) return(paraValue[[i]])
  return (NULL)
}

setPara=function(key, value=NULL, reset=FALSE, exp=Sys.Date(), filename="para.RData",PK=NULL,fileLocation=NULL)
{
  if (is.null(value)) stop("value cannot be NULL")
  
  if (isnot.null(PK))
  {
    dbV=getPara(key, filename=filename, fileLocation=fileLocation)
    nrowdbV=nrow(dbV)
    if (is.null(nrowdbV)) setPara(key, value, filename=filename,fileLocation=fileLocation)
    else 
    {
      if (!isSubset(value[PK],dbV[PK])) dbV=rbind(dbV, value)
      setPara(key, dbV,filename=filename, fileLocation=fileLocation)
      return (nrow(dbV)-nrowdbV)
    }
  }
  
  #if (isDefined("machineUseRedis") && machineUseRedis==TRUE) 
  #{
  #  if (class(value)=="character" && length(value)==1) return(redisSet(key, charToRaw(value)))
  #  return(redisSet(key, value))
  #}
  
  paraKey=list()
  paraValue=list()
  paraExp=list()
  fileNotFound=FALSE
  if (is.null(fileLocation)) fileLocation=uniPath(paste("Dropbox/_data/",filename,sep=""))
  else fileLocation=paste(fileLocation,filename,sep="")
  
  if (file.exists(fileLocation)==FALSE)  fileNotFound=TRUE
  if ( TRUE==fileNotFound  || TRUE==reset )
  {
    paraKey[[1]]   = key
    paraValue[[1]] = value
    paraExp[[1]]   = exp 
    save(paraKey, paraValue, paraExp, file=fileLocation, version=globalsaveversion)
  } else
  {
    load(file=fileLocation)
    totalRecord=length(paraKey)
    if (totalRecord!=0)
    {
      for (i in 1: totalRecord)
      {
        if (key == paraKey[[i]]) 
        {
          paraValue[[i]]=value
          paraExp[[i]]  =exp 
          save(paraKey, paraValue, paraExp,  file=fileLocation, version=globalsaveversion)
          return(0) #mean update
        }
      }
    }
    paraKey[[(totalRecord+1)]]=key
    paraValue[[(totalRecord+1)]]=value
    paraExp[[(totalRecord+1)]]=exp
    save(paraKey, paraValue, paraExp,  file=fileLocation, version=globalsaveversion)
  }
  return(1) #mean one row added
}

 

#(length(grep(pattern = "^hk","hkHSI" , ignore.case = TRUE))==1)
#(length(grep(pattern = "hk$", "ddd.hkk", ignore.case = TRUE))==1)
#sinaCode="0001.HK"
#### __DATA_RETRIEVAL__ ####
# sinaCode="hk03988"  u.getQuote("^HSI") u.getQuote("0001.hk") u.getQuote(12)


getSess=function(selectD=NULL , removeD=NULL)
{
  selectD=as.Date(selectD)
  globalUseHTTP=FALSE
  globalCache="/Users/admin/cache/"
  filename=paste0(uniPath("Dropbox/_data/"),"sess.RData")
  
  load(filename)
  sess=sess[isnot.na(sess$date), ]
  if (isnot.null(removeD)) 
  {
    sess=sess[ !(sess$date %in% as.Date(removeD)), ]
    save(sess, file=filename, version=globalsaveversion)
  }
  if (is.null(selectD)) return(sess)
  selectD.org=selectD
  selectD=selectD[ !(selectD %in% sess$date)]
  if (length(selectD)==0) return (sess)
  for (iselectD in selectD)
  {
    rtdat=subset(rtdata(date=iselectD, tablename="rtday", useHTTP=FALSE, cacheDir=globalCache), batch>=29)
    if (nrow(rtdat) != 0)
    {
      tmpSess = NULL
      coln = "hsi"
      lunch = 0
      tmpdate = rtdat$date[1]
      for (i in 0:10)
      {
        if (i >= 5)
          lunch = 60
        batch = i * 30 + 30 + lunch
        #x.print(i, batch)
        tmp = range(rtdat[rtdat$batch >= batch &
                            rtdat$batch <= (batch + 30), coln])
        tmpSess[4 * i + 1] = tmp[1]
        tmpSess[4 * i + 2] = tmp[2]
        tmpSess[4 * i + 3] = rtdat[rtdat$batch == batch |
                                     rtdat$batch == (batch - 1), coln][1]
        tmpSess[4 * i + 4] = rtdat[rtdat$batch == (batch + 30) |
                                     rtdat$batch == (batch + 30 - 1), coln][1]
      }
      tmpSess = data.frame(tmpdate, t(tmpSess))
      names(tmpSess) = c(
        "date","s1","e1","o1","c1", "s2","e2","o2","c2","s3","e3","o3","c3", "s4","e4","o4","c4","s5","e5","o5","c5",
        "s6","e6","o6","c6", "s7","e7","o7","c7","s8","e8","o8","c8", "s9","e9","o9","c9","s10","e10","o10","c10",
        "s11","e11","o11","c11"
      )
      sess = rbind(sess, tmpSess)
    }
  }
  sess=sess[order(sess$date), ]
  sess$date=as.Date(sess$date)
  save(sess, file=filename, version=globalsaveversion)
  sess=sess[  as.Date(sess$date) %in% as.Date(selectD.org), ]
  return (sess)
}




AAprice=function(code="00000")
{
  if (code=="00000") return(NULL)
  cmd=paste("/Users/admin/Documents/StockAnalysis/app/AAprice/run.sh", code)
  system(cmd)
}


testPHP=function(serverPrefix=paste("http://", globalDefaultServer,"/weblib/Q/query.php?q=",sep=""))
{
  #same as get today cbbc
  selectD=u.Sys.Date()
  sqlQ=paste("select+*+from+cbbc+where+batch<9990+and+'",selectD,"'<=date+and+date<='",selectD,"'+and+batch>0+order+by+batch",sep="")
  finalQ=paste(serverPrefix, sqlQ, sep="")
  read.table(finalQ, head=TRUE)
}

u.getQuote_OLD_2019_02_10=function(sinaCode="hkHSI")
{
  # u.getQuote("^hsi")
  # u.getQuote("sh000001")
  # u.getQuote("hk01182")
  if (tolower(sinaCode)=="^ssec") sinaCode="sh000001"
  if (tolower(sinaCode)=="^hsi") sinaCode="hkHSI"
  if (length(grep(pattern = "hk$", sinaCode, ignore.case = TRUE))==1)
  {
    code=substr(sinaCode, 1, (nchar(sinaCode)-3) )
    #sinaCode=paste("hk",sprintf("%05s", code),sep="")
    sinaCode=paste("hk", str_pad(code, 5, pad = "0"),sep="")  
  }
  if (length(grep("^[0-9]+$", sinaCode))==1)
  {
    code=substr(sinaCode, 1, (nchar(sinaCode)) )
    #sinaCode=paste("hk",sprintf("%05s", code),sep="")    
    sinaCode=paste("hk", str_pad(code, 5, pad = "0"),sep="")   
  }
  url=paste("http://hq.sinajs.cn/?list=", sinaCode, sep="")
  tmp=read.table(url, sep=",", encoding="latin1", stringsAsFactors=FALSE)
  tmp=as.character(tmp[,1])
  a=strsplit(tmp,",")
  a=unlist(a)
  if (length(grep("hk",sinaCode))!=0)
  {
    OPEN=3
    HIGH=5
    LOW=6
    CLOSE=7
    VOL=10
    PRIVOL=12
    CHANGE=8
    PCHANGE=9
    DATE=18
    STAMP=19  
    chg=as.numeric(a[CHANGE])
    perChg=paste(a[PCHANGE],"%",sep="")
  } else
  {
    OPEN=2
    HIGH=5
    LOW=6
    CLOSE=4
    VOL=9
    PRIVOL=10
    CHANGE=3
    PCHANGE=3
    DATE=length(a)-2
    STAMP=length(a)-1
    PREVCLOSE=3
    chg=as.numeric(a[CLOSE])-as.numeric(a[PREVCLOSE])
    perChg=round(chg/as.numeric(a[PREVCLOSE])*100,2)
  }
  
  stamp=paste(a[DATE], a[STAMP])
  stamp=sub(";","",stamp)
  stamp=gsub("-","/",stamp)
  #(timestamp <- strptime(stamp, "%Y/%m/%d %H:%M", tz = "HKT"))
  (timestamp <- strptime(stamp, "%Y/%m/%d %H:%M"))
  #a.f=a[c(5,3,6,7,12,7,19,18)]
  a.f=data.frame("Trade.Time"=timestamp, 
                 Last=as.numeric(a[CLOSE]),
                 Change=chg,
                 "PChange"=perChg,
                 Open=as.numeric(a[OPEN]),
                 High=as.numeric(a[HIGH]),
                 Low=as.numeric(a[LOW]),
                 Volume=as.numeric(a[PRIVOL]),
                 stringsAsFactors =FALSE)
  rownames(a.f)=sinaCode
  colnames(a.f)=c("Trade Time", "Last", "Change", "% Change", "Open", "High", "Low","Volume")
  return(a.f)
}  


library(XML)
getET=function()
{
  tt =  getURL("https://www.etnet.com.hk/www/eng/futures/index.php")
  rht=readHTMLTable(tt)
  hp=htmlParse(tt)
  cur=xpathApply(hp, "/html/body/div[1]/div[3]/div[1]/div[1]/div[2]/div[1]/div[3]/div[2]/div[1]/span/node()"
                 ,xmlValue)[[1]]
  mainstring=xpathApply(hp, "/html/body/div[1]/div[3]/div[1]/div[1]/div[2]/div[1]/div[3]/div[3]/div[1]"
                        ,xmlValue)
  cur=as.numeric(gsub(",","", cur))
  #colonSymbol=substring(tmpstring,2,2)
  #tmpstring=gsub(colonSymbol, "", tmpstring)
  #tmpstring=gsub("C", "", tmpstring)
  #tmpstring=gsub("O", "", tmpstring)
  #tmpstring=gsub("H", "", tmpstring)
  #tmpstring=gsub("L", "", tmpstring)
  #tmpstring=gsub("/", "", tmpstring)
  tmpstring=gsub(",", "", mainstring)
  for (i in 1:nchar(tmpstring))
  {
    aSymbol=substring(tmpstring,i,i)
    #print(aSymbol)
    #if ( !(!is.na(as.numeric(aSymbol)) || aSymbol=="." ))
    is.charnum=function(xx) grepl("^[0-9]{1,}$", xx)  
    if (!(is.charnum(aSymbol) || aSymbol=="." ))
    {
      tmpstring=gsub(aSymbol, " ", tmpstring)
    }  
  }
  vl=as.numeric(unlist(strsplit(trim(tmpstring), "\\s+")))
  vl[1]=vl[2]
  vl[2]=vl[3]
  vl[3]=vl[4]
  vl[4]=cur
  return(vl)
}

realtimeUpdate_OLD=function(s)
{
  library(XML)
  library(RCurl)
  if (Sys.Date()!=u.Sys.Date())
  {
    print("No Realtime")
    return(s)    
  }
  seldate=u.Sys.Date()
  if ('date' %in% names(s))
  {
    if (nrow(s[s$date==seldate, c('o','h','l','c')])!=1) 
    {
      print("No Realtime")
      return(s)
    }
    etprice=getET()
    s[s$date==seldate, 'o']=etprice[1]
    s[s$date==seldate, 'h']=etprice[2]
    s[s$date==seldate, 'l']=etprice[3]
    s[s$date==seldate, 'c']=etprice[4]
    s[s$date==seldate, 'v']=as.numeric(format(Sys.time(),"%H%M"))
  }
  if (seldate %in% index(s))
  {
    etprice=getET() 
    s[seldate,'o']=etprice[1]
    s[seldate,'h']=etprice[2]
    s[seldate,'l']=etprice[3]
    s[seldate,'c']=etprice[4]
    s[seldate,'v']=as.numeric(format(Sys.time(),"%H%M"))
  } 
  return(s)
}

realtimeUpdate=function (s) 
{
  if (1==2) print("ver 20220508")
  library(XML)
  library(RCurl)
  seldate = u.Sys.Date()
  if ("date" %in% names(s)) {
    if (nrow(s[s$date == seldate, c("o", "h", "l", "c")]) != 
        1) {
      print("No Realtime")
      return(s)
    }
    etprice = getET()
    s[s$date == seldate, "o"] = etprice[1]
    s[s$date == seldate, "h"] = etprice[2]
    s[s$date == seldate, "l"] = etprice[3]
    s[s$date == seldate, "c"] = etprice[4]
  }
  if (seldate %in% index(s)) {
    etprice = getET()
    s[seldate, "o"] = etprice[1]
    s[seldate, "h"] = etprice[2]
    s[seldate, "l"] = etprice[3]
    s[seldate, "c"] = etprice[4]
  }
  return(s)
}

u.getQuote=function(sinaCode="hkHSI")
{
  # u.getQuote("^hsi")
  # u.getQuote("sh000001")
  # u.getQuote("hk01182")
  if (tolower(sinaCode)=="^ssec") sinaCode="sh000001"
  if (tolower(sinaCode)=="^hsi") sinaCode="hkHSI"
  if (length(grep(pattern = "hk$", sinaCode, ignore.case = TRUE))==1)
  {
    code=substr(sinaCode, 1, (nchar(sinaCode)-3) )
    #sinaCode=paste("hk",sprintf("%05s", code),sep="")
    sinaCode=paste("hk", str_pad(code, 5, pad = "0"),sep="")  
  }
  if (length(grep("^[0-9]+$", sinaCode))==1)
  {
    code=substr(sinaCode, 1, (nchar(sinaCode)) )
    #sinaCode=paste("hk",sprintf("%05s", code),sep="")    
    sinaCode=paste("hk", str_pad(code, 5, pad = "0"),sep="")   
  }
  url=paste("http://hq.sinajs.cn/?list=", sinaCode, sep="")
  tmp=read.table(url, sep=",", encoding="latin1", stringsAsFactors=FALSE)
  tmp=as.character(tmp[,1])
  a=strsplit(tmp,",")
  a=unlist(a)
  if (length(a)==1) 
  {
    print("u.getQuote has error: return false")
    return (FALSE)
  }
  if (length(grep("hk",sinaCode))!=0)
  {
    OPEN=3
    HIGH=5
    LOW=6
    CLOSE=7
    VOL=10
    PRIVOL=12
    CHANGE=8
    PCHANGE=9
    DATE=18
    STAMP=19  
    chg=as.numeric(a[CHANGE])
    perChg=paste(a[PCHANGE],"%",sep="")
  } else
  {
    OPEN=2
    HIGH=5
    LOW=6
    CLOSE=4
    VOL=9
    PRIVOL=10
    CHANGE=3
    PCHANGE=3
    DATE=length(a)-2
    STAMP=length(a)-1
    PREVCLOSE=3
    chg=as.numeric(a[CLOSE])-as.numeric(a[PREVCLOSE])
    perChg=round(chg/as.numeric(a[PREVCLOSE])*100,2)
  }
  
  stamp=paste(a[DATE], a[STAMP])
  stamp=sub(";","",stamp)
  stamp=gsub("-","/",stamp)
  #(timestamp <- strptime(stamp, "%Y/%m/%d %H:%M", tz = "HKT"))
  (timestamp <- strptime(stamp, "%Y/%m/%d %H:%M"))
  #a.f=a[c(5,3,6,7,12,7,19,18)]
  a.f=data.frame("Trade.Time"=timestamp, 
                 Last=as.numeric(a[CLOSE]),
                 Change=chg,
                 "PChange"=perChg,
                 Open=as.numeric(a[OPEN]),
                 High=as.numeric(a[HIGH]),
                 Low=as.numeric(a[LOW]),
                 Volume=as.numeric(a[PRIVOL]),
                 stringsAsFactors =FALSE)
  rownames(a.f)=sinaCode
  colnames(a.f)=c("Trade Time", "Last", "Change", "% Change", "Open", "High", "Low","Volume")
  return(a.f)
}  


getHSIClose=function(date=u.Sys.Date())
{
  s=loadData("^HSI", from=date, to=date)
  return(as.numeric(s$c))
}

getSHClose=function(date=u.Sys.Date())
{
  s=loadData("^SSEC", from=date, to=date)
  return(as.numeric(s$c))
}



pickCBBC=function(typeThd=NULL, priceThd=9999 ,date=u.Sys.Date())
{
  cbbc=subset(rtday(date=date, tablename="cbbc"), stock="^HSI")
  lastbc=subset(cbbc, batch==1620)
  lastbc=data.frame(icode=lastbc$icode, type=lastbc$type, price=lastbc$price)
  dat=data.frame(icode=cbbc$icode,p=cbbc$price, pm=cbbc$pre)
  
  tmp=aggregate(dat, by=list(cbbc$icode), FUN=sd, na.rm=TRUE)
  tmp$icode=tmp$Group.1
  tmp=merge(tmp,lastbc)
  #tmp=tmp[order(tmp$pm, decreasing=TRUE),]
  rownames(tmp) <- 1:nrow(tmp)
  tmp$Group.1=NULL
  retV=tmp
  if (isnot.null(typeThd)) 
  {
    print("filter")
    retV=subset(retV, type==typeThd)
  }
  retV=subset(retV, price<=priceThd)
  retV=retV[order(retV$pm, decreasing=TRUE),]  
  return (retV)
}


u.getURL=function(url)
{
  #url="http://www.aastocks.com/tc/LTP/RTQuote.aspx?&Symbol=66076"
  filename <- "tmp.txt"
  download.file(url, "tmp.txt")
  txt=readChar(filename, file.info(filename)$size)
  file.remove(filename)
  return( txt)
}

getAA=function(aacode)
{
  require(XML)
  url=paste("http://www.aastocks.com/en/ltp/rtquote.aspx?symbol=",aacode,sep="")
  tt =  u.getURL(url)
  rht=readHTMLTable(tt)
  retV=rht[[12]]
  return(retV)
}

getPrice=function(code)
{
  require(XML)
  link = paste("http://www.aastocks.com/en/ltp/rtquote.aspx?symbol=",code,sep="")
  tables <- readHTMLTable(link)
  #str(tables)  #this line is used to understand where the data is
  print(tables[[12]])
}

clearMemory=function()
{
  stockNameListForMachineUse<<-NULL
  stockDataListForMachineUse<<-NULL  
}

#dirPath="/Users/admin/Documents/Dropbox/_Data"
loadData=function(stockCode, downloadFlag=FALSE, dirPath="default", realtime=FALSE,  
        from=as.Date("2010-01-01"), to=(u.Sys.Date(+1)), abbr=TRUE, v0.rm=FALSE)
{
  if (dirPath=="default")
  {
    #dirPath=getPath()
    if (is.windows()) 
    {
      dirPath="/Users/admin/Documents/Dropbox/_Data/"
    } else
    {
      dirPath="/Users/admin/Documents/Dropbox/_Data/"
    }
  }
  
  stockCode=tolower(stockCode)
  stockC=paste(dirPath, stockCode, ".RData", sep="") 
  
  if (file.exists(stockC) & downloadFlag==FALSE & realtime==TRUE)
  {
    #remove the record from memory if it is there
    if (isDefined("stockNameListForMachineUse") &&
          isDefined("stockNameListForMachineUse") && 
          0!=length(stockNameListForMachineUse) )
    {
      for (i in 1:length(stockNameListForMachineUse))
      {
        if (stockCode==stockNameListForMachineUse[[i]])
        {
          stockNameListForMachineUse[[i]]<<-NULL
          stockDataListForMachineUse[[i]]<<-NULL
          break
        }
      }
    } else
    {
      stockNameListForMachineUse <<- list()
      stockDataListForMachineUse <<- list()
    }
    load(file=stockC)
    if (stockCode=="^ssec") rt=getSSEC() else rt=getQuote(stockCode)
    #rt=getQuote(stockCode)
    rt.df=data.frame(o=rt[,5], h=rt[,6],l=rt[,7],c=rt[,2],
                     v=rt[,8],a=rt[,2],
                     date=as.Date(format( rt[,1] , format="%Y-%m-%d")))
    if (rt.df$v[1]==0) rt.df$v[1]=1
    rt.ts=xts(rt.df[,1:6], rt.df$date)
    s=subset(s, time(s)!=rt.df$date[1])
    if ( !any(time(rt.ts)==time(s)) ) 
    { 
      s=rbind(s, rt.ts)
    }
    totalEle=length(stockNameListForMachineUse)
    stockNameListForMachineUse[[(totalEle+1)]] <<- stockCode
    stockDataListForMachineUse[[(totalEle+1)]] <<- s
    names(stockDataListForMachineUse) <<- unlist(stockNameListForMachineUse)
    
    names(s)=c("o", "h", "l", "c", "v", "a")
    save(s,file=stockC, version=globalsaveversion )  
    s=subset(s, index(s)<=to)  # added on 2014-2
    if (v0.rm==TRUE) s=subset(s, v!=0)
    if (abbr!=TRUE) names(s)=c("Open","High","Low","Close","Volume","Adjusted")
    s=subset(s, from<= index(s) & index(s) <=to )
    return (s)
  }
  
  if (downloadFlag==FALSE && isDefined("stockNameListForMachineUse") &&
        isDefined("stockNameListForMachineUse") && 0!=length(stockNameListForMachineUse) )
  {
    #print("search memory")  #debug
    for (i in 1:length(stockNameListForMachineUse))
    {
      if (stockCode==stockNameListForMachineUse[[i]])
      { 
        #print("got from memory")  #debug
        names( stockDataListForMachineUse[[i]] )=c("o", "h", "l", "c", "v", "a")
        s=stockDataListForMachineUse[[i]]
        s=subset(s, index(s)<=to)  # added on 2014-2
        if (v0.rm==TRUE) s=subset(s, v!=0)
        if (abbr!=TRUE) names(s)=c("Open","High","Low","Close","Volume","Adjusted")
        s=subset(s, from<= index(s) & index(s) <=to)
        return (s)
      }
    }
  } else
  {
    stockNameListForMachineUse <<- list()
    stockDataListForMachineUse <<- list()
  }
  
  
  if (file.exists(stockC) & downloadFlag==FALSE) {
    #print("got from file") #debug
    load(file=stockC)
  } else
  {
    #print("got from internet") #debug  
    # stockCode="0001.HK"
    #fromD=as.Date("2010-01-01")
    fromD=as.Date("2010-01-01")
    if (tolower(stockCode)=="^hsi") fromD=as.Date("2005-01-01")
    if (from<=fromD) fromD=from
    s=getSymbols(stockCode, from=fromD, auto.assign=FALSE)
    names(s)=c("o", "h", "l", "c", "v", "a")
    
    if (realtime==TRUE)
    {
      if (stockCode=="^ssec") rt=getSSEC() else rt=getQuote(stockCode)
      #rt=getQuote(stockCode)
      
      rt.df=data.frame(o=rt[,5], h=rt[,6],l=rt[,7],c=rt[,2],
                       v=rt[,8],a=rt[,2],
                       date=as.Date(format( rt[,1] , format="%Y-%m-%d")))
      if ( rt.df$v[1]!="N/A"  )
      {
        if (rt.df$v[1]==0) rt.df$v[1]=1
        rt.ts=xts(rt.df[,1:6], rt.df$date)
        if ( !any(time(rt.ts)==time(s)) ) 
        { 
          s=rbind(s, rt.ts)
        }
      }
    }
    save(s,file=stockC , version=globalsaveversion )
  }
  totalEle=length(stockNameListForMachineUse)
  stockNameListForMachineUse[[(totalEle+1)]] <<- stockCode
  stockDataListForMachineUse[[(totalEle+1)]] <<- s
  names(stockDataListForMachineUse) <<- unlist(stockNameListForMachineUse)
  
  names(s)=c("o", "h", "l", "c", "v", "a")
  s=subset(s, index(s)<=to)  # added on 2014-2
  if (v0.rm==TRUE) s=subset(s, v!=0)
  if (abbr!=TRUE) names(s)=c("Open","High","Low","Close","Volume","Adjusted")
  s=subset(s, from<= index(s) & index(s) <=to)
  return (s)
}

realtimeLoad=function(asset_all= NULL)
{
  clearMemory()
  printMessage=TRUE
  if (is.null(asset_all))
  {
    asset_all=c(getIndex(), getHSIComponent()) 
    #asset_all=c("^HSI",  getHSIComponent())  
  } 
  
  for (i in 1 : length(asset_all))
  {
    print(asset_all[i])
    s=loadData(asset_all[i])
    maxDate=max(time(s))
    secondMaxDate=max(subset(time(s), time(s)!=maxDate))
    diffD=maxDate-secondMaxDate

    sdate=Sys.Date()
    if (  weekdays(as.Date(sdate))=="Saturday" ) sdate=sdate-1
    if (  weekdays(as.Date(sdate))=="Sunday" ) sdate=sdate-2
    
    
    if (is.na(maxDate)==FALSE &&  (sdate-maxDate)<=2 && diffD<6)
    {
      if (TRUE==printMessage) { print("clear memory and reload to memory"); printMessage=FALSE}
      downloadFlag=FALSE
    } else
    {
      if (TRUE==printMessage) { print("clear memory and re-download"); printMessage=FALSE}
      downloadFlag=TRUE
    }
    retry=1
    repeat{
      s=handle(FUN="loadData",stockCode=asset_all[i],  downloadFlag=downloadFlag, realtime=TRUE)
      if(  class(s)[1]=="xts" ){ break }
      if( retry==3) {
        print(paste("fail to reload ", asset_all[i]))
        break
      }
      retry=retry+1
      Sys.sleep(9)
    }
    if (downloadFlag==TRUE) Sys.sleep(1)
  }  
}

refreshLoad=function(asset_all= NULL, deep=FALSE, ignoreDateDiff=TRUE, quantmod_flag=FALSE )
{
  clearMemory()
  if (is.null(asset_all)) asset_all=c(getIndex(), getHSIComponent())    
  for (i in 1 : length(asset_all))
  {  # i=1
    currAsset=asset_all[i]
    print(currAsset)
    retry=1
    s=loadData(currAsset)  
    updatedStatus=0
    repeat{
      if (quantmod_flag==TRUE)
      {
        if (currAsset=="^SSEC") us=quantmod::getSymbols("000001.SS", auto.assign=FALSE)
        if (currAsset!="^SSEC") us=quantmod::getSymbols(currAsset, auto.assign=FALSE)
      } else
      {
        if (currAsset=="^SSEC") us=u.getSymbols("000001.SS")
        if (currAsset!="^SSEC") us=u.getSymbols(currAsset)
      }
      us=us[!duplicated(index(us)),]
      
      if (is.null(us)==TRUE) 
      {
        print(paste("fail to get any date for", currAsset ))
        break
      }
      
      if ( index(tail(us, n=1))!=u.Sys.Date() && ignoreDateDiff==FALSE)
      {
        usg=NULL
        usg=getSymbols(currAsset, auto.assign = FALSE)
        if (!is.null(usg)==TRUE) 
        {
          print(paste(currAsset ," : try to use getSymbols as u.getSymbols date not updated"))
          if ( index(tail(usg, n=1))!=u.Sys.Date())
          {
            print(paste(currAsset ," : fail to get updated date from getSymbols"))
            if (max(index(usg))>max(index(us))) 
            {
              print("final decision: getSymbols")
              us=usg
            }
          } else
          {
            print("final decision: getSymbols")
            us=usg
          }
        }
      }
      
      for (nrs in nrow(us):1)  
      { #nrs=70 us[70]
        
        if (deep==FALSE)
        {
          if ( !any(time(us[nrs])==time(s)) ) 
          {
            s=rbind(us[nrs],s)
            updatedStatus=1
          }
        } else
        {
          if ( any(time(us[nrs])==time(s)) )
          {
            if (!(isHoliday(time(us[nrs]))==TRUE)) 
            {
              s=s[ ! time(s) %in%   time(us[nrs]) ]
              s=rbind(us[nrs],s)
              updatedStatus=1
            }
            
          } else
          {
            s=rbind(us[nrs],s)
            updatedStatus=1
          }
        }
      }
      
      if (updatedStatus==1)
      {
        dirPath=getPath()
        stockC=paste(dirPath,asset_all[i], ".RData", sep="") 
        print(stockC)
        save(s,file=stockC , version=globalsaveversion)
      }
      if(  class(s)[1]=="xts" ){ break }
      if( retry==3) {
        print(paste("fail to reload ", asset_all[i]))
        break
      }
      retry=retry+1
      Sys.sleep(9)
    }
    Sys.sleep(1)
  }
  clearMemory()
}






isAvaliableGetSymbols=function(Symbols)
{
  if (tolower(Symbols)=="1113.hk") return(TRUE)
  if (is.null(Symbols)) Symbols="^hsi"
  functionName="getSymbols"
  retV=handleWithGet(FUN=functionName, pos="package:quantmod", Symbols=Symbols, from="2015-01-02", to="2015-02-03", auto.assign=FALSE, errorValue=-999)
  if (-999 ==  retV) return (FALSE)
  return (TRUE)
}

handleWithGet=function(FUN, pos=-1, ..., errorValue=-999)
{
  FUN <- get(FUN, pos=pos)
  tryCatch(FUN(...), error = function(e) { return(errorValue)}, finally=function() {retV} ) 
}


u.getSymbols=function (Symbols = "^hsi") 
{
  if (is.null(Symbols)) 
    return(NULL)
  base_url <- paste0("https://finance.yahoo.com/quote/", Symbols, 
                     "/history?p=", Symbols)
  library(httr)
  r = GET(base_url)
  status = status_code(r)
  if (status == 500) {
    stop(paste0("Error : u.getSymbols  ", Symbols))
  }
  doc <- read_html(r)
  tbl = html_table(doc)
  if (length(tbl) == 0) 
    return(NULL)
  tb = tbl[[1]]
  tb = tb[-nrow(tb), ]
  tb[, 2:7] <- apply(tb[, 2:7], 2, function(x) as.numeric(gsub(",", 
                                                               "", x)))
  temp = head(tb, n = 1)
  tb = na.omit(tb)
  if (is.na(temp$Volume[1]))  temp$Volume[1] = 0
  tb = rbind(temp, tb)
  tb=as.data.frame(tb)                   
  tb[, 1] = as.Date(tb[, 1], "%b %d, %Y")
  
  qxts = xts(tb[, -1], order.by = as.Date(tb[, 1]))
  
  qxts = qxts[, c(1, 2, 3, 4, 6, 5)]
  names(qxts) = c("o", "h", "l", "c", "v", "a")
  return(qxts)
}


#getSymbols=function(Symbols=NULL, from=as.Date("2013-02-01"), to=Sys.Date(), auto.assign=FALSE,...)
#{
#  if (isAvaliableGetSymbols(Symbols)) 
#  {
#    print("Use quantmod getSymbols")
#    return(quantmod::getSymbols(Symbols=Symbols, from=from, to=to, auto.assign=auto.assign,...))
#  }
#  print("Use My Own Version of getSymbols")
#  #Symbols="^hsi"
#  #from=as.Date("2013-02-01")
#  #to=Sys.Date()
#  if (is.null(Symbols)) stop("Symbols not found")
#  fy=format(from, format="%Y")
#  fm=format(from, format="%M")
#  fd=format(from, format="%d")  
#  ty=format(to, format="%Y")
#  tm=format(to, format="%M")
#  td=format(to, format="%d")
#  a1="http://ichart.finance.yahoo.com/table.csv?s="
#  f1m="&a="  #need minus 1 as 0 means Jan
#  f2d="&b="  
#  f3y="&c="
#  t1m="&d="
#  t2d="&e="
#  t2y="&f="
#  a2="&g=d&ignore=.csv"
#  url=paste(a1,Symbols, f1m,fm,f2d,fd,f3y,fy,t1m,tm,t2d,td,t2y,ty,a2,sep="")
#  q=read.csv(url)
#  qxts <- xts(q[,-1], order.by=as.Date(q[,1]))
#  return(qxts)
#}

rtday_old=function(...,tablename="rtday") 
{   rt=rtdata(...,tablename=tablename)
    subset(rt, 30<=batch & batch<=422)
}

rtday=function(...,tablename="rtday") rtdata(...,tablename=tablename)


rtday_v=function(...,tablename="rtday_v") 
{   rt=rtdata(...,tablename=tablename)
    subset(rt, 30<=batch & batch<=422)
}

rtdata_OLD=function(date=u.Sys.Date(), from=NULL, to=NULL, save.Flag=FALSE,
                tablename="rtday", cond="", forceDB=FALSE, useHTTP=FALSE, cacheDir="")
{ # date="2014-01-01" 
  if (is.null(from)==TRUE) from=date
  if (is.null(to)==TRUE) to=date
  if (cacheDir!="" && !file.exists(cacheDir)) 
  {
    cacheDir=""
  }
  
  if (cacheDir!="" && substr(cacheDir, nchar(cacheDir), nchar(cacheDir))!="/")
  {
    cacheDir=paste0(cacheDir,"/")
  }
  
  cacheFile=paste(cacheDir,tablename,date,"cache.RData",sep="")
  
  #filename=paste(getPath(),tablename,".RData",sep="")
  filename=uniPath("Dropbox/_data/",tablename,".RData")
  if ( ((date!=Sys.Date() && from!=Sys.Date() && to!=Sys.Date()) || 
          (date==Sys.Date() && 1700<as.numeric(format(Sys.time(), "%H%M")) ) )&& 
         file.exists(filename) && save.Flag==FALSE && forceDB==FALSE)
  {
    u.print("try to use Dropbox/_data/ for ",tablename)
    requestD=date
    load(file=filename )
    s=subset(s, from<=date & date<=to )
    if (nrow(s)>10) return(s)
    print("fail to get from Dropbox/_data/. Then Try (1) local cache and then (2) download!")
  }
  lastBatch=0
  s=data.frame()
  if (file.exists(cacheFile))
  {
    u.print("try local cache file: ",cacheFile)
    load(file=cacheFile)
    if (!is.null(s) && nrow(s)>0)   lastBatch=tail(s, n=1)$batch
  }
  
  batchNumUsed=0
  if (0<lastBatch && lastBatch<800 ) batchNumUsed=as.numeric(lastBatch)
  if (0<lastBatch && lastBatch>=800 ) batchNumUsed=stampToBatch(lastBatch)
  
  t=data.frame()
  clockBatch=round(getBatch())
  isToDownloadToday=(round(round(batchNumUsed)-clockBatch)!=0) && (u.Sys.Date()==date )
  isToDownloadPastday=!( (u.Sys.Date()!=date)  && 420<batchNumUsed )
  x.print(isToDownloadPastday)
  isToDownload = isToDownloadToday || isToDownloadPastday
  x.print(isToDownload)
  if (isToDownload  )
  {
    u.print("download. Cache is not enough as ",isToDownload,":",batchNumUsed, "vs",clockBatch)
    sqlQCond=paste( "'",from,"'<=date and date<='",to,"' and batch>",lastBatch, sep="")
    sqlQ=paste("select * from ",tablename," where batch<9990 and ",sqlQCond, cond, " order by batch",sep="")
  
    if (useHTTP==FALSE)
    {
      myconn <-odbcConnect("odbcmysql")
      t <- sqlQuery(myconn, sqlQ)
      close(myconn)
    } else
    {
      sqlQ=gsub(' ','+', sqlQ)
    #sqlQ="select+*+from+cbbc+where+batch<9990+and+'2015-03-27'<=date+and+date<='2015-03-27'+and+batch>0+and+stock='1'+order+by+batch"
      prefix=paste("http://",globalDefaultServer,"/weblib/Q/query.php?q=",sep="")
      finalQ=paste(prefix, sqlQ, sep="")
      t=read.table(finalQ, head=TRUE)
    }
  }
  else 
  {
    print("not download. Cache is enough")
  }
  #print(as.numeric(format(Sys.time(), "%S"))) class(t) class(s)
  
  if (!is.null(s) && nrow(s)>0 && nrow(t)>0) s=rbind(s,t)
  if (!is.null(s) && nrow(s)==0) s=t
  if (isToDownload)
  {
    if (cacheDir=="")  u.print("download date is locally saved to ",getwd(),"/",cacheFile)
    if (cacheDir!="") u.print("download date is locally saved to ",cacheFile)
    save(s, file=cacheFile , version=globalsaveversion)
  }
  if (save.Flag==TRUE) 
  {
    u.print("download date is globally saved to ",filename)
    save(s, file=filename , version=globalsaveversion) 
  }
  s=s[order(s$date,s$batch),]
  return (s)
}

rtdata=function (date = u.Sys.Date(), from = NULL, to = NULL, save.Flag = FALSE, 
                 tablename = "rtday", cond = "", forceDB = FALSE, useHTTP = FALSE, 
                 cacheDir = "", useRMySQL=0) 
{
  if (is.null(from) == TRUE) 
    from = date
  if (is.null(to) == TRUE) 
    to = date
  if (cacheDir != "" && !file.exists(cacheDir)) {
    cacheDir = ""
  }
  if (cacheDir != "" && substr(cacheDir, nchar(cacheDir), nchar(cacheDir)) != 
      "/") {
    cacheDir = paste0(cacheDir, "/")
  }
  cacheFile = paste(cacheDir, tablename, date, "cache.RData", 
                    sep = "")
  filename = uniPath("Dropbox/_data/", tablename, ".RData")
  if (((date != Sys.Date() && from != Sys.Date() && to != Sys.Date()) || 
       (date == Sys.Date() && 1700 < as.numeric(format(Sys.time(), 
                                                       "%H%M")))) && file.exists(filename) && save.Flag == 
      FALSE && forceDB == FALSE) {
    u.print("try to use Dropbox/_data/ for ", tablename)
    requestD = date
    load(file = filename)
    s = subset(s, from <= date & date <= to)
    if (nrow(s) > 10) 
      return(s)
    print("fail to get from Dropbox/_data/. Then Try (1) local cache and then (2) download!")
  }
  lastBatch = 0
  s = data.frame()
  if (file.exists(cacheFile)) {
    u.print("try local cache file: ", cacheFile)
    load(file = cacheFile)
    if (!is.null(s) && nrow(s) > 0) 
      lastBatch = tail(s, n = 1)$batch
  }
  batchNumUsed = 0
  if (0 < lastBatch && lastBatch < 800) 
    batchNumUsed = as.numeric(lastBatch)
  if (0 < lastBatch && lastBatch >= 800) 
    batchNumUsed = stampToBatch(lastBatch)
  t = data.frame()
  clockBatch = round(getBatch())
  isToDownloadToday = (round(round(batchNumUsed) - clockBatch) != 
                         0) && (u.Sys.Date() == date)
  isToDownloadPastday = !((u.Sys.Date() != date) && 420 < batchNumUsed)
  x.print(isToDownloadPastday)
  isToDownload = isToDownloadToday || isToDownloadPastday
  x.print(isToDownload)
  if (isToDownload) {
    u.print("download. Cache is not enough as ", isToDownload, 
            ":", batchNumUsed, "vs", clockBatch)
    sqlQCond = paste("'", from, "'<=date and date<='", to, 
                     "' and batch>", lastBatch, sep = "")
    sqlQ = paste("select * from ", tablename, " where batch<9990 and ", 
                 sqlQCond, cond, " order by batch", sep = "")
    if (useHTTP == FALSE) {
      if (useRMySQL==1)
      {
        require(RMySQL)
        RMySQLconobj <- dbConnect(MySQL(),user="wits" ,password=passwd,db="nammik",host="127.0.0.1")
        t=dbGetQuery(RMySQLconobj, sqlQ)
        dbDisconnect(RMySQLconobj)
      } else
      {
        myconn <- odbcConnect("odbcmysql")
        t <- sqlQuery(myconn, sqlQ)
        close(myconn)
      }
    }
    else {
      sqlQ = gsub(" ", "+", sqlQ)
      prefix = paste("http://", globalDefaultServer, "/weblib/Q/query.php?q=", 
                     sep = "")
      finalQ = paste(prefix, sqlQ, sep = "")
      t = read.table(finalQ, head = TRUE)
    }
  }
  else {
    print("not download. Cache is enough")
  }
  if (!is.null(s) && nrow(s) > 0 && nrow(t) > 0) 
    s = rbind(s, t)
  if (!is.null(s) && nrow(s) == 0) 
    s = t
  if (isToDownload) {
    if (cacheDir == "") 
      u.print("download date is locally saved to ", getwd(), 
              "/", cacheFile)
    if (cacheDir != "") 
      u.print("download date is locally saved to ", cacheFile)
    save(s, file = cacheFile, version = globalsaveversion)
  }
  if (save.Flag == TRUE) {
    u.print("download date is globally saved to ", filename)
    save(s, file = filename, version = globalsaveversion)
  }
  s = s[order(s$date, s$batch), ]
  return(s)
}


subsetRtdata=function(dat, period=1)
{
  if (period==1) return(subset(dat, 30<=batch & batch<60))
  if (period==2) return(subset(dat, 60<=batch & batch<90))
  if (period==3) return(subset(dat, 90<=batch & batch<120))
  if (period==4) return(subset(dat, 120<=batch & batch<150))
  if (period==5) return(subset(dat, 150<=batch & batch<180))
  if (period==6) return(subset(dat, 240<=batch & batch<270))
  if (period==7) return(subset(dat, 270<=batch & batch<300))
  if (period==8) return(subset(dat, 300<=batch & batch<330))
  if (period==9) return(subset(dat, 330<=batch & batch<360))
  if (period==10) return(subset(dat, 360<=batch & batch<390))
  if (period==11) return(subset(dat, 390<=batch & batch<420))
}  



nameXbatch=function(b.c.o)
{
  nm=names(b.c.o)
  if (any(nm=="Xbatch")) return (b.c.o)
  if (any(nm=="batch"))
  {
    dx=which(nm=="batch")
    names(b.c.o)[dx]="Xbatch"
    return (b.c.o)
  }
  stop ("Error: cannot fnd batch")
}

bigTable=function(date=u.Sys.Date(), batch=c("numeric","stamp"), stock=c("hsi","nonhsi"),
                  indexN=c("hsi","nowsh"),  forceDB=FALSE, useHTTP=FALSE)
{  
  if (batch[1]=="stamp") selCol="stamp" else selCol="batch"
  #raw=rtdata(date=date,tablename="rtday")[,c(selCol,"hsi")]
  selectColN=selCol
  raw=NULL
  allcomp=NULL
  if (isnot.null(indexN)) selectColN=c(selCol,indexN)
  #raw=rtdata(date=date,tablename="rtday_v")[,c(selCol,"hsi","nowsh")]
  if (isnot.null(indexN)) raw=rtdata(date=date,tablename="rtday_v", forceDB=forceDB, useHTTP=useHTTP)[,selectColN]
  
  if (any(stock=="hsi")) comp=rtHSIComponent(date=date, batch=batch[1], forceDB=forceDB, useHTTP=useHTTP)
  if (any(stock=="nonhsi")) noncomp=rtNONHSIComponent(date=date, batch=batch[1], forceDB=forceDB, useHTTP=useHTTP)
  
  if ( any(stock=="hsi")  &&  any(stock=="nonhsi") ) allcomp=merge(comp, noncomp)
  if ( any(stock=="hsi")  &&  !any(stock=="nonhsi") ) allcomp=comp
  if ( !any(stock=="hsi") &&  any(stock=="nonhsi") ) allcomp=noncomp
  if (is.null(raw))  
  {
    a=allcomp 
    if (selCol=="stamp") colnames(a)[1]="stamp"
    if (selCol=="batch") colnames(a)[1]="batch"
    
  } else if (is.null(allcomp))
  {
    a=raw
    if (selCol=="stamp") colnames(a)[1]="stamp"
    if (selCol=="batch") colnames(a)[1]="batch"
  } else
  {
    a=merge(raw, allcomp ,by.x=selCol, by.y="Xbatch")    
  }  
  if (selCol=="stamp") a=subset(a, stamp>=930)
  else a=subset(a, batch>=30)  
  return(a)
}

orange=function(x)
{
  write.table(x, getownCloudDir("data/orange.txt"), sep = "\t",row.names = FALSE)
}


rtHSIComponent=function(date=u.Sys.Date(), colSelected=c("close","vol"), batch=c("numeric","stamp"), forceDB=FALSE, useHTTP=FALSE)
{
  tablename="pricehighfreq"
  datA  =rtdata(date=date, tablename=tablename, forceDB=forceDB, useHTTP=useHTTP)
  datA = subset(datA, batch>=930) 
  datB  =rtdata(date=date, tablename="highfreq", forceDB=forceDB, useHTTP=useHTTP)
  datA  =rbind(datA, datB)
  datA  =datA[order(datA$batch),]
  
  library(reshape)
  b.c.o=cast(datA, batch  ~ code, value=colSelected[1])
  #b.v.o=cast(b, batch  ~ code, value="vol")
  
  tmp=na.fix(b.c.o)
  b.c.o=tmp[[1]]
  u.print("num of data records be fixed for ",colSelected[1],": ", tmp[[2]])
  
  if (batch[1]=="numeric") b.c.o$batch=batch.numeric(b.c.o$batch)
  b.c.o$batch=round(b.c.o$batch, 0)
  names(b.c.o)=paste("X",names(b.c.o),sep="")
  return(b.c.o)
}

rtHSIComponentWithHSI=function(date=u.Sys.Date(), batch=c("numeric","stamp"), forceDB=FALSE, useHTTP=FALSE)
{
  if (batch[1]=="stamp") selCol="stamp" else selCol="batch"
  #raw=rtdata(date=date,tablename="rtday")[,c(selCol,"hsi")]
  raw=rtdata(date=date,tablename="rtday_v", forceDB=forceDB, useHTTP=useHTTP )[,c(selCol,"hsi")]
  
  comp=rtHSIComponent(date=date, batch=batch[1], forceDB=forceDB, useHTTP=useHTTP)
  a=merge(raw, comp ,by.x=selCol, by.y="Xbatch")
  if (selCol=="stamp") a=subset(a, stamp>=930)
  else a=subset(a, batch>=30)  
  return(a)
}


rtNONHSIComponent=function(date=u.Sys.Date(), colSelected=c("close","vol"), batch=c("numeric","stamp"), forceDB=FALSE, useHTTP=FALSE)
{
  tablename="highfreq_nonhsi"
  datA  =rtdata(date=date, tablename=tablename, forceDB=forceDB, useHTTP=useHTTP)
  datA  =subset(datA, batch>=930)
  datA  =datA[order(datA$batch),]
  
  library(reshape)
  b.c.o=cast(datA, batch  ~ code, value=colSelected[1])
  #b.v.o=cast(b, batch  ~ code, value="vol")
  
  tmp=na.fix(b.c.o)
  b.c.o=tmp[[1]]
  u.print("num of data records be fixed for ",colSelected[1],": ", tmp[[2]])
  
  if (batch[1]=="numeric") b.c.o$batch=batch.numeric(b.c.o$batch)
  b.c.o$batch=round(b.c.o$batch, 0)
  names(b.c.o)=paste("X",names(b.c.o),sep="")
  return(b.c.o)
}



#### __CONVERSION__ ####

weeknumber=function(x)  as.numeric(strftime(x, format = "%V"))

yearweeknumber=function(x)  as.numeric(format(as.Date(x, format="%d/%m/%Y"),"%Y"))*100+weeknumber(x)

getCommonBatch=function(x)
{
  if (360<=x) return (360)
  if (240<=x && x<=360) return (x)
  if (150<=x && x<=240) return (150)
  return (x)
}  



tableToDataframe=function(tb)
{
  tmp=as.data.frame(tb)
  tmp$Var1=as.character(tmp$Var1)
  tmp=tmp[order(tmp[,"Var1"]), ]
  df=as.data.frame(t(tmp))
  names(df) = as.vector(t(df[1,]))
  df=df[-1,]
  return(df)
}

toStamp=function(...) {batchToStamp(...)}
toBatch=function(...) {stampToBatch(...)}

stampToBatch=function(stamp) stamp2Num(as.numeric(stamp))
stamp2Num=function(stamp)
{
  h=as.integer(stamp/100)*100
  a=(h/100-9)*60+(stamp-h)
  return(a)
}

#the following has a bad name as it functions as stamp2Num
batch.numeric=function(x) { k=(x/100); return (round(as.integer(k)*60 + (k-as.integer(k))*100 -9*60)) }
#batch.numeric=function(x) { k=(x/100); return (as.integer(k)*60 + (k-as.integer(k))*100 -9*60) }

batchToStamp=function(x)  return ( (x%/%60+9)*100+(x%%60) )
toBatch=function(x) return (batchToStamp(x))

xts.numeric=function(...) { return(as.numeric(as.POSIXlt(...)))}

getStamp=function() format(Sys.time(),"%H%M")
getBatch=function() batch.numeric(as.numeric(getStamp()))

mvColToEnd=function(x, colnameV)
{
  colnam=colnames(x)
  colDX=which(colnam==colnameV)
  colnam[colDX]="txtxtxtxtxtxtx"
  colnames(x)=colnam
  x$new_txtxtxtxtxtxtx=x$txtxtxtxtxtxtx
  x$txtxtxtxtxtxtx=NULL
  colnam=colnames(x)
  colnam[length(colnam)]=colnameV
  colnames(x)=colnam
  return(x)
}


mvColForward=function(x, colnameV, ToLoc)
{
  #testcase dat=data.frame(a=c(1,2), b=c(2,3), c=c(3,3), d=c(1,1))
  #         mvColForward(dat, colnameV="d", 2)
  if (ToLoc<=1) stop("error: ToLoc must be greater than or equal to 2")
  if (which(colnames(x)==colnameV) <= ToLoc) stop("error: cannot move Forward")
  colnam=colnames(x)
  x1=x[, 1:(ToLoc)]
  tmpName=colnames(x1)
  x2=x[, (ToLoc):ncol(x)]  
  
  x1[,ToLoc]=x[,colnameV]
  tmpName[ToLoc]=colnameV
  colnames(x1)=tmpName
  x2[,colnameV]=NULL
  
  x3=cbind(x1,x2)
  return(x3)
}

old_xtsToDf=function(s.xts)
{
  s.df=as.data.frame(s.xts)
  s.df$date=as.Date(rownames( s.df))
  return(s.df)
}

xtsToDf=function(s.xts, mvDateAtStart=FALSE)
{
  s.df=as.data.frame(s.xts)
  s.df$date=as.Date(rownames( s.df))
  if (mvDateAtStart==FALSE) return(s.df)
  colnam=colnames(s.df)
  dx=which(colnam=="date")
  s.dfB=cbind(date=s.df$date, s.df[, -dx]) 
  return(s.dfB)
}

dfToXts=function(df)
{
  if ("xts" %in% class(df)) return(df)
  n=names(df)
  if (any(n=="date")==FALSE) 
  {
    print("there is no 'date' column")
    return (NULL)
  }
  idx=which(n=="date")
  retV=as.xts(df[,-idx], order.by=(df[,idx]))
  return(retV)
}

convertYahooCode=function(iCode) {
  iCodeNoX=sub("X","", iCode)
  allYahooCode=NULL
  for (iCode in iCodeNoX)
  {
    if (is.na(as.numeric(iCode))) return (iCode)
    if (9901==iCode) yahooCode="^hsi"
    else if (9902==iCode) yahooCode="^ssec" 
    else if (9903==iCode) yahooCode="^n225" 
    else if (9904==iCode) yahooCode="^gspc" 
    else if (9905==iCode) yahooCode="^ftse" 
    else if (9906==iCode) yahooCode="^gdaxi" 
    else if (9907==iCode) yahooCode="^fchi" 
    else
    {
      code=paste("0000",iCode,".HK", sep="")
      yahooCode=substr(code, nchar(code)-6 , nchar(code))
    }
    allYahooCode=append(allYahooCode,yahooCode)
  }
  return ( allYahooCode)
}

convertYahooCode.OLD=function(iCode) {
  iCode=sub("X","", iCode)
  if (is.na(as.numeric(iCode))) return (iCode)
  if (9901==iCode) yahooCode="^hsi"
  else if (9902==iCode) yahooCode="^ssec" 
  else if (9903==iCode) yahooCode="^n225" 
  else if (9904==iCode) yahooCode="^gspc" 
  else if (9905==iCode) yahooCode="^ftse" 
  else if (9906==iCode) yahooCode="^gdaxi" 
  else if (9907==iCode) yahooCode="^fchi" 
  else
  {
    code=paste("0000",iCode,".HK", sep="")
    yahooCode=substr(code, nchar(code)-6 , nchar(code))
  }
  return ( yahooCode)
}

convertICode=function(yahooCode) {
  if (tolower(yahooCode)=="^hsi") iCode=9901 
  else if (tolower(yahooCode)=="^ssec")  iCode=9902
  else if (tolower(yahooCode)=="^n225")  iCode=9903
  else if (tolower(yahooCode)=="^gspc")  iCode=9904
  else if (tolower(yahooCode)=="^ftse")  iCode=9905
  else if (tolower(yahooCode)=="^gdaxi") iCode=9906
  else if (tolower(yahooCode)=="^fchi")  iCode=9907
  else iCode=as.numeric(substr(yahooCode, 1 , 4))
  return (iCode  ) 
}

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#### __TIME AND DATE __ ####
getHKDate=function(x) as.Date(substr(as.POSIXct(x,"CST"),1,10))

getNow=function() {as.numeric(format(Sys.time(),"%H%M"))}

tick_size=function(price)
{
  if (0.01<=price & price<0.25)        return(0.001)
  if (0.25<=price & price<0.50)        return(0.005)
  if (0.5<=price & price<10.00)        return(0.010)
  if (10.00<=price & price<20.00)      return(0.020)
  if (20.00<=price & price<100.00)     return(0.050)
  if (100.00<=price & price<200.00)    return(0.100)
  if (200.00<=price & price<500.00)    return(0.200)
  if (500.00<=price & price<1000.00)   return(0.500)
  if (1000.00<=price & price<2000.00)  return(1.000)
  if (2000.00<=price & price<5000.00)  return(2.000)
  if (5000.00<=price & price<9995.00)  return(5.000)
}

isTyphoon=function(toD)
{
  md=as.numeric(format(toD,"%m%d"))
  ymd=as.numeric(format(toD,"%y%m%d"))
  if (ymd==201013) return(TRUE)
  return (FALSE)
}

isHoliday=function(toD)
{
  md=as.numeric(format(toD,"%m%d"))
  ymd=as.numeric(format(toD,"%y%m%d"))
  if (md==101) return(TRUE)
  if (md==501) return(TRUE)
  if (md==701) return(TRUE)
  if (md==1001) return(TRUE)
  if (md==1225) return(TRUE)  
  if (ymd==140909) return(TRUE)
  if (ymd==141002) return(TRUE)
  if (ymd==141226) return(TRUE)
  if (ymd %in% c(150219, 150220, 150403, 150406, 150407, 
                 150525, 150903, 150928, 151021, 151226)) return (TRUE)   
  if (ymd %in% c(160208, 160209, 160210, 160325, 160328, 
                 160404, 160502, 160609, 160916, 161010, 161021,
                 161226, 161227, 160802)) return (TRUE)  
  if (ymd %in% c(170102, 170130, 170131, 170404, 170414, 
                 170417, 170503, 170530, 171002, 171005, 
                 171225, 171226, 170823)) return (TRUE)  
  if (ymd %in% c(180101, 180216, 180217, 180219, 180330, 
                 180331, 180402, 180405, 180501, 180618, 
                 180702, 180925,
                 181001, 181017, 181225, 181226)) return (TRUE)  
  if (ymd %in% c(190101, 190205, 190206, 190207, 190405, 
                 190419, 190420, 190422, 190501, 190513,190607 ,
                 190701, 190914,
                 191001, 191007, 191225, 191226)) return (TRUE)
  
  if (ymd %in% c(200101, 200125, 200127, 200128, 200404, 
                 200410, 200411, 200413, 200430, 200501, 
                 200625, 200701, 201001, 201002, 201026, 
                 201225, 201226)) return (TRUE)
  
  if (ymd %in% c(210101, 210212, 210213, 210215, 210402, 
                 210403, 210405, 210406, 210501, 210519, 
                 210614, 210701, 210922, 211001, 211014, 
                 211225, 211227)) return (TRUE)  
  if (ymd %in% c(220101, 220201, 220202, 220405, 220415, 
                 220416, 220418, 220502, 220509, 220603, 
                 220701, 220912, 221001, 221004, 221226, 
                 221227)) return (TRUE) 
  if (ymd %in% c(230102, 230123, 230124, 230125, 230405, 
                 230407, 230408, 230410, 230501, 230526, 
                 230622, 230701, 230930, 231002, 231023, 230717, 
                 231225, 231226)) return (TRUE) 
  if (ymd %in% c(240101, 240210, 240212, 240213, 240329, 
                 240401, 240404, 240501, 240515, 240610, 
                 240701, 240918, 241001, 241011,  
                 241225, 241226)) return (TRUE) 
  if (ymd %in% c(250101, 250129, 250130, 250131, 250404, 
                 250418, 250419, 250421, 250501, 250505, 
                 250531, 250701, 251001, 251007 ,251029,  
                 251225, 251226)) return (TRUE) 
  if (ymd %in% c(260101, 260217, 260218, 260219, 260403, 
                 260404, 260406, 260407, 260501, 260525, 
                 260619, 260701, 260926, 261001 ,261019,  
                 261225, 261226)) return (TRUE)   
  
  
  
  
    return (FALSE)
}
# 230717 typhoon

refreshSettle=function()
{
  manyD=c("2014-01-31", "2014-02-28", "2014-03-31", "2014-04-30",
          "2014-05-30", "2014-06-30", "2014-07-31", "2014-08-29",
          "2014-09-30", "2014-10-31", "2014-11-28", "2014-12-31", 
          "2015-01-30", "2015-02-27", "2015-03-31", "2015-12-31")
  settleD=NULL
  for (m in 1:length(manyD))
  {
    for (i in 0:5) settleD=c(settleD,u.Sys.Date(-i, manyD[m]))
  }
  print(as.Date(settleD))
  u.save(settleD, file="Dropbox/_data/settleDate.RData" , version=globalsaveversion)
}

#refreshSettle()
#isSettle("2014-01-31")
isSettle=function(inputD=u.Sys.Date())
{
  u.print("please run refreshSettle()")
  u.load("Dropbox/_data/settleDate.RData")
  
  if (as.Date(inputD) %in% as.Date(settleD)) return (TRUE)
  return (FALSE)
}

u.lastDate=function(...) u.last(...)

u.last=function(x,n=1,dateX=NULL)
{ 
  if ("xts" %in% class(x) ) 
  {
    return (index(last(x,n)[1]))
  }
  if ("data.frame" %in% class(x) )
  {
    dx=dateX
    if (is.null(dx)) dx=which(names(x)=="date")
    x=as.data.frame(x[order(x[,dx]),dx])
    tl=as.Date(tail(x[,1],n=n))[1]
    return (tl)
  }
  stop("error: not know how to get last date")
}

u.Sys.time2=function()
{
  tm=Sys.time()
  hms=as.numeric(format(tm, "%H%M%S"))
  hm=as.numeric(format(tm, "%H%M"))
  h=as.integer(hm/100)*100
  batch=(h/100-9)*60+(hm-h)
  m=as.numeric(format(tm, "%M"))
  s=as.numeric(format(tm, "%S"))
  return( data.frame(batch,hms,hm,h,m,s) )
}

u.Sys.time=function(digit=6) 
{
  if (4==digit) return(as.numeric(format(Sys.time(), "%H%M")))
  return(as.numeric(format(Sys.time(), "%H%M%S")))
}

isSunday=function(toD)
{
  dayOfWeek=format(toD, "%w")
  if (dayOfWeek==0) return (TRUE)
  return (FALSE)
}

isSaturday=function(toD)
{
  dayOfWeek=format(toD, "%w")
  if (dayOfWeek==6) return (TRUE)
  return (FALSE)
}

is.Settlement=function(sDate=u.Sys.Date())
{
  sDate=as.Date(sDate)
  if (length(sDate)!=1)
  {
    return(sapply(sDate, is.Settlement ))
  }
  if (is.na(sDate)) return (FALSE)
  today=u.Sys.Date(0, sDate)
  tomorrow=u.Sys.Date(1, sDate)
  dayBeforeTom=u.Sys.Date(2, sDate)
  sM1=format(today, format="%m")
  sM2=format(tomorrow, format="%m")
  nM=format(dayBeforeTom, format="%m")
  if (sM1==sM2 && sM2 != nM) return (TRUE)
  else return (FALSE)
}

getValidDate=function(dateV)
{
  dateV=as.Date(dateV)
  while (isHoliday(dateV) || isSunday(dateV) || isSaturday(dateV)) dateV=dateV-1
  return (dateV)  
}


u.Sys.Date=function(day=0, toD=Sys.Date(),sysSign=sign(day))
{ 
  # sysSign is used by the system, not user
  # unit test: u.Sys.Date(day=-10, toD="2015-01-31")=="2015-01-16"
  if (day!=0 && sysSign==0) toD=u.Sys.Date(day=0)
  toD=as.Date(toD)
  if (toD==Sys.Date() && as.numeric(format(Sys.time(),"%H"))<=6) toD=toD-1
  while(isHoliday(toD)==TRUE || isSunday(toD)==TRUE || isSaturday(toD)==TRUE) 
  {
    toD=toD+sysSign
    if (day==0 && sysSign==0) toD=toD-1
  }
  if (day!=0)
  {
    for (i in 1:abs(day))
    { 
      #u.print(toD)
      toD=toD+sign(day)
      dayOfWeek=format(toD, "%w")
      {
        if (isHoliday(toD)==TRUE) toD=u.Sys.Date(0, toD,sysSign)
        else if (dayOfWeek==0 && sign(day)<0) {toD=toD-2}
        else if (dayOfWeek==6 && sign(day)<0) toD=toD-1    
        else if (dayOfWeek==0 && sign(day)>0) toD=toD+1
        else if (dayOfWeek==6 && sign(day)>0) toD=toD+2   
      }
      if (isHoliday(toD)==TRUE) toD=u.Sys.Date(0, toD,sysSign)       
    }
  } # end if 
  if (isHoliday(toD)==TRUE) return(u.Sys.Date(0, toD,sysSign))
  return(toD)
}

last.u.Sys.Date=function(n=-10) { return (as.Date(sapply(c(-1:n), u.Sys.Date))) }
  



timelog2=function(x=""){
  if (x=="") paste("printed:", format(Sys.time(), "%m/%d %H:%M %p "), x)
  else paste("printed:", format(Sys.time(), "%m/%d %H:%M %p "), "|",x)
}

timelog=function() {
  tz=Sys.timezone()
  if (is.na(tz))  return(format(Sys.time(), "%H:%M"))
  if ("GMT"==tz || isCloud())
  {
    H=format(Sys.time(), "%H")
    M=format(Sys.time(), "%M")
    H=(as.numeric(H)+8)%%24 
    return( paste(H,":",M,sep=""))    
  }
  if ("PST"==tz)
  {
    H=format(Sys.time(), "%H")
    M=format(Sys.time(), "%M")
    H=(as.numeric(H)+16)%%24 
    return( paste(H,":",M,sep=""))    
  }
  
  return(format(Sys.time(), "%H:%M"))
}


isTradingTime=function(nolunch=FALSE)
{
  wd=format(Sys.time(), "%w") #sunday is 0
  nw=format(Sys.time(), "%H%M")
  nwd=as.numeric(wd)
  nnw=as.numeric(nw)
  if (nolunch==TRUE && 1202<=nnw && nnw<=1258) return (FALSE)
  if (930<nnw && nnw<1605 && wd!=0 && wd!=6) return (TRUE)
  return (FALSE)
}

startTimer=function() {globalPtm <<- proc.time() }

#stopTimer= function() 
#{
#  elapsed=c(hour=(proc.time()-globalPtm)[3])
#  if (isPolyu()) return(elapsed/3600)
#  if (isLenovo()) return(elapsed/3600)
#  if (isMac()) return(elapsed/3600)
#  if (isCloud()) return(elapsed/3600)
#  return(elapsed/3600)
#}

stopTimer=function() 
{
  elapsed=c(hour=(proc.time()-globalPtm)[3])
  elapsed=elapsed/3600
  HH=floor(elapsed)
  elapsed=u.float(elapsed,8)*60
  MM=floor(elapsed)
  elapsed=u.float(elapsed,8)*60
  SS=elapsed
  res=paste("elasped : ",HH,":",MM,":",SS,collapse="")
  if (isPolyu()) return(res)
  if (isLenovo()) return(res)
  if (isMac()) return(res)
  if (isCloud()) return(res)
  return(res)
}



testMachineSpeed <- function(n = 10000) 
{
  source("http://geogle.comp.polyu.edu.hk/performance.R")
}  # from Internet R

getMonth=function(from="2000-01-15", to=Sys.Date())
{
  s=loadData("^HSI")
  mr=monthlyReturn(s)
  period=xtsToDf(mr)
  period$monthly.returns=NULL
  period$to=c(period$date[-1],Sys.Date())
  names(period)=c("from","to")
  period=period[order(period$to,decreasing=TRUE ),]
  period=period[ from<=period$from & period$to<=to, ]
  return(period)
}

synTime=function(curTime, atLeastSec=9)
{
  #atLeastSec must be greater than 3
  #eg  print( (curTime=Sys.time()) )
  #    for (i in 1:10)  print( (curTime=synTime(curTime)) )
  min=as.numeric(format(curTime, "%M"))
  while (min==as.numeric((format(Sys.time(), "%M"))))
  {
    Sys.sleep(3)    
  }
  while (atLeastSec>=as.numeric((format(Sys.time(), "%S"))) )
  {
    Sys.sleep(1)  
  }
  return(Sys.time())
}

wakeUp=function(fromSec=40, toSec=45)
{
  sec=as.numeric(format(Sys.time(),"%S"))
  if (sec<=fromSec)
  { 
    Sys.sleep(fromSec-sec)
  } else if (sec>=toSec)
  {
    nextWait=fromSec+(60-sec)
    Sys.sleep(nextWait)
  }  
}


waitAt=function(sec=30)
{
  form="%H%M%S"
  form="%S"
  buffer=15
  time_H=as.numeric(format(Sys.time(), "%H"))
  time_M=as.numeric(format(Sys.time(), "%M"))
  time_S=as.numeric(format(Sys.time(), "%S"))
  end_H=time_H
  end_M=time_M
  end_S=time_S
  start_T=end_H*10000+end_M*100+end_S
  howLong=sec+(60-time_S) #assume secT>sec by default
  if (time_S<=sec) howLong=sec-time_S
  #x.print(howLong)
  end_S=end_S+howLong 
  if (60<=end_S) {end_S=end_S-60; end_M=end_M+1}
  end_T=end_H*10000+end_M*100+end_S
  curr_H=as.numeric(format(Sys.time(), "%H"))
  curr_M=as.numeric(format(Sys.time(), "%M"))
  curr_S=as.numeric(format(Sys.time(), "%S"))
  curr_T=curr_H*10000+curr_M*100+curr_S
  while(curr_T<end_T) 
  {
    x.print(start_T, curr_T, end_T)
    if (curr_S<=end_S) howLong=end_S-curr_S
    if (curr_S>end_S) howLong=60-curr_S
    #x.print(howLong)
    u.Sys.sleep(howLong)
    curr_H=as.numeric(format(Sys.time(), "%H"))
    curr_M=as.numeric(format(Sys.time(), "%M"))
    curr_S=as.numeric(format(Sys.time(), "%S"))
    curr_T=curr_H*10000+curr_M*100+curr_S
  }
}
u.Sys.sleep=function(sleep=60, waitAt=99)
{
  if (sleep==0 && waitAt<60)
  {
    currSec=as.numeric(format(Sys.time(),"%S"))
    if (currSec < waitAt) waitInterval=waitAt-currSec
    if (currSec >= waitAt) waitInterval=(60-currSec)+waitAt
    Sys.sleep(waitInterval)
    return (0)
  }
  sleepA=sleep%/%60
  sleepB=sleep%%60
  t1=as.numeric(format(Sys.time(), "%H%M%S"))
  t1=t1+sleepB+sleepA*100
  num=4
  if (as.numeric(format(Sys.time(), "%H"))<10) num=3
  if (as.numeric(format(Sys.time(), "%H"))==0) num=2
  
  tc=as.numeric(paste(substr(t1,1,num)))*100
  if (waitAt!=99) 
  {
    t1=as.numeric(paste(substr(t1,1,num)))*100+waitAt
    
  }
  tt=as.numeric(format(Sys.time(), "%H%M%S"))
  while (tt<t1) 
  {
    
    interval=(tc-tt)/100*60
    if (interval<10) interval=1
    #u.print(tt, "->", t1, "(",tc,")  set interval(sec):",interval)
    Sys.sleep(interval)
    tt=as.numeric(format(Sys.time(), "%H%M%S"))
  }
}


schedule=function(task, planTime=NULL, waitAt=8,
                  mp3file=NULL, 
                  repeatMode=FALSE, plot=FALSE,
                  sleep=60, showTime=TRUE,toPNG=FALSE)
{
  #riak=NULL #riak is not used anymore #planTime=1200
  startT=u.Sys.time2()
  startTimeOfDay=planTime[1]  
  #batch=startT["hm"]
  #startBatch=tm["hms"] #as.numeric(format(Sys.time(), "%H%M%S"))
  #startMin=as.numeric(format(Sys.time(), "%M"))
  #startSec=as.numeric(format(Sys.time(), "%S"))
  taskRun=FALSE
        #as.numeric(format(Sys.time(), "%H%M"))
  if (startTimeOfDay!=0)
  {
    #while (600<batch && batch<=startTimeOfDay)
    while (600<startT["hm"] &&  startT["hm"] <=startTimeOfDay)
    {
      u.Sys.sleep(55,waitAt=99)
      startT=u.Sys.time2()
      #startBatch=as.numeric(format(Sys.time(), "%H%M%S"))
      #startMin=as.numeric(format(Sys.time(), "%M"))
      #startSec=as.numeric(format(Sys.time(), "%S"))
      #batch=as.numeric(format(Sys.time(), "%H%M"))
    }
  }
  for (i in 1:2000)
  {
    taskRun=FALSE
    startT=u.Sys.time2()
    #startBatch=as.numeric(format(Sys.time(), "%H%M%S"))
    #startMin=as.numeric(format(Sys.time(), "%M"))
    #startSec=as.numeric(format(Sys.time(), "%S"))
    #batch=as.numeric(format(Sys.time(), "%H%M"))
    #u.print(batch)
    if (i!=1 && startT["hm"]  >= 1605 && repeatMode==FALSE) 
    {  print("schedule break after 1605"); break; }
    textH=""
    if (repeatMode==TRUE) textH="\n(repeat mode)"
    #startTimer
    if (is.null(planTime)||length(planTime)==1)
    {
      if (plot==TRUE) plot(c(1), col=0,main=paste("start task at ",Sys.time(),textH), xlab="", ylab="")
      #u.source("Dropbox/warrant/topoNum/Schedule_Task.R")
      if (repeatMode==TRUE) 
      {
        if (class(task)=="function")  task()  else source(task)
        taskRun=TRUE
      }
      #else if (i==1 || isTradingTime(nolunch=TRUE)) 
      else if ( (i==1 &&  (startT["hm"] <900 || 930<=startT["hm"] )) || isTradingTime(nolunch=TRUE))
      {
        #tryCatch(source(task), error = function(e) {print(paste("schedule:",e)); return(errorValue)}, finally=function(){})       
        if (class(task)=="function")  task()  else source(task)
        taskRun=TRUE
      }
      #u.Sys.sleep(sleep,waitAt)
    } else
    {
      if ( i==1 || startT["hm"]  %in% planTime)
      {
        if (plot==TRUE) plot(c(1), col=0,main=paste("start task at ",Sys.time(),textH), xlab="", ylab="")
        #tryCatch(source(task), error = function(e) {print(paste("schedule:",e)); return(errorValue)}, finally=function(){})       
        if (class(task)=="function")  task()  else source(task)
        #u.source("Dropbox/warrant/topoNum/Schedule_Task.R")
        if (isnot.null(mp3file)) play(mp3file)
      } else 
      {
        print("sleep=55 watiAt=99")
        sleep=55
        waitAt=99
        #u.Sys.sleep(55)
      }
    }
    if (toPNG==TRUE)
    {
      dev.copy(png,'myplot.png')
      dev.off()
    }
    endT=u.Sys.time2()
    #endBatch=as.numeric(format(Sys.time(), "%H%M%S"))
    #endMin=as.numeric(format(Sys.time(), "%M"))
    #endSec=as.numeric(format(Sys.time(), "%S"))
    diffSec=endT["s"]-startT["s"]
    diffMin=endT["m"]-startT["m"]-ifelse(diffSec>=0,0,1)
    diffSec=diffSec+ifelse(diffSec>=0,0,60)
    u.print(".run at:", startT["hms"],"~", endT["hms"]," (",diffMin,":",diffSec,")")    
    #u.print(".run at:", batch,"; ", "elasped:", round(stopTimer(),4))
    if (sleep==0 && taskRun==FALSE) u.Sys.sleep(sleep,waitAt)
    if (sleep!=0 && waitAt>=0) u.Sys.sleep(sleep,waitAt)
  } #end of for loop
}


#### __LOG AND DEBUG__ ####
logging=function(textMessage)
{
  if (tolower(textMessage)!="clear") 
  {
    cat(textMessage,file="^log.txt", sep="\n", append=TRUE)  
  }
  else 
  {
    cat("",file="^log.txt", append=FALSE)
  } 
}  



rolloutGetStatus=function()
{
  REALTIME <<- FALSE
  CHECKPERF <<- FALSE
  
  if (file.exists("status_realtime")) REALTIME <<-TRUE
  if (file.exists("status_checkperf")) CHECKPERF <<-TRUE
}







messageClear=function()
{
  messageLogHead <<- list()
  messageLogDetail <<- list()
  
}

messageLog=function(header=NULL, detail=NULL)
{
  if (is.null(header))
  {
    for (i in index(messageLogHead)) 
    {
      print(messageLogHead[[i]])
      print(messageLogDetail[[i]])
    }
  }
  if (isnot.null(detail)) 
  {
    len=length(messageLogHead)
    messageLogHead[[len+1]] <<- header
    messageLogDetail[[len+1]] <<-  detail
  }
}


#### __APPLY_RELATED__ ####

cumrollapply=function(data, width, FUN,fill=NULL, align="center", ...)
{
  if (!(align=="center"|| align=="right" || align=="left")) stop("error for align")
  if (!(is.na(fill) || is.null(fill))) stop("error for fill")
  FUN <- match.fun(FUN)
  trow=(nrow(data)-width+1)
  for (k in 1:(ncol(data)))
  {
    for (i in 1:trow )
    {
      index=NULL
      j=i+width-1
      r=FUN(data[i:j,k],...)
      if (align=="left") index=i 
      if (align=="right") index=j 
      if (align=="center") index=round((i+j)/2)
      data[index,k]=r
      if (i==1 && i<index) data[i:(index-1),k]=NA
      if (i==trow && nrow(data)>index) data[(index+1):(nrow(data)),k]=NA
    }
  }
  if (is.null(fill)) na.omit(data)
  return(data)
}

zapply=function(X, FUN, startRow=nrow(X), endRow=nrow(X), ...)
{
  # zapply(data.frame(c(1,2,3)), sum, 1, 3)
  # It returns a dataframe as 1, 3, 6
  # zapply(data.frame(c(1,2,3)), sum)
  # It returns 6
  totVal=NULL
  for (k in startRow:endRow) #k=1
  {
    s.x=X[1:k,]
    rVal=FUN(s.x, ...)
    totVal=rbind( totVal, rVal)
  }  
  return(totVal)
}

init=function(something, x, fill=0)
{
  if (isnot.null(something)) return (something)
  return(initial(x=x, fill=0))
}

initial=function(x, fill=NA, xR=range(1,nrow(x)), xC=range(1,ncol(x)))
{
  for (i in xR[1]:xR[2])
  {
    for (k in xC[1]:xC[2])
    {
      x[i,k]=fill
    }
  }
  return(x)
}

combine=function(xdf, ydf, align=c("left","right"), fill=NA)
{
  nrx=nrow(xdf)
  nry=nrow(ydf)
  dif=abs(nrx-nry)
  if ( nrx>nry) 
  {
    sampleRow=ydf[1,]
    sampleRow=initial(sampleRow, fill=fill)
    allSamlpleRow=NULL
    for (i in 1:dif) allSamlpleRow=rbind(allSamlpleRow, sampleRow)
    if (align=="left") ydf=rbind(ydf, allSamlpleRow)
    if (align=="right") ydf=rbind(allSamlpleRow, ydf)
  }
  if ( nry>nrx) 
  {
    sampleRow=xdf[1,]
    sampleRow=initial(sampleRow, fill=fill)
    allSamlpleRow=NULL
    for (i in 1:dif) allSamlpleRow=rbind(allSamlpleRow, sampleRow)
    if (align=="left") xdf=rbind(xdf, allSamlpleRow)
    if (align=="right") xdf=rbind(allSamlpleRow, xdf)
  } 
  return( cbind(xdf,ydf))
}

seqSegment=function(x)
{
  if (class(x)=="data.frame") x=x[,1]
  IND=x
  IND[1]=1
  lv=x[1]
  for (i in 2:length(x))
  {
    v=x[i]
    if (v==lv)
    {
      IND[i]=IND[i-1]
    } else
    {
      IND[i]=IND[i-1]+1
      lv=v
    }
  }
  return(IND)
}  

zby=function(data, INDICES, FUN,...)
{
  data=as.data.frame(data)
  IND=seqSegment(INDICES)
  byr=by(data, IND, FUN=FUN, ...)
  return(list(IND, byr))
}

#### __MATH__ ####

getMonoV=function(i.x, run=1)
{
  retV=1 ## the return value is in terms of 100%
  x=na.omit(i.x)
  if (run==1) x=normalize(x)
  #if (run==1) x =  (x / last(x, n=1))
  for (i in 1:(length(x)-1))
    if (x[i]>x[i+1])  retV=retV - ((x[i]-x[i+1])/x[i])
  retV=round(retV*100,0)
  if (run==2) return (retV)
  
  retV.R2=getMonoV(rev(x), run=2)
  retV.R2=(-1)*retV.R2
  
  retV=max(0, retV)
  retV.R2=min(0, retV.R2)
  if (retV==100) return (100)
  if (retV.R2==-100) return (-100)
  return(retV+retV.R2)
}

u.float=function(x,n=1) 
{
  x.s=paste(toString(x),"0000000000",collapse="",sep="")
  start=regexpr("\\.", x.s)[1]
  res=substr(x.s,start,start+n)
  return(as.numeric(res))
  
  #pw=10^(n-1)
  #x=x*pw
  ##need refactoring
  #tmp=(trunc.dig(x,n)-floor(x))/pw
  #return(round(tmp,n))
}
trunc.dig <- function(x, digits) trunc(x*10^digits)/10^digits 
# u.float(x=3.39,n=2)==0.09
# u.float(x=3.199,1)==0.1
# u.float(x=3.09,1)==0

#### __CONTROL__ ####
register=function(prgName="test",exeDate=Sys.Date(),update=0) 
{
  reg=data.frame(program="test", date=Sys.Date(), cnt=1)
  filename=uniPath("Dropbox/_data/reg.RData")
  if(!file.exists(filename)) save(reg, file = filename , version=globalsaveversion)
  reg=NULL
  load(file = filename)
  if (update==0) #read only
  {
    retV=subset( reg , program==prgName & date==exeDate)$cnt
    if (length(retV)==0) retV=0
    return(retV)
  }  
  if (update!=0) #update 
  {
    retV=subset( reg , program==prgName & date==exeDate)$cnt
    if (length(retV)!=0) reg[ reg[,"program"]==prgName & reg[,"date"]==exeDate  ,"cnt"] = retV+1
    if (length(retV)==0) 
    {
      retV=0
      myData=data.frame(program=prgName, date=exeDate, cnt=1)
      reg=rbind(reg, myData)
    }
    save(reg, file = filename , version=globalsaveversion)
    return(retV+1)
  }  
}

#### __SERVER__ ####

web=function(wdata, FUN_name, port=6666)
{
  if (!("xts" %in% class(wdata))) stop("error: must be xts")
  require(shiny)
  FUN <- match.fun(FUN_name) 
  web_Data_range=range(index(wdata))
  numDay=as.numeric(web_Data_range[2] - web_Data_range[1])
  
  runApp(port=port,
         host = getOption("shiny.host", "0.0.0.0"),
         list(
           ui   = pageWithSidebar
           ( 
             headerPanel(""),
             sidebarPanel
             (
               sliderInput("from", "from:", min =0 ,max = numDay, value = 0),
               sliderInput("to", "to:", min =0 ,max = numDay, value = numDay)
             ),
             mainPanel(plotOutput("wPlot")) 
           ),
           
           server= function(input, output) {
             
             output$wPlot = renderPlot({
               
               r=range(index(wdata))
               fromD=r[1] + input$from 
               toD=r[1] + input$to
               #u.print(fromD, "~", toD)
               tmp=subset(wdata, fromD<=index(wdata) & index(wdata)<=toD)
               FUN(tmp )
             })
           }
         ))
} # end of web


lightRed=rgb(1, 200/255, 200/255)
lightGreen=rgb(200/255, 1, 200/255)
lightYellow=rgb(1,1,153/255)

red  =function(alp=1) rgb(1,0,0,alp)
green=function(alp=1) rgb(0,1,0,alp) 
blue =function(alp=1) rgb(0,0,1,alp) 
black =function(alp=1) rgb(0,0,0,alp) 


groupA=c(2,5,6,11,12,16,17,23,83,1088,1880)
groupB=c(4,27,135,144,291,386,494,688,836,939,992,1199,1299,1398,1928,2318,2319,2388,2628,3328,3988)


options(error=dump.frames)

print(utilVersion)

jdbcURL="jdbc:mysql://99.myftp.org/nammik"



u.source("Dropbox/weblib/R/_shared/jdbc.R")

#list.of.packages <- c("RODBC")
#missing.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if (length(missing.packages)!=0 && missing.packages=="RODBC") 
#{
#  if (file.exists(uniPath("Dropbox/weblib/R/_shared/jdbc.R")))
#  {
#    print("loading (dropbox) jdbc.R .... ")
#    u.source("Dropbox/weblib/R/_shared/jdbc.R")
#  }
#  else
#  {
#    print("loading (Internet) jdbc.R .... ")
#    internet.source("jdbc.R", isOnline() );  
#  }
#} else 
#{ 
#  print("library RODBC")
#  library("RODBC")
#}

##### DNA

getDNA=function()
{
  dna=read.csv(paste(getDropboxDir(),"_data/dna.txt",sep=""))
  dna$date=as.Date(dna$date)
  dna=data.frame(lapply(dna, as.character), stringsAsFactors=FALSE)
  dna$pat = trim(dna$pat)
  return(dna)
}


genMyStudy=function()
{
  library(xtable)
  library(lubridate)
  dna=getDNA()
  ndna=names(dna)
  ndna[1]="wd"
  ndna=c("date", ndna)
  dna=data.frame(date=dna$date, dna)
  names(dna)=ndna
  dna=data.frame(lapply(dna, as.character), stringsAsFactors=FALSE)
  dna$wd=round(wday(as.Date(dna$date))-1 ,0)
  
  dna$bgcolor=gsub("yellow", "#ffff00", dna$bgcolor)
  dna$bgcolor=gsub("blue", "#6699ff", dna$bgcolor)
  dna$bgcolor=gsub("green", "#66ff66", dna$bgcolor)
  dna$bgcolor=gsub("magenta", "#ff66ff", dna$bgcolor)
  dna$bgcolor=gsub("red", "#ff6666", dna$bgcolor)
  dna$bgcolor=gsub("purple", "#a64dff", dna$bgcolor)
  
  DX=which(trim(dna$bgcolor)!="")
  tmp=dna[DX,]$pat
  tmp.col=dna[DX,]$bgcolor
  dna[DX,]$pat=addColor(tmp, tmp.col )
  
  ###
  ### settlement
  ###
  
  
  
  DX=which(is.Settlement(dna$date))
  dna$wd=as.character(dna$wd)
  tmp=trim(dna[DX,]$wd)
  tmp.png=paste0("c_",dna[DX,1],".png")
  tmp=addLink(tmp, tmp.png)
  dna[DX,]$wd=addColor(tmp, "#ff0000" )
  
  DX=which(trim(dna$simPat)!="")
  tmp=trim(dna[DX,]$simPat)
  tmp.png=paste("../MyStudy2/",tmp,".png",sep="")
  dna[DX,]$simPat=addLink(tmp,tmp.png )
  
  DX=which(trim(dna$date)!="")
  tmp=trim(dna[DX,]$date)
  tmp.png=paste("p",tmp,"-",substr(weekdays(as.Date(tmp)),1,3),".png", sep="")
  tmp.htm=paste("p",tmp,"-",substr(weekdays(as.Date(tmp)),1,3),".htm", sep="")
  dna[DX,]$date=addLink(tmp,tmp.htm )
  
  dna$bgcolor=NULL 
  
  fileName=paste0(uniPath("Dropbox/temp/MyStudy"),"/index.html")
  makeHTML(dna, fileName, caption=paste(timelog2()," genMyStudy from util.R"))
  
  tmpStr=readChar(fileName, file.info(fileName)$size)
  tmpStr=gsub("a href", "a target='_blank' href", tmpStr)
  write(tmpStr, fileName )
  print("please run : exec system('/Users/admin/Documents/Dropbox/temp/MyStudy/go.sh')")
  
  htmlTemplate="<!DOCTYPE html><html><style>body {font-family: Arial,serif,Times;}</style> <body><a href='index.html'>home</a>&nbsp;&nbsp; <a href='yesterdayhtm'>yesterdaypng</a>  <strong><font color='red'>todaypngtext</font></strong>  <a href='tomorrowhtm'>tomorrowpng</a>   <img src='todaypng'> </body></html>"
  
  totDay=length(tmp.htm)
  for(i in 1:totDay)
  {
    #i=1
    tempHtml=gsub("todaypngtext", gsub(".png","",tmp.png[i]) ,htmlTemplate)
    tempHtml=gsub("todaypng"    , tmp.png[i]                 ,tempHtml)
    if ((i-1)>0)      tempHtml=gsub("tomorrowpng"  ,  gsub(".png","",tmp.png[i-1]) ,tempHtml) else  tempHtml=gsub("tomorrowpng"  ,  "" ,tempHtml)
    if ((i-1)>0)      tempHtml=gsub("tomorrowhtm" ,  tmp.htm[i-1] ,tempHtml)  else  tempHtml=gsub("tomorrowhtm" , "" ,tempHtml) 
    if ((i+1)<totDay) tempHtml=gsub("yesterdaypng" ,  gsub(".png","",tmp.png[i+1]) ,tempHtml) else  tempHtml=gsub("yesterdaypng" , "" ,tempHtml)
    if ((i+1)<totDay) tempHtml=gsub("yesterdayhtm",  tmp.htm[i+1] ,tempHtml)  else  tempHtml=gsub("yesterdayhtm",  "" ,tempHtml) 
    fileName=paste0(uniPath("Dropbox/temp/MyStudy/"),tmp.htm[i])
    write(tempHtml, fileName )
  }
}


showChart=function(csdat)
{
  cthm=chart_theme()
  cthm$col$up.col="green"
  chart_Series(csdat, theme=cthm)
}


showDNA=function(csdat, cex=0.8)
{
  dna=getDNA()
  #hsi=loadData("^hsi",abbr=FALSE)
  #csdat=tail(hsi,n=numDay)
  yPos=max(csdat$High)
  for (k in 1:nrow(csdat))
  {  # k=1
    da=index(csdat[k,])    
    wh=which(dna$date==da)
    if (length(wh)>0)
    {
      text(k, yPos, dna[wh,"dna"], cex=cex)
      text(k, yPos-100, dna[wh,"ana"], cex=cex)
    }
  }
}


#### __TO_HTML__ ####

addLink=function(txt,url)
{
  return( paste("<a href='",url,"'>",txt,"</a>",sep="" ))
}

addColor=function(txt,colcode)
{
  return (paste("<span  style='background-color:",colcode,"'>&nbsp",txt,"&nbsp</span>",sep="" )) 
}

makeHTML=function(df, filename, caption="")
{
  print(xtable(df, caption),  type = 'html', sanitize.text.function = function(x) x, file=filename,  caption.placement = "top")  
}


########## 2022

tableMaster=function (s, coln = c("e.o.po", "e.o.ph", "e.o.pl", "e.o.pc", 
                                  "e.c.po", "e.c.ph", "e.c.pl", "e.c.pc")) 
{
  tablel = list()
  lencoln = length(coln)
  if (lencoln == 1) {
    result = table(s[, coln])
    result = as.data.frame(result)
    result$Var1 = as.numeric(as.character(result$Var1))
    result$pref=round(result$Freq/sum(result$Freq),2)
  }
  else {
    for (i in 1:lencoln) {
      tablel[[i]] = table(s[, coln[i]])
    }
    result = Reduce(function(...) merge(..., all = TRUE, 
                                        by = "Var1"), tablel)
    colnames(result) = c("dist", coln)
  }
  return(result)
}



myGrep=function(x) { 
  e <- environment() # current environment
  p <- parent.env(e)
  tmp=ls(envir =p)
  dx=grep( x  , tmp)
  return(tmp[dx]) 
}




#### __DISTRIBUTED__ ####

Tracker.getLogFile=function() return("/Users/admin/Documents/Dropbox/dcim/tracker.txt")

Tracker.clean=function()
{
  taskTracker=data.frame(machine=c("dummy"), time=c(Sys.time()), status=c("x"))
  write.table(taskTracker, file=Tracker.getLogFile(), sep="\t") 
}

Tracker.ls=function()
{
  taskTracker <- read.table(file=Tracker.getLogFile(), header=TRUE, sep="\t")
  print(taskTracker)
}


Tracker.rm=function(t.hostname=getHostName())
{
  taskTracker <- read.table(file=Tracker.getLogFile(), header=TRUE, sep="\t") 
  if (any(taskTracker$machine==t.hostname))
  {
    timestamp= as.vector(taskTracker$time[ taskTracker$machine==t.hostname])
    taskTracker = taskTracker[ taskTracker$machine!=t.hostname    , ]
    write.table(taskTracker, file=Tracker.getLogFile(), sep="\t") 
    Sys.sleep(120)
    
    taskTracker <- read.table(file=Tracker.getLogFile(), header=TRUE, sep="\t") 
    if (any(taskTracker$machine==t.hostname))
    {
      timestamp2= as.vector(taskTracker$time[ taskTracker$machine==t.hostname])
      if (timestamp2==timestamp)
      {
        taskTracker = taskTracker[ taskTracker$machine!=t.hostname    , ]
        write.table(taskTracker, file=Tracker.getLogFile(), sep="\t") 
      }
    }
  }
}


Tracker.isRunning=function(t.hostname=getHostName())
{
  taskTracker <- read.table(file=Tracker.getLogFile(), header=TRUE, sep="\t")
  return(t.hostname %in% taskTracker$machine)
}


Tracker.startTask=function(status="R")
{
  status=as.character(status)
  checkIf(Tracker.isRunning(), "stop: check tracker file") 
  taskTracker <- read.table(file=Tracker.getLogFile(), header=TRUE, sep="\t")
  taskTracker=rbind(taskTracker, data.frame(machine=c(getHostName()), time=as.character(Sys.time()), status=status ))
  write.table(taskTracker, file=Tracker.getLogFile(), sep="\t") 
  return (1)
}


Tracker.waitForAllFinished=function(aM, timeoutV, minPerWait)
{
  for (cntTimeout in 0:timeoutV)
  {
    t.status=sapply(aM, FUN=Tracker.isRunning)
    if ( all(!t.status[-1] )==TRUE)
    {
      Tracker.clean()      
      break
    } else Sys.sleep(minPerWait*60)
  }
}

Tracker.loadData=function()
{
  FS_ENG.data=NULL
  load(file="/Users/admin/Documents/Dropbox/dcim/rpt_dc.RData")
  return (FS_ENG.data)
}

#internet.source("riak.R" ,  internet=FALSE)


my.redisConnect=function(host="158.132.11.247", port=2009) 
{
  redisConnect(host, port)
  print("redisGet('x'); redisSet('x',rnorm(5))")
}

#FUN FROM INTERNET
        
##arrange df vars by position
##'vars' must be a named vector, e.g. c("var.name"=1)
# Sample arrange.vars(df, c("colname"=1))
# https://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-a-data-frame
arrange.vars <- function(data, vars){
    ##stop if not a data.frame (but should work for matrices as well)
    stopifnot(is.data.frame(data))

    ##sort out inputs
    data.nms <- names(data)
    var.nr <- length(data.nms)
    var.nms <- names(vars)
    var.pos <- vars
    ##sanity checks
    stopifnot( !any(duplicated(var.nms)), 
               !any(duplicated(var.pos)) )
    stopifnot( is.character(var.nms), 
               is.numeric(var.pos) )
    stopifnot( all(var.nms %in% data.nms) )
    stopifnot( all(var.pos > 0), 
               all(var.pos <= var.nr) )

    ##prepare output
    out.vec <- character(var.nr)
    out.vec[var.pos] <- var.nms
    out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
    stopifnot( length(out.vec)==var.nr )

    ##re-arrange vars by position
    data <- data[ , out.vec]
    return(data)
}

#  arrange.vars.last(s1, c("fkill", "kill5", "wkd")) 
chead.last=function(...) arrange.vars.last (...)
arrange.vars.last=function(data, colvec)
{
  ncolv=ncol(data)
  
  for (i in 1:length(colvec))
  {
    nVec=ncolv
    names(nVec)=colvec[i]
    data=arrange.vars(data, nVec )
  }
  return(data)
}  

toString.data.frame = function (object, ..., digits=NULL, quote=FALSE, right=TRUE, row.names=TRUE) {
  nRows = length(row.names(object));
  if (length(object)==0) {
    return(paste(
      sprintf(ngettext(nRows, "data frame with 0 columns and %d row", "data frame with 0 columns and %d rows")
              , nRows)
      , "\\n", sep = "")
    ); 
  } else if (nRows==0) {
    return(gettext("<0 rows> (or 0-length row.names)\\n")); 
  } else {
    # get text-formatted version of the data.frame
    m = as.matrix(format.data.frame(object, digits=digits, na.encode=FALSE)); 
    # define row-names (if required)
    if (isTRUE(row.names)) {
      rowNames = dimnames(object)[[1]];
      if(is.null(rowNames)) { 
        # no row header available -> use row numbers
        rowNames = as.character(1:NROW(m));
      } 
      # add empty header (used with column headers)
      rowNames = c("", rowNames);
    }
    # add column headers
    m = rbind(dimnames(m)[[2]], m);
    # add row headers
    m = cbind(rowNames, m);
    # max-length per-column
    maxLen = apply(apply(m, c(1,2), stringr::str_length), 2, max, na.rm=TRUE);
    
    # add right padding
    ##  t is needed because "If each call to FUN returns a vector of length n, then apply returns an array of dimension c(n, dim(X)[MARGIN])"
    m = t(apply(m, 1, stringr::str_pad, width=maxLen, side="right"));
    m = t(apply(m, 1, stringr::str_pad, width=maxLen+3, side="left"));
    # merge columns
    m = apply(m, 1, paste, collapse="");
    # merge rows (and return)
    return(paste(m, collapse="\n"));
  }
}

callDeepSkyData_OLD=function()
{
  require(session)

  if (!exists('masterData'))
  {  print("<<<  loading >>>")
    load("/Users/admin/Documents/Dropbox/jupyter/workspace.RData")
    restore.session(file='/Users/admin/Documents/Dropbox/jupyter/worksession.Rda')
    print(paste0("-- Last Loading Executed on ",Sys.time()," --"))
    print(paste0("last_save: ", g_last_save))
  } else
  {
    g_last_save=Sys.time()
    print(g_last_save) 
    print("<<< saving >>>")
    save.image  ("/Users/admin/Documents/Dropbox/jupyter/workspace.RData")
    save.session('/Users/admin/Documents/Dropbox/jupyter/worksession.Rda')
    print(paste0("-- Last Saving Executed on ",Sys.time()," --"))    
  }
  print("-----")
  print("-----")
  print("----- Data Information")
  print(paste("total:", length(masterTDF), " asset:",   paste(names(masterTDF), collapse=";") )  )
  scode='^HSI'
  s<<-masterTDF[[scode]]
}

callDeepSkyData=function () 
{
  require(session)
  deepSkyFile="/Users/admin/Documents/Dropbox/jupyter/workspace.RData"
  wipFile    ="/Users/admin/Documents/Dropbox/jupyter/workspacewip.RData"
  deepSky_SessFile="/Users/admin/Documents/Dropbox/jupyter/worksession.Rda"
  wip_SessFile="/Users/admin/Documents/Dropbox/jupyter/worksessionwip.Rda"
  if (!exists("masterData")) {
    
    print("<<<  loading >>>")
    if (file.exists(wipFile))
    {
      if (file.info(deepSkyFile)$ctime<file.info(wipFile)$ctime)
      {
        print("load wip")
        load(wipFile)
        restore.session(file = wip_SessFile) 
      } else
      {
        load(deepSkyFile)
        restore.session(file = deepSky_SessFile)            
      }
    } else
    {
      load(deepSkyFile)
      restore.session(file = deepSky_SessFile)
    }
    print(paste0("-- Last Loading Executed on ", Sys.time(), 
                 " --"))
    print(paste0("last_save: ", g_last_save))
  }
  else {
    
    g_last_save = Sys.time()
    print(g_last_save)
    print("<<< saving >>>")
    save.image(wipFile)
    save.session(wip_SessFile)
    print(paste0("-- Last Saving Executed on ", Sys.time(), 
                 " --"))
  }
  print("-----")
  print("-----")
  print("----- Data Information")
  print(paste("total:", length(masterTDF), " asset:", paste(names(masterTDF), 
                                                            collapse = ";")))
  scode = "^HSI"
  s <<- masterTDF[[scode]]
}