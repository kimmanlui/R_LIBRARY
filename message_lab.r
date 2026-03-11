
getEnglishMsg=function(res, message='')
{
    upcnt=0
    downcnt=0
    if  (any (as.vector( res[ grepl(">=", res$Prediction), 'Odd']<0.25) ))
    {
        message=paste0(message,"\nthere is a low chance it will go up. \n")
        downcnt=downcnt+1
    }
    if  (any (as.vector(res[grepl(">=", res$Prediction), 'Odd']>0.75) ))
    {
        message=paste0(message,"\nThere is a high chance it will go up.\n")
        upcnt=upcnt+1
    }
    if  (any (as.vector(res[grepl("<=", res$Prediction), 'Odd']<0.25) ))
    {
        message=paste0(message,"\nthere is a low chance it will go down. \n")
        upcnt=upcnt+1
    }
    if  (any (as.vector(res[grepl("<=", res$Prediction), 'Odd']>0.75) ))
    {
        message=paste0(message,"\nThere is a high chance it will go down. \n")
        downcnt=downcnt+1
    }
   if (downcnt==0 && upcnt==0) message='There is no clear or useful forecast.'
     if (downcnt>0 && upcnt>0) message='The price may fluctuate within a narrow range'
    print(message)
    return(message)
}

getChineseMsg=function(res, messageV='', type='message', lowerV=0.25, upperV=0.75)
{
    upcnt=0
    downcnt=0
    pattern=NULL

    M1TIME="[TIME]"
    if (any(grepl("pv.f1", res$Prediction))) M1TIME="今天"
    if (any(grepl("wkhwk_pv.f1", res$Prediction))) M1TIME="本週和上週"
    
	if  (any (as.vector( res[ grepl(">=", res$Prediction), 'Odd']<lowerV) ))
	{ 
        M1="上漲高於#TIME#最高價的機率為低.\n"
        M1=gsub('#TIME#', M1TIME, M1)
		messageV=paste0(messageV,M1)
        downcnt=downcnt+1
        pattern=c(pattern,'not 2')
	}

	if  (any (as.vector( res[ grepl(">=", res$Prediction), 'Odd'] >upperV) ))
	{
		M1="上漲高於#TIME#最高價的機率為高.\n"
        M1=gsub('#TIME#', M1TIME, M1)
		messageV=paste0(messageV,M1)
        upcnt=upcnt+1
        pattern=c(pattern,'2')
	}
     
	if  (any (as.vector( res[ grepl("<=", res$Prediction), 'Odd']<lowerV) ))
	{
		M1="下跌低於#TIME#最低價的機率為低. \n"
        M1=gsub('#TIME#', M1TIME, M1)
		messageV=paste0(messageV,M1)
        upcnt=upcnt+1
        pattern=c(pattern,'not -2')
	}
	if  (any (as.vector(res[ grepl("<=", res$Prediction) , 'Odd']>upperV) ))
	{
		M1="下跌低於#TIME#最低價的機率為高. \n"
        M1=gsub('#TIME#', M1TIME, M1)
		messageV=paste0(messageV,M1)
        downcnt=downcnt+1
        pattern=c(pattern,'-2')
	}
	if (downcnt==0 && upcnt==0) messageV='目前尚無明確或有用的預測。'
    if (downcnt>0 && upcnt>0) messageV='價格可能在不大範圍內波動。'
   
    expected=0 #Used for backtest

    if (downcnt==0 && upcnt==0) expected=0
    if (downcnt>0 && upcnt>0) expected=0
    if (length(pattern)==1) expected=pattern
    if (length(pattern)>1 && downcnt==0) expected="2"
    if (length(pattern)>1 && upcnt==0) expected="-2"    
    if (type=='pattern') return (list(pattern=pattern,expected=expected))
    
	if (type!='pattern') print(messageV)
	return(messageV)
}

standardName=function(db, shrink=1)
{   #c( old -> new)
    namelist=list(c('targetcond','Prediction'), 
                  c('Pb','Odd'), c('hit', 'CN'), c('total','tot'), c('odd', 'Odd')
                  )
    for (i in 1:length(namelist))
    {
        oldname=namelist[[i]][1]
        newname=namelist[[i]][2]
      if (oldname %in% names(db)) 
                db <- db %>% rename(`:=`(!!newname, all_of(oldname)))
    }
    if (shrink==1) db=db[, c('Prediction','Odd','CN','Tot')]
    rownames(db)=NULL
    return(db)
}

getMethodText=function() 
{
  return("方法")
}
getPredictionText=function() 
{
 return("預測")
}
getPastPerfText=function(numday=0) 
{
	if (numday!=0)
	{
		return(paste("過往",numday,"天預測表現"))
	}
	return("過往預測表現")
}

getChineseTitle=function()
{
    msg='自家研發2.1'
	return(msg)

}


