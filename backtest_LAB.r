checkdf_return_string=function (df, actualColname = "pv", expectColname = "expected", debug=0) 
{
    df = df[order(df$date, decreasing = TRUE), ]
    resString = NULL
    for (i in 1:nrow(df)) {
        actual = df[i, actualColname]
        expect = df[i, expectColname]
        resultSingle=checkTestResultSingle(actual, expect)
        if (debug==1) print(data.frame(actual, expect, resultSingle))
        resString = c(resString, resultSingle)
        
    }
    retV = paste(resString, collapse = "")
    retV = gsub("NA", "u", gsub("0", "x", gsub("1", "o", retV)))
    return(retV)
}


checkTestResult=function(df, actualColname='actual', expectedColname='expected')
{
    hit_for_not=0
    total_for_not=0
    if ((nrow(df))!=0)
    {
        df$hit=0
       
        for (i in 1:nrow(df))
        {
            actual=df[i, actualColname ]
            expected=df[i, expectedColname ]
            df[i,'hit']=checkTestResultSingle(actual, expected)             
        }
        hit_for_not=sum(df$hit)
        total_for_not=(nrow(df))
        odd_for_not=round(hit_for_not/total_for_not,2)
    }
    res=data.frame( hit_for_not,total_for_not,odd_for_not)
    return(list(res, df)) 
}

checkTestResultSingle=function(actual, expected)
{
     if (expected==0) return(NA)

    if (!grepl('not' , expected) && actual==2 && expected==2) return(1)
    if (!grepl('not' , expected) && actual==-2 && expected==-2) return(1)
    if (actual==1 && grepl('not' , expected)) return(1)
     if (actual==-1 && grepl('not' , expected)) return(1)
    if (actual<=-2 && grepl('not 2' , expected)) return(1)
    if (actual>=2 && grepl('not -2' , expected)) return(1)
    if (actual>=2 && expected>=2 ) return(1)
    if (actual<=-2 && expected<=-2 ) return(1)
    return(0)
}

prepare_backtest_result_for_daily22=function(hsi_data_backtest)
{
    backtest_res=NULL 
    for (i in 2:nrow(hsi_data_backtest))
    {
        hsi_rt_backtest=hsi_data_backtest[i,]
        actual=hsi_data_backtest[i,'pv']
        
        res=extractRecord(hsi_rt_backtest, APP_Daily22DB, col_name=c('Weekday', 'bd','bp'), type=c(0,1,1))
        res=standardName(res)
        res=res[res$Tot>5,]
      
        message=""
        message=getChineseMsg(res, message, type='message')
        expectedList=getChineseMsg(res, message, type='pattern',lowerV=0.25)
        pattern=expectedList[['pattern']]
        pattern=paste(pattern, collapse = " , ")
        expected=expectedList[['expected']]
        if (is.null(expected)) expected='_NULL_'
        
        print(res)
        print(data.frame(i , actual, expected, pattern , message))
        cat('----------------------------\n')
        backtest_res=rbind(backtest_res, data.frame(actual, expected))
    }
    return(backtest_res)
}

processTestResult=function(backtest_res)
{
    ExpectedZeroCount=nrow(backtest_res[backtest_res$expected==0,])
    backtest_res_non_zero=backtest_res[backtest_res$expected!=0,]
        
    NonNotResult=backtest_res_non_zero[!grepl('not', backtest_res_non_zero$expected), ]
    NotResult=backtest_res_non_zero[grepl('not', backtest_res_non_zero$expected), ]
    res_for_non_not=checkTestResult(NonNotResult)
    res_for_not=checkTestResult(NotResult)
    


    return(list(ExpectedZeroCount, res_for_non_not[[1]], res_for_non_not[[2]], 
           res_for_not[[1]], res_for_not[[2]]))
}




