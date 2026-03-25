genTrafficLight_OLD=function(traffic_light)
{
    for (i in 1:length(traffic_light))
    {
        if (length(traffic_light[[i]])==2)
        {
            if (traffic_light[[i]][1]=='not 2' && traffic_light[[i]][2] == 'not -2') 
            {
                traffic_light[[i]]=gsub('###COLOR###', paste(LIGHTGRAY, YELLOW, LIGHTGRAY), TRAFFIC_LIGHT)
                next
            } else if (traffic_light[[i]][1]=='not -2' && traffic_light[[i]][2]=='not 2') 
            {
                traffic_light[[i]]=gsub('###COLOR###', paste(LIGHTGRAY, YELLOW, LIGHTGRAY), TRAFFIC_LIGHT)
                next
            }            
        }
        traffic_light[[i]]=sort(traffic_light[[i]] )
        traffic_light[[i]]=  traffic_light[[i]][1]
        
      if (traffic_light[[i]]=='not 2')
      { 
          traffic_light[[i]]=gsub('###COLOR###', paste(RED, YELLOW, LIGHTGRAY), TRAFFIC_LIGHT)
       } else if (traffic_light[[i]]=='2') 
      {
          traffic_light[[i]]=gsub('###COLOR###', paste(LIGHTGRAY, LIGHTGRAY, GREEN), TRAFFIC_LIGHT)
       } else if (traffic_light[[i]]=='-2') 
     {
          traffic_light[[i]]=gsub('###COLOR###', paste(RED, LIGHTGRAY, LIGHTGRAY), TRAFFIC_LIGHT)
    } else if (traffic_light[[i]]=='not -2')
    {
          traffic_light[[i]]=gsub('###COLOR###', paste(LIGHTGRAY, YELLOW, GREEN), TRAFFIC_LIGHT)
    } else 
        {
          traffic_light[[i]]=gsub('###COLOR###', paste(LIGHTGRAY, LIGHTGRAY, LIGHTGRAY), TRAFFIC_LIGHT)
        }
    }
    return(traffic_light)
}









genTrafficLight <- function(traffic_light) {
  # Ensure it's a list to allow uniform processing
  if (!is.list(traffic_light)) traffic_light <- as.list(traffic_light)

  # helper to produce the replaced template
  fill_template <- function(cols) {
    gsub("###COLOR###", paste(cols, collapse = " "), TRAFFIC_LIGHT)
  }

  # map each element to the appropriate template string
  result <- lapply(traffic_light, function(el) {
    # make sure el is character
    el <- as.character(el)

    # handle two-element special cases (order-insensitive)
    if (length(el) == 2) {
      # use a set-like comparison (order-independent)
      two_set <- sort(el)
      # compare to known combos (sorted)
      if (identical(two_set, sort(c("not 2", "not -2")))) {
        return(fill_template(c(LIGHTGRAY, YELLOW, LIGHTGRAY)))
      }
      # add other two-element combos here if needed
    }

    # handle single-element cases (or fallback if length>1 non-matching)
    val <- el[1]  # choose first element if vector length > 1 and not matched above
    if (val == "not 2") {
      fill_template(c(RED, YELLOW, LIGHTGRAY))
    } else if (val == "2") {
      fill_template(c(LIGHTGRAY, LIGHTGRAY, GREEN))
    } else if (val == "-2") {
      fill_template(c(RED, LIGHTGRAY, LIGHTGRAY))
    } else if (val == "not -2") {
      fill_template(c(LIGHTGRAY, YELLOW, GREEN))
    } else {
      fill_template(c(LIGHTGRAY, LIGHTGRAY, LIGHTGRAY))
    }
  })

  # return same type as input: if original was a vector, return character vector
  if (!is.list(traffic_light)) return(unlist(result))
  result
}







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




