
full=read.csv("Melbourne_housing_FULL.csv")

s

























##################################################################
function(train,test,start_combination,end_combination,target)
{
  for(j in seq(1,13))
  {
    combn_col = combn(colnames,m=2)
    t_list[[j]]=combn_col
  }
df=data.frame(att="",RMSE=0,stringsAsFactors = F)
parse_list=function(t)
{
  
  for(i in 1:ncol(t))
  {
    rf_model <- randomForest(as.formula(paste0(target,"~",paste0( t[,i],collapse = "+"))),data = train,ntree=121)
    
    test$predicted <- predict(rf_model,test)
    
    df=rbind(df,c(paste0( t[,i],collapse = "+"),sum(sqrt((actual$medv-test$predicted)^2/sum(actual$medv)))))
    
  }
  return(df)
}

l<- lapply(t_list[start_combination:end_combination], parse_list)


l_new <- lapply(l,function(comb_df1){
  temp<- as.data.frame(comb_df1)
  temp[temp$RMSE<4.5,]
})

return(l_new)
}


data("")

rf_final(train,test,start_combination,end_combination,target)