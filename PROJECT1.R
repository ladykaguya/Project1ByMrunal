
setwd("/Users/mrunalsawant/Desktop/Project/project 1/")

ld_test=read.csv("housing_test.csv",stringsAsFactors = F)
ld_train= read.csv("housing_train.csv",stringsAsFactors = F)
ld_test$Price=NA

ld_test$data='test'
ld_train$data='train'

ld_all=rbind(ld_test,ld_train)

library(dplyr)
glimpse(ld_all)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}
table(ld_all$Distance)

  ld_all=CreateDummies(ld_all ,"Rooms",100)
  ld_all=CreateDummies(ld_all ,"Postcode",100)  
##  ld_all=CreateDummies(ld_all ,"Car",100)
  
###ld_all$Bedroom2[is.na(ld_all$Bedroom2)]=round(mean(ld_all$Bedroom2,na.rm = T),0)

  ld_all=ld_all %>% 
    select(-SellerG,-Suburb,-Address)
  
  lapply(ld_all,function(x) sum(is.na(x)))
  #checking number of NA values
  
  for(col in names(ld_all)){
    
    if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data"))){
      
      ld_all[is.na(ld_all[,col]),col]=mean(ld_all[,col],na.rm=T)
    }
    
  }  
  
  ld_train=ld_all %>% filter(data=='train') %>% select(-data)
  ld_test=ld_all %>% filter(data=='test') %>% select(-data,Price)
  
  set.seed(2)
  s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
  ld_train1=ld_train[s,]
  ld_train2=ld_train[-s,]
  glimpse(ld_train1)

  fit=lm(Price~.,data=ld_train1)  

  library(car)  
  sort(vif(fit),decreasing = T) [1:3]
  
  fit=step(fit)
 fit=lm(Price ~ Method + Distance + Bedroom2 + Bathroom + Car + Landsize + 
          BuildingArea + YearBuilt + CouncilArea + Rooms_1 + Rooms_4 + 
          Rooms_2 + Rooms_3 + Postcode_3068 + Postcode_3071 + Postcode_3147 + 
          Postcode_3013 + Postcode_3103 + Postcode_3124 + Postcode_3145 + 
          Postcode_3127 + Postcode_3081 + Postcode_3207 + Postcode_3044 + 
          Postcode_3187 + Postcode_3031 + Postcode_3104 + Postcode_3181 + 
          Postcode_3015 + Postcode_3011 + Postcode_3188 + Postcode_3101 + 
          Postcode_3186 + Postcode_3146 + Postcode_3056 + Postcode_3141 + 
          Postcode_3182 + Postcode_3012 + Postcode_3072 + Postcode_3204 + 
          Postcode_3058 + Postcode_3163 + Postcode_3040 + Postcode_3032 + 
          Postcode_3121 + Postcode_3165 + Postcode_3046 + Postcode_3020 + 
          Postcode_3073 +Type,
         data=ld_train1)

  summary(fit)  
  
  val.pred=predict(fit,newdata=ld_train2)
  
  errors=ld_train2$Price-val.pred
  
  errors**2 %>% mean() %>% sqrt()
  
  fit.final=fit=lm(Price ~ . ,
                   data=ld_train)
  
  fit.final=step(fit.final)
  
  summary(fit.final)
  plot(x = ld_train2$Price,y = val.pred)
  points(ld_train2$Price,ld_train2$Price,"l",col='blue')
  test.pred=predict(fit.final,newdata=ld_test)
  
  write.csv(test.pred,"Mrunall_sawant_p1_part2.csv",row.names = F)
  