library(tidyverse)
library(readxl)
library(writexl)
library(mlr3verse)
library(mlr3mbo)
library(bbotk)
library(mlr3data)
library(mlr3learners)
library(mlr3viz)
library(mlr3tuning)
library(mlr3tuningspaces)
library(mlr3pipelines)
library(mlr3fselect)
library(paradox)
library(metrica)
library(mlr3spatiotempcv)
library(sf)
library(terra)
library(sperrorest)
####Data prepare
data_yield<-read_excel("data_yield.xlsx")#data_yield换为data_yield
data_yield1<-data_yield%>%
  select(-c(Year,Code_County,Code_Perfecture,Code_Province))%>%
  mutate(across(2:89,scale))
task_yield<-as_task_regr(data_yield1,target = "Wheat_yield")
task_yield$set_col_roles("N", roles = "name")
####feature subsetting
###cauculate filter value
F_filter<-function(a){
  a1<-as_task_regr(a,target = "Wheat_yield")
  a1$set_col_roles("N", roles = "name")
  #MIC
  mic<-tibble(feature=colnames(a)[1:(ncol(a)-1)])%>%
    mutate(score=map(feature,~MIC(obs=a[[colnames(a)[ncol(a)]]],pred=a[[.x]])))%>%
    filter(feature!="N")%>%
    unnest(score)
  #information gain
  flt_ig<-flt("information_gain")
  flt_ig$calculate(a1)
  ig<-as.data.table(flt_ig)
  #combine results
  a2<-bind_rows(mic,ig)%>%
    mutate(class=rep(c("mic","ig"),each=(ncol(a)-2)))
  return(a2)
}
feat_yield<-data_yield1%>%
  F_filter()%>%
  group_by(class)%>%
  arrange(desc(score),.by_group = TRUE)%>%
  ungroup()%>%
  mutate(n=rep(1:88,2))
write_xlsx(feat_yield,"feat.xlsx")
###Screening programs
##way1:filter only
#25%(keeping features in the top 25% under both rankings)1-22
#50%(keeping features in the top 50% under both rankings)1-44
#75%(keeping features in the top 75% under both rankings)1-66
feat_yield1<-feat_yield%>%
  group_nest(class)%>%
  mutate(ranks=map(class,~return(c(1:3)*22)))%>%
  unnest(ranks)%>%
  mutate(data=map2(data,ranks,~return(filter(.x,n<=.y))))%>%
  unnest(data)%>%
  select(class,feature,ranks)%>%
  group_nest(ranks)%>%
  mutate(data=map(data,~return(intersect(filter(.,class=="mic")[["feature"]],filter(.,class=="ig")[["feature"]]))))%>%
  unnest(data)%>%
  rename(class=ranks,feature=data)
feat_yield1<-feat_yield1%>%
  mutate(class=case_when(class==22~"f1",
                         class==44~"f2",
                         TRUE~"f3"))
write_xlsx(feat_yield1,"way1.xlsx")
##way2:Wrapper only
  set.seed(1)
instance1 = fsi(
  task = task_yield,
  learner = lrn("regr.ranger"),
  resampling = rsmp("cv", folds=4),
  measure = msr("regr.rsq"),
  terminator = trm("evals", n_evals=500))
instance2 = fsi(
  task = task_yield,
  learner = lrn("regr.svm"),
  resampling = rsmp("cv", folds=4),
  measure = msr("regr.rsq"),
  terminator = trm("evals", n_evals=500))
fselector=fs("random_search")
fselector$optimize(instance1)
fselector$optimize(instance2)
feat_yield2<-tibble(class="w",feature=intersect(instance1$result_feature_set,instance2$result_feature_set))
write_xlsx(feat_yield2,"way2.xlsx")
##way3:filter+Wrapper
fea1<-filter(feat_yield1,class=="f2")
data_yield2<-data_yield1%>%
    pivot_longer(2:89,names_to = "class",values_to = "value")%>%
    filter(class%in%fea1[[2]])%>%
    pivot_wider(names_from = "class",values_from = "value")
task_yield1<-as_task_regr(data_yield2,target = "Wheat_yield")
task_yield1$set_col_roles("N", roles = "name")
set.seed(1)
instance3 = fsi(
  task = task_yield1,
  learner = lrn("regr.ranger"),
  resampling = rsmp("cv", folds=4),
  measure = msr("regr.rsq"),
  terminator = trm("evals", n_evals=500))
instance4 = fsi(
  task = task_yield1,
  learner = lrn("regr.svm"),
  resampling = rsmp("cv", folds=4),
  measure = msr("regr.rsq"),
  terminator = trm("evals", n_evals=500))
fselector=fs("random_search")
fselector$optimize(instance3)
fselector$optimize(instance4)
feat_yield3<-tibble(class="fw",feature=intersect(instance3$result_feature_set,instance4$result_feature_set))
write_xlsx(feat_yield3,"way3.xlsx")
##way4:manual selection
#Pass prec, NDVI, wind, silt/clay/sand, srad, lrad
feat_yield4<-feat_yield%>%
  filter(class=="ig")%>%
  select(feature)%>%
  filter(!(str_detect(feature,"prec")|str_detect(feature,"NDVI")|str_detect(feature,"wind")|str_detect(feature,"silt")|str_detect(feature,"clay")|str_detect(feature,"sand")))%>%
  filter(!(str_detect(feature,"srad")|str_detect(feature,"lrad")))%>%
  filter(!(str_detect(feature,"Oct")|str_detect(feature,"Nov")|str_detect(feature,"Feb")|str_detect(feature,"Jan")))
write_xlsx(feat_yield4,"way4.xlsx")  
####Grouping method
###SC
#Calculating the geographic center of gravity
file_wheat<-str_c("CHN_Wheat_",2008:2018,".tif")
china<-read_sf("china_county.gpkg")%>%
  select(-c(4:6))
F_geo_center<-function(a,b){
  a<-china%>%
    filter(Code_County==a)
  b<-rast(file_wheat[str_detect(file_wheat,as.character(b))])
  b<-crop(b,a)
  b<-as.polygons(b)%>%
    st_as_sf()%>%
    st_centroid()%>%
    select(-1)%>%
    st_transform(crs="EPSG:4326")
  return(b)
}
data_yield_po<-data_yield%>%
  mutate(data=map2(Code_County,Year,F_geo_center))%>%
  unnest(data)%>%
  st_as_sf(crs="EPSG:4326")
F_loc_text<-function(a){
  a<-st_as_text(a)
  a<-str_sub(a,8,(str_length(a)-1))
  return(a)
}
data_yield_po<-data_yield_po%>%
  mutate(class=map(geometry,F_loc_text))%>%
  unnest(class)%>%
  separate(class,into=c("x","y"),sep=" ")%>%
  mutate(across(c(x,y),as.numeric))
F_loc_resample<-function(a,b){
  a1<-a%>%
    st_drop_geometry()
  b<-partition_kmeans(a1, coords = c("x", "y"),nfold=4, repetition=1,seed1=13-3*b,return_factor=TRUE)
  a1<-tibble(class=b[[1]])
  return(a1)
}
data_loc_resample<-tibble(n=1:4)%>%
  mutate(data=map(n,~return(data_yield_po)))%>%
  mutate(data=map2(data,n,F_loc_resample))%>%
  unnest(data)
write_xlsx(data_loc_resample,"space_resample.xlsx")
###TC
F_per<-function(a,b){
  set.seed(a)
  return(sample(b[[1]],4, replace=FALSE))
}
F_div<-function(a){
  a<-a%>%
    mutate(class=1:4)%>%
    mutate(data=map(data,~return(1:.x)))%>%
    unnest(data)%>%
    mutate(Year=2008:2018)%>%
    select(class,Year)
  return(a)
}
ti_div<-tibble(x=c(3,3,3,2,3,2,4,2,2,2,5,2),Class=rep(1:3,each=4))%>%
  group_nest(Class)%>%
  mutate(n=map(Class,~return(1:100)))%>%
  unnest(n)%>%
  mutate(data=map2(n,data,F_per))%>%
  distinct(data)
set.seed(1)  
n<-sample(1:20,20,replace=FALSE)
ti_div<-ti_div%>%
  mutate(n=n)%>%
  arrange(n)%>%
  slice(1:4)%>%
  unnest(data)%>%
  group_nest(n)%>%
  mutate(data=map(data,F_div))%>%
  unnest(data)
write_xlsx(ti_div,"time_resample.xlsx")

#####Evaluating strategy trade-offs#####
####Measures comparison
#1、Pearson correlation coefficient (r2)
MeasureRegrPearson= R6::R6Class("MeasureRegrThresholdAcc",
                                      inherit = mlr3::MeasureRegr, 
                                      public = list(
                                        initialize = function() { 
                                          super$initialize(
                                            id = "pearson_cor", 
                                            packages = character(), 
                                            properties = character(), 
                                            predict_type = "response",
                                            range = c(0, 1), 
                                            minimize = FALSE 
                                          )
                                        }
                                      ),
                                      private = list(
                                          .score = function(prediction, ...) {
                                            Pearson_cor=function(truth, response) {
                                             (cor(truth,response,method="pearson"))^2
                                            }
                                            Pearson_cor(prediction$truth, prediction$response)
                                        }
                                      )
)
mlr3::mlr_measures$add("regr.pearson_cor", MeasureRegrPearson)
#2、Coefficient of determination (R2) msr("regr.rsq")
#3、Mean absolute percentage error (MAPE) msr("regr.mape")
#4、Root mean square error (RMSE) msr("regr.rmse")
#5、Mean absolute error (MAE) msr("regr.mae")
F_co_m<-function(a,b,c,d){#a: learner; b: measure; c: random seed; d: parameter/performance 
  if(a=="regr.svm"){
    lrn<-lrn("regr.svm",
             type  = "eps-regression",
             kernel = "radial",
             cost  = to_tune(1e-4, 1e4,logscale = TRUE))
  }else{
    lrn<-lrn("regr.ranger",
             num.trees=to_tune(1, 2000))
  }
    data1<-read_excel("space_resample.xlsx")%>%
      filter(n==c)%>%
      select(-n)
    data<-data_yield1%>%
      bind_cols(data1)
  task<-as_task_regr(data,target = "Wheat_yield")
  task$set_col_roles("N", roles = "name")
  task$set_col_roles("class", roles = "group")
  r<-rsmp("loo")
  c<-5-c
  set.seed(c)
  b1=tune(tuner= tnr("grid_search", resolution=50),
          task = task,
          learner = lrn,
          resampling =r,
          measures = msr(b),
          store_models =TRUE)
  b2<-as.data.table(b1$archive,
                    measures=msrs(setdiff(c("regr.rsq","regr.mape","regr.rmse","regr.mae","regr.pearson_cor"),b)))
  if(a=="regr.svm"){
    if(d==1){
      return(tibble(cost=exp(b1$result_learner_param_vals[[3]])))
    }
    else{
        b2<-as_tibble(b2)%>%
      mutate(cost=exp(cost))%>%
      filter(cost==b1$result_learner_param_vals[[3]])%>%
      select(2:6)
      return(b2)  
    }
  }else {
    if(d==1){
      return(tibble(tree_num=b1$result_learner_param_vals[[2]]))
    }
    else{
    b2<-filter(b2, num.trees==b1$result_learner_param_vals[[2]])%>%
      select(2:6)
      return(b2)  
    }

  }
}
M_compare<-tibble(learner=c("regr.ranger","regr.svm"))%>%
  mutate(measure=map(learner,~return(c("regr.rsq","regr.mape","regr.rmse","regr.mae","regr.pearson_cor"))))%>%
  unnest(measure)%>%
  mutate(n=map(measure,~return(1:2)))%>%
  unnest(n)%>%
  arrange(learner)
M_compare<-M_compare%>%
  mutate(data1=pmap(list(learner,measure,n),~F_co_m(...,1)),
         data2=pmap(list(learner,measure,n),~F_co_m(...,2)))
M_compare1<-M_compare%>%
  select(-data2)%>%
  unnest(data1)
M_compare2<-M_compare%>%
  select(-data1)%>%
  unnest(data2)
write_xlsx(M_compare1,"M1.xlsx")
write_xlsx(M_compare2,"M2.xlsx")
####resample strategy comparison
F_co_r<-function(a,b,c,d){#a: learner; b: resample strategy; c: random seed; d: nested or not
  if(a=="regr.svm"){
    lrn<-lrn("regr.svm",
             type  = "eps-regression",
             kernel = "radial",
             cost  = to_tune(1e-4, 1e4,logscale = TRUE),
             gamma = to_tune(1e-4, 1e4,logscale = TRUE))
    lrn0<-lrn("regr.svm")
  }else {
    lrn<-lrn("regr.ranger",
        replace=TRUE,
        mtry.ratio=to_tune(0, 1),
        sample.fraction=to_tune(0.1, 1))
    lrn0<-lrn("regr.ranger")
  }
  if(b=="kfold"){
    task<-task_yield$clone()
    r1<-rsmp("cv",folds=4)
    r2<-rsmp("cv",folds=4)
  }else if(b=="time"){
    data1<-read_excel("time_resample.xlsx")%>%
      filter(n==c)%>%
      select(-n)
    data2<-data_yield1%>%
      mutate(Year=data_yield[[1]])%>%
      left_join(data1)%>%
      select(-Year)
    task<-as_task_regr(data2,target = "Wheat_yield")
    task$set_col_roles("N", roles = "name")
    task$set_col_roles("class", roles = "group")
    r1<-rsmp("loo")
    r2<-rsmp("loo")
  }else{
    data1<-read_excel("space_resample.xlsx")%>%
      filter(n==c)%>%
      select(-n)
    data2<-data_yield1%>%
      bind_cols(data1)
    task<-as_task_regr(data2,target = "Wheat_yield")
    task$set_col_roles("N", roles = "name")
    task$set_col_roles("class", roles = "group")
    r1<-rsmp("loo")
    r2<-rsmp("loo")
  }
  if(d==0){
    set.seed(c)
    tu_result=tune(tuner= tnr("grid_search", resolution =6),
                   task = task,
                   learner = lrn,
                   resampling = r2,
                   measures = msr("regr.rsq"),
                   store_models =TRUE,
                   terminator=trm("evals", n_evals=50))
    lrn0$param_set$values=tu_result$result_learner_param_vals
    set.seed(c+1)
    result<-mlr3::resample(task, lrn0, r1, store_models = TRUE)
  }else{
    set.seed(c)
    lrn<-auto_tuner(tuner=tnr("grid_search", resolution=6),
                    learner=lrn,
                    resampling=r2,
                    measure=msr("regr.rsq"),
                    terminator=trm("evals", n_evals=50))
    result<-mlr3::resample(task, lrn, r1, store_models = TRUE)
  }
  result<-as.data.table(result)
  result<-tibble(n=1:nrow(result))%>%
    mutate(N=map(n,~return(result$prediction[[.x]]$row_ids)),
           Truth=map(n,~return(result$prediction[[.x]]$truth)),
           Response=map(n,~return(result$prediction[[.x]]$response)))%>%
    select(-n)%>%
    unnest(N,Truth,Response)
  return(result)
}
 R_compare<-tibble(res=rep(c("kfold","time","space"),each=2),nest=rep(c(0,1),3))%>%
   mutate(n=map(res,~return(1:4)))%>%
   unnest(n)%>%
   mutate(learner=map(n,~return(c("regr.svm","regr.ranger"))))%>%
   unnest(learner)%>%
   arrange(nest,learner)
 R_compare<-R_compare%>%
   mutate(data=pmap(list(learner,res,n,nest),F_co_r))%>%
   unnest(data)
 write_xlsx(R_compare,"R.xlsx")
###data distribution
data2_3<-data_yield%>%
  select(SWC_May_0_60cm,EVI_Feb,SIF_Apr,Wheat_yield,Year)
task2_3<-as_task_regr(data2_3,target = "Wheat_yield")
set.seed(1)
c1<-rsmp("cv", folds = 4)
c1$instantiate(task2_3)
c1<-tibble(class=1:4)%>%
  mutate(n=map(class,~return(c1$test_set(.x))))%>%
  unnest(n)%>%
  arrange(n)%>%
  select(-n)%>%
  bind_cols(data2_3)%>%
  group_nest()
c2<-read_excel("space_resample.xlsx")%>%
  filter(n==1)%>%
  select(-n)%>%
  mutate(class=as.numeric(class))%>%
  bind_cols(data2_3)%>%
  group_nest()
c3<-read_excel("time_resample.xlsx")%>%
  filter(n==1)%>%
  select(-n)%>%
  right_join(data2_3)%>%
  group_nest()
data_c<-bind_rows(c1,c2,c3)%>%
  mutate(Class=c("CR","SC","TC"))%>%
  unnest(data)%>%
  select(-Year)
write_xlsx(data_c,"d.xlsx")

#####Optimize modeling results#####
####Feature downgrading scheme
 feat_yield1<-read_excel("way1.xlsx")
 feat_yield2<-read_excel("way2.xlsx")
 feat_yield3<-read_excel("way3.xlsx")
 feat_yield4<-read_excel("way4.xlsx")
 feat_yield5<-tibble(class="A",feature=colnames(data_yield1)[2:89])
 feature<-bind_rows(feat_yield1,feat_yield2,feat_yield3,feat_yield4,feat_yield5)
 ####OPTI
 F_opti<-function(a,b,c,d,e,f,g){#a: learner; b: feature set; c: Whether or not to tune; d: tuning algorithm; e: resampling strategy; f: random seed; g: tuning frequency
   if(a=="regr.glmnet"){
     lrn<-lts(lrn("regr.glmnet"))#2
     lrn0<-lrn("regr.glmnet")
     n1=81
     n2=16
   }
   else if(a=="regr.kknn"){
     lrn<-lrn("regr.kknn",#2
              kernel="optimal",
              k=to_tune(1, 50,logscale = TRUE),
              distance=to_tune(1, 5))
     lrn0<-lrn("regr.kknn")
     n1=81
     n2=16
   }    
   else if(a=="regr.svm"){  
     lrn<-lrn("regr.svm",#2 
              type  = "eps-regression",
              kernel = "radial",
              cost  = to_tune(1e-4, 1e4,logscale = TRUE),
              gamma = to_tune(1e-4, 1e4,logscale = TRUE))
     lrn0<-lrn("regr.svm")
     n1=81
     n2=16
   }
   else if(a=="regr.ranger"){
     if(d=="cmaes"){
       lrn<-lrn("regr.ranger",
                replace=TRUE,
                mtry.ratio=to_tune(0, 1),
                sample.fraction=to_tune(0.1, 1))
     }
     else{
       lrn<-lrn("regr.ranger",#3 
                replace=TRUE,
                num.trees=to_tune(1, 2000),
                mtry.ratio=to_tune(0, 1),
                sample.fraction=to_tune(0.1, 1))
     }
     lrn0<-lrn("regr.ranger")
     n1=19
     n2=6
   }
   else {
     if(d=="cmaes"){
       lrn<-lrn("regr.xgboost",
                eta=to_tune(1e-4, 1,logscale = TRUE),
                colsample_bytree=to_tune(0.1, 1),
                colsample_bylevel=to_tune(0.1, 1),
                lambda=to_tune(0.001, 1000,logscale = TRUE),
                alpha=to_tune(0.001, 1000,logscale = TRUE),
                subsample=to_tune(0.1,1))
     }
     else{
       lrn<-lts(lrn("regr.xgboost"))#8
     }  
     lrn0<-lrn("regr.xgboost")
     n1=3
     n2=2
   }
   if(g==500){
     n=n1
   }
   else{
     n=n2
   }
   fea<-filter(feature,class==b)
   data<-data_yield1%>%
     pivot_longer(2:89,names_to = "class",values_to = "value")%>%
     filter(class%in%fea[[2]])%>%
     pivot_wider(names_from = "class",values_from = "value")
   if(e=="space"){
     data1<-read_excel("space_resample.xlsx")%>%
       filter(n==f)%>%
       select(-n)
     data<-data%>%
       bind_cols(data1)
   }
   else{
     data1<-read_excel("time_resample.xlsx")%>%
       filter(n==f)%>%
       select(-n)
     data<-data%>%
       mutate(Year=data_yield[[1]])%>%
       left_join(data1)%>%
       select(-Year)
   }
   task<-as_task_regr(data,target = "Wheat_yield")
   task$set_col_roles("N", roles = "name")
   task$set_col_roles("class", roles = "group")
   r<-rsmp("loo")
   if(e=="space"){
     f<-5-f
   }
   else{
     f<-f
   }
   if(c==0){
     set.seed(f)
     result<-mlr3::resample(task, lrn0, r, store_models = TRUE)
   }
   else if(d%in%c("grid_search","random_search")){
     if(d=="grid_search"){
       tuner<-tnr(d, resolution=n)
     }
     else{
       tuner<-tnr(d)
     }  
     set.seed(f)
     lrn<-auto_tuner(tuner=tuner,
                     learner=lrn,
                     resampling=r,
                     measure=msr("regr.rsq"),
                     terminator=trm("evals", n_evals=g))
     result<-mlr3::resample(task, lrn, r, store_models = TRUE)  
   }
   else{
     if(d=="cmaes"){
       tuner<-tnr("cmaes")
     }
     else{
       bayesopt_ego=mlr_loop_functions$get("bayesopt_ego")
       surrogate=srlrn(lrn("regr.km", covtype = "matern5_2",
                           optim.method = "BFGS", control = list(trace = FALSE)))
       acq_function=acqf("ei")
       if(g==500){
         i=100
       }
       else{
         i=10 
       }
       acq_optimizer=acqo(opt("nloptr", algorithm = "NLOPT_GN_ORIG_DIRECT"),
                          terminator = trm("stagnation", iters = i, threshold = 1e-5))
       tuner=tnr("mbo",
                 loop_function=bayesopt_ego,
                 surrogate=surrogate,
                 acq_function=acq_function,
                 acq_optimizer=acq_optimizer)
     }
     set.seed(f)
     lrn<-auto_tuner(tuner=tuner,
                     learner=lrn,
                     resampling=r,
                     measure=msr("regr.rsq"),
                     terminator=trm("evals", n_evals=g))
     result<-mlr3::resample(task, lrn, r, store_models = TRUE)  
   }
   result<-as.data.table(result)%>%
     mutate(Time=map2(learner,prediction,~return(.y$score(msr("time_train"),learner =.x))))%>%
     unnest(Time)
   result<-tibble(n=1:nrow(result))%>%
     mutate(N=map(n,~return(result$prediction[[.x]]$row_ids)),
            Truth=map(n,~return(result$prediction[[.x]]$truth)),
            Response=map(n,~return(result$prediction[[.x]]$response)),
            Time=result[["Time"]])%>%
     select(-n)%>%
     unnest(N,Truth,Response)
   return(result)
 }
 Re_opti<-tibble(learner=str_c("regr.",c("glmnet","kknn","svm","ranger","xgboost")))%>%
   mutate(fea=map(learner,~return(unique(feature[["class"]]))))%>%
   unnest(fea)%>%
   mutate(data=map(fea,~return(tibble(opti=c(0,1,1,1,1),method=c(0,"grid_search","random_search","mbo","cmaes")))))%>%
   unnest(data)%>%
   mutate(res=map(learner,~return(c("space","time"))))%>%
   unnest(res)%>%
   mutate(n=map(res,~return(1:4)))%>%
   unnest(n)%>%
   mutate(nevel=map(res,~return(c(50,500))))%>%
   unnest(nevel)%>%
   filter(!(opti==0&nevel==50))
 Re_opti<-Re_opti%>%
   mutate(data=pmap(list(learner,fea,opti,method,res,n,nevel),F_opti))%>%
   mutate(data=map(data,~return(select(.,2:3))))
 walk2(Re_opti[["data"]],str_c(Re_opti[["learner"]],"_",Re_opti[["fea"]],"_",Re_opti[["opti"]],"_",Re_opti[["method"]],"_",Re_opti[["res"]],"_",Re_opti[["n"]],"_",Re_opti[["nevel"]],".xlsx"),~write_xlsx(.x,.y))

#####Model interpretability#####
 ###the best model
 feat_yield<-feature%>%
   filter(class=="FM")
 data<-data_yield1%>%
   pivot_longer(2:89,names_to = "class",values_to = "value")%>%
   filter(class%in%feat_yield[["feature"]])%>%
   pivot_wider(names_from = "class",values_from = "value")
 data1<-read_excel("space_resample.xlsx")%>%
   filter(n==4)%>%
   select(-n)
 data<-data%>%
   bind_cols(data1)
 lrn<-lrn("regr.ranger",
          replace=TRUE,
          mtry.ratio=to_tune(0, 1),
          sample.fraction=to_tune(0.1, 1))
 task<-as_task_regr(data,target = "Wheat_yield")
 task$set_col_roles("N", roles = "name")
 task$set_col_roles("class", roles = "group")
 r<-rsmp("loo")
 set.seed(1)
 tuner<-tnr("cmaes")
 opti<-ti(task=task,
          learner=lrn,
          resampling=r,
          measure=msr("regr.rsq"),
          terminator=trm("evals", n_evals=500))
 tuner$optimize(opti)
 lrn_opti<-lrn("regr.ranger")
 set.seed(1)
 lrn_opti$param_set$values<- opti$result_learner_param_vals
 lrn_opti$train(task)
 lrn_opti$predict(task)$score(msrs(c("regr.rsq","regr.rmse","regr.mae")))
 response<-lrn_opti$model$predictions
 truth<-data[["Wheat_yield"]]
 predict_data<-tibble(x=response,y=truth)
###feature dependency
feat_yield<-feature%>%
  filter(class=="FM")
F_vif<-function(a){
  library(car)
  a<-data_yield1%>%
    pivot_longer(2:89,names_to = "class",values_to = "value")%>%
    filter(class%in%a)%>%
    pivot_wider(names_from = "class",values_from = "value")%>%
    select(-N)
  b<-vif(lm(Wheat_yield~.,data=a))
  b<-tibble(fea=labels(b))%>%
    mutate(vif=b)
  return(b)
}
F_pair<-function(a){
  a<-tibble(fea1=a[1:(length(a)-1)])%>%
    mutate(fea2=(1:n())+1)%>%
    mutate(fea2=map(fea2,~return(a[.x:length(a)])))%>%
    unnest(fea2)
  return(a)
}
fea1<-feat_yield[["feature"]][c(1:3,8,17)]
fea2<-feat_yield[["feature"]][setdiff(1:25,c(1:3,8,17))][c(2,3,1,4,10,11,9,12,14,15,13,16,18,19,17,20,7,6,5,8)]
fea_data2<-F_pair(c(fea2,fea1))%>%
    mutate(cor=map2(fea1,fea2,~return(cor(data_yield1[[.x]],data_yield1[[.y]]))))%>%
    unnest(cor)%>%
    arrange(desc(cor))
write_xlsx(fea_data2,"cor.xlsx")
fea_data3_1<-F_vif(feat_yield[["feature"]])
fea_data3_2<-tibble(month=c("Dec","Mar","Apr","May"))%>%
  mutate(data=map(month,~return(c(fea1,fea2[str_detect(fea2,.x)]))))%>%
  mutate(data=map(data,F_vif))%>%
  unnest(data)
write_xlsx(fea_data3_1,"vif1.xlsx")
write_xlsx(fea_data3_2,"vif2.xlsx")
###imp
##shap
library(kernelshap) 
set.seed(10)
X<-data[sample(nrow(data), 6078), feat_yield[["feature"]]]
bg_X<-data[sample(nrow(data), 6078), ]
shap<-kernelshap(lrn_opti,X = X, bg_X = bg_X, feature_names =feat_yield[["feature"]])  
a1<-shap$X%>%
  as_tibble()%>%
  mutate(n=1:n())%>%
  pivot_longer(1:25,names_to = "class",values_to = "value")
a2<-shap$S%>%
  as_tibble()%>%
  mutate(n=1:n())%>%
  pivot_longer(1:25,names_to = "class",values_to = "value")
write_xlsx(a1,"kernel_fea.xlsx")
write_xlsx(a2,"kernel_shap.xlsx")
##M-plot
library(DALEX)
library(DALEXtra)
lrn_opti1<-lrn("regr.ranger",
              replace=TRUE,
              mtry.ratio=0.5088434,
              sample.fraction=0.9115698)
data1<-data_yield%>%
  select(-c(Year,Code_County,Code_Perfecture,Code_Province))%>%
  pivot_longer(2:89,names_to = "class",values_to = "value")%>%
  filter(class%in%feat_yield[["feature"]])%>%
  pivot_wider(names_from = "class",values_from = "value")%>%
  select(-N)
task1<-as_task_regr(data1,target = "Wheat_yield")
set.seed(1)
lrn_opti1$train(task1)
explainer_rf<-DALEX::explain(model = lrn_opti1,  
                               data = select(data1,-Wheat_yield),
                               y = data1[["Wheat_yield"]])
F_pdp<-function(a){
pdp_rf<-model_profile(explainer = explainer_rf, variables = a,type="conditional")
return(pdp_rf$agr_profiles)
}
pdp<-feat_yield%>%
  mutate(data=map(feature,F_pdp))
walk2(pdp[["data"]],str_c(pdp[["class"]],"_",pdp[["feature"]],".xlsx"),~write_xlsx(.x,.y))