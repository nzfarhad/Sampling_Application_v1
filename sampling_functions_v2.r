require(plyr)
require(dplyr)
require(car)
require(reshape2)
require(stringr)
library(shiny)
library(shinyjs)
library(crayon)

Ssize<-function (x,A,p,E) {(qchisq(A,1)*x*p*(1-p)) / (E^2*(x-1)+qchisq(A,df=1)*p*(1-p))}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# create the smapling frame
prepare_sampling_target<-function(cens,input){
plyr::ddply(cens,"strata_id", summarise, 
            Population = max(SumDist,na.rm=T),
            target = ceiling(Ssize(Population,input$conf_level,input$proportion,input$e_marg)),
            target.with.buffer = ceiling(target * (1+input$buf))
)	%>% as.data.frame}

# create the smapling frame
create_sampling_frame<-function(cens,input){
  cens$id_sampl<-paste0("id_",row.names(cens))
  if(input$stratified=="Stratified"){
    cens$strata_id<-cens[[as.character(input$strata)]]
  } else {
    cens$strata_id<-rep("all",nrow(cens))
  }
  
  if(input$samp_type=="Cluster sampling"){
    cens$psu_id<-cens[[as.character(input$col_psu)]]
    cens$pop_numbers<-cens[[as.character(input$colpop)]]
  }else if(input$samp_type=="2 stages random - st1"){
    cens$psu_id<-cens$id_sampl
    cens$pop_numbers<-cens[[as.character(input$colpop)]]
  }else{
    cens$psu_id<-cens$id_sampl
    cens$pop_numbers<-rep(1,nrow(cens))
  }
  
  sumdist<-plyr::ddply(cens, .(strata_id), summarise, SumDist = sum(pop_numbers,na.rm=T))
  cens<-merge(cens,sumdist,by="strata_id")
  proba<-as.numeric(cens$pop_numbers)/as.numeric(cens$SumDist)
  cens<-cbind(cens,proba)
  cens<-cens[!is.na(cens$proba),]
  
  if(input$samp_type=="Cluster sampling"){
    cens$psu_id<-factor(cens$psu_id)
  }
  return(cens) 
}


clustersample<-function(cens,sam,cls,buf,ICC,sw_rand=c()) {
  
  target<-as.numeric(as.character(sam[["target"]]))
  dist<-as.character(sam[["strata_id"]])
  out<-something(cens,cls,buf,ICC,dist,target,mode="forced")
  if(is.null(out)){
    dbr<-cens[as.character(cens$strata_id)==dist,]
    out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam[["target"]])*(1+buf+0.1)),prob=dbr$proba,replace=TRUE)
    sw_rand<-c(sw_rand,dist)
  }
  return(list(output=out,sw_rand=sw_rand))
}


randomsample<-function(cens,sam,buf){
  dist<-as.character(sam[["strata_id"]])
  dbr<-cens[as.character(cens$strata_id)==dist,]
  tosample<-as.numeric(sam[["target.with.buffer"]])
  pop<-as.numeric(sam[["Population"]])
  if(tosample>pop){target<-pop}else{target<-tosample}
  out<-sample(as.character(dbr$id_sampl),target,replace=FALSE)
  incProgress(round(1/nrow(sam),2), detail = paste("Sampling", dist))
  return(out)
}

stage2rdsample<-function(cens,sam,buf){
  dist<-as.character(sam[["strata_id"]])
  dbr<-cens[as.character(cens$strata_id)==dist,]  
  tosample<-as.numeric(sam[["target.with.buffer"]])
  pop<-as.numeric(sam[["Population"]])
  if(tosample>pop){target<-pop}else{target<-tosample}
  out<-sample(as.character(dbr$id_sampl),target,prob=dbr$proba,replace=TRUE)
  # incProgress(round(1/nrow(sam),2), detail = paste("Sampling", dist))
  return(out)
}
# stage2rdsample<-function(cens,sam,buf){
#   dist<-as.character(sam[["strata_id"]])
#   dbr<-cens[as.character(cens$strata_id)==dist,]  
#   tosample<-as.numeric(sam[["target.with.buffer"]])
#   pop<-as.numeric(sam[["Population"]])
#   if(tosample>pop){target<-pop}else{target<-tosample}
#   out<-sample(as.character(dbr$id_sampl),target,prob=dbr$proba,replace=TRUE)
#   incProgress(round(1/nrow(sam),2), detail = paste("Sampling", dist))
#   return(out)
# }


something<-function(cens,cls,buf,ICC,dist,target,mode="notforced"){
  dbr<-cens[as.character(cens$strata_id)==dist,]
  dbr<-dbr[dbr$pop_numbers>=cls,]
  out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(target*(1+buf))/cls),prob=dbr$proba,replace=TRUE)
  
  stop<-F
  
  while(stop==F){
    d<-as.data.frame(table(out))[,2]
    #ms<-sum(d)/nrow(as.data.frame(d))
    ms<-1
    DESS<-1+(ms*cls-1)*ICC
    targ<-DESS*(target*(1+buf))/cls		
    
    if(sum(d)>=targ){
      # message(green(paste0(dist," : yeah")))
      stop<-T
      return(out)
      
    } else if ((mode == "forced" & cls==1 & DESS > 3 )){
      # message(red(paste0(dist," : exited because of DESS > 3")))
      stop<-T
      return(out)
      
    } else {
      out<-c(out,sample(as.character(dbr$id_sampl),1,prob=dbr$proba,replace=TRUE))
      rd_check<-all(unique(dbr$id_sampl)%in%unique(out))
      
      if(rd_check & mode == "notforced" ){
        # message(paste0(dist," : reduced cluster size to 1"))
        out<-NULL
        stop<-T
        return(out)
      } 
    }
  }
}


simulate_sample<-function(input_main,sampling_frame,titl="sampling"){
  
  for (j in 1:nrow(input_main)){
    
    params<-input_main[j,]
    frame<-prepare_sampling_frame(sampling_frame,ch(params$popu),ch(params$stratat),params$samp_type)
    
    sam<-prepare_sampling_target(frame,params)
    sasampl<-sssampling(sam,frame,params,titl)
    
    params$total_surveys<-sum(sasampl[[2]][["# surveys (buffer)"]],na.rm=T)
    params$total_unit<-sum(sasampl[[2]][["# units to assess"]],na.rm=T)
    params$group<-titl
    params$number_disaggregation<-nrow(sasampl[[2]])
    
    if(j==1){myresults<-params}else{myresults<-rbind(myresults,params)}
    
  }
  return(myresults)
  
}


sssampling<-function(sam,frame,input){
 
  cens<-frame
  sam<-sam
  sw_rand<-c()
  output<-c()
  
  if(input$samp_type=="Cluster sampling"){
      if(input$topup=="Enter sample size"){
        clsamapling<-apply(sam,1,clustersample,cens=cens,cls=input$cls,buf=input$buf,ICC=0) 
        output<-lapply(clsamapling,function(x) x$output) %>% unlist %>% c
        sw_rand<-lapply(clsamapling,function(x) x$sw_rand) %>% unlist %>% c
      }else {
        clsamapling<-apply(sam,1,clustersample,cens=cens,cls=input$cls,buf=input$buf,ICC=input$ICC) 
        output<-lapply(clsamapling,function(x) x$output) %>% unlist %>% c
        sw_rand<-lapply(clsamapling,function(x) x$sw_rand) %>% unlist %>% c
      }
  

  } else if (input$samp_type=="2 stages random - st1"){
       output<-apply(sam,1,stage2rdsample,cens=cens,buf=input$buf) %>% unlist 

  } else if (input$samp_type=="Simple random"){
      output<-apply(sam,1,randomsample,cens=cens,buf=input$buf) %>% unlist 
  
  }
  
  output<-as.data.frame(table(output))
  dbout<-merge(output,cens,by.x="output",by.y="id_sampl",all.x=T,all.y=F)
  
  if(input$samp_type=="Cluster sampling"){  
    dbout$Freq<-ifelse(dbout$strata_id%in%sw_rand,dbout$Freq,dbout$Freq*input$cls)
  }
  
  names(dbout)<-recode(names(dbout),"'output'='id_sampl';'Freq'='Survey'")
  dbout$survey_buffer<-dbout$Survey
  
  summary_sample<- plyr::ddply(dbout,"strata_id", summarise, 
                               Surveys = sum(Survey,na.rm=T), 
                               PSUs = length(strata_id), 
                               Cluster_size = round(Surveys/PSUs,2),
                               Cluster_size_init = input$cls,
                               ICC = input$ICC,
                               DESS = 1+(Cluster_size_init-1)*ICC,
                               Effective_sample = round(Surveys / DESS,0),
                               Surveys_buffer = input$buf,
                               Confidence_level = input$conf_level,
                               Error_margin = input$e_marg,
                               NB_Population = max(SumDist,na.rm=T),
                               Sampling_type = input$samp_type
  )
  
  if(input$samp_type=="Cluster sampling"){
    for(i in 1:nrow(summary_sample)){
      if(summary_sample$strata[i]%in%sw_rand){
        summary_sample$Surveys_buffer[i]<-summary_sample$Surveys_buffer[i]+.1
        summary_sample$Cluster_size[i]<-1
        summary_sample$DESS[i]<-1
        summary_sample$Effective_sample[i]<-summary_sample$Surveys[i]
        summary_sample$Sampling_type[i]<-"Cluster sampling with size 1 = random sampling"
      }
    }
  }
  
  if(input$samp_type!="Cluster sampling"){
    le<-nrow(summary_sample)
    summary_sample$Cluster_size<-rep(NA,le)
    summary_sample$Cluster_size_init <-rep(NA,le)
    summary_sample$ICC<-rep(NA,le)
    summary_sample$DESS<-rep(NA,le)
    summary_sample$Effective_sample<-rep(NA,le)
  }
  
  if(input$topup=="Enter sample size"){
    le<-nrow(summary_sample)
    summary_sample$Cluster_size<-rep(NA,le)
    summary_sample$Cluster_size_init <-rep(NA,le)
    summary_sample$ICC<-rep(NA,le)
    summary_sample$DESS<-rep(NA,le)
    summary_sample$Effective_sample<-rep(NA,le)
    summary_sample$Error_margin<-rep(NA,le)
    summary_sample$Confidence_level<-rep(NA,le)
  }
  
  tab<-paste0("Sampling_output",gsub(":","_",Sys.time()),".xlsx")
  
  names(summary_sample)<-c("Stratification","# surveys", "# units to assess","Cluster size","Cluster size set","ICC","DESS","Effective sample","% buffer","Confidence level","Error margin","Population","Sampling type")
  list(dbout,summary_sample,sw_rand)
}


sssampling_old<-function(sam,frame,input,titl){
  
  samp_type=input$samp_type
  ICC=input$ICC
  cls=input$cls
  buf=input$buf
  conf_level=input$conf_level
  e_marg=input$e_marg
  
  
  rd<-"no"
  sw_rand<-NA
  if(samp_type=="Cluster sampling"){
    
    sw_rand<-NA
    for (i in 1:nrow(sam)){
      rd<-"no"
      dist<-as.character(sam$strata_id[i])
      dbr<-frame[which(as.character(frame$strata_id)==dist),]
      dbr<-dbr[which(dbr$pop_numbers>=cls),]
      out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam$target[i]*(1+buf))/cls),prob=dbr$proba,replace=TRUE)
      repeat{
        d<-as.data.frame(table(out))[,2]
        #ms<-sum(d)/nrow(as.data.frame(d))
        ms=1
        DESS<-1+(ms*cls-1)*ICC
        targ<-DESS*(sam$target[i]*(1+buf))/cls		
        
        if(sum(d)>=targ){
          break
        }
        
        out<-c(out,sample(as.character(dbr$id_sampl),1,prob=dbr$proba,replace=TRUE))
        rd_check<-all(unique(dbr$id_sampl)%in%unique(out))	
        # if(rd_check){
        #   rd<-"yes"
        #   sw_rand<-c(sw_rand,dist)
        #   out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam$target[i]*(1+buf))),prob=dbr$proba,replace=TRUE)
        #   break
        # }
      }
      if (i==1){output<-out} else {output<-c(output,out)}
    }
    
    
  } else if (samp_type=="Simple random"){
    for (i in 1:nrow(sam)){
      dist<-as.character(sam$strata[i])
      dbr<-frame[as.character(frame$strata)==dist,]
      out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam$target[i])*(1+buf)),replace=FALSE)
      if (i==1){output<-out} else {output<-c(output,out)}
    }
    
  } else if (samp_type=="Simple random - weight"){
    for (i in 1:nrow(sam)){
      dist<-as.character(sam$strata[i])
      
      dbr<-frame[as.character(frame$strata)==dist,]
      
      buffed<-ceiling(as.numeric(sam$target[i])*(1+buf))
      
      if(buffed>sam$Population[i]){
        target_nb<-sam$Population[i]
      } else {
        target_nb<-buffed
      }
      
      
      out<-sample(as.character(dbr$id_sampl),target_nb,prob=dbr$proba,replace=FALSE)
      
      if (i==1){output<-out} else {output<-c(output,out)}
    }
    
  } else if (samp_type=="2 stages random - st1"){
    for (i in 1:nrow(sam)){
      dist<-as.character(sam$strata[i])
      dbr<-frame[as.character(frame$strata)==dist,]
      out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(sam$target[i])*(1+buf)),prob=dbr$proba,replace=TRUE)
      if (i==1){output<-out} else {output<-c(output,out)}
    }
    
  }
  
  output<-as.data.frame(table(output))
  
  dbout<-merge(output,frame,by.x="output",by.y="id_sampl",all.x=T,all.y=F)
  
  if(samp_type=="Cluster sampling"){  
    dbout$Freq<-ifelse(dbout$strata_id%in%sw_rand,dbout$Freq,dbout$Freq*cls)
  }
  
  names(dbout)<-car::recode(names(dbout),"'output'='id_sampl';'Freq'='Survey'")
  
  # if(samp_type=="Cluster sampling"){  
  # dbout$survey_buffer<-ceiling(dbout$Survey*(1+buf))
  # } else {
  dbout$survey_buffer<-dbout$Survey
  # }
  
  summary_sample<- plyr::ddply(dbout,"strata_id", summarise, 
                               Surveys = sum(Survey,na.rm=T), 
                               PSUs = length(strata_id), 
                               ICC = input$ICC,
                               Cluster_size = input$cls,
                               Cluster_size_no_gps = round(Surveys/PSUs,2),
                               DESS = 1+(Cluster_size-1)*ICC,
                               DESS_no_gps = 1+(Cluster_size_no_gps-1)*ICC,
                               Effective_sample = round(Surveys / DESS,0),
                               Effective_sample_np_gps = round(Surveys / DESS_no_gps,0),
                               Surveys_buffer = sum(survey_buffer,na.rm=T),
                               NB_Population = max(SumDist,na.rm=T)
  )
  
  
  summary_sample$Confidence_level = conf_level
  summary_sample$Error_margin = e_marg
  summary_sample$Sampling_type = samp_type
  
  if(samp_type=="Cluster sampling"){
    for(i in 1:nrow(summary_sample)){
      if(summary_sample$strata[i]%in%sw_rand){
        summary_sample$Cluster_size[i]<-NA
        summary_sample$ICC[i]<-NA
        summary_sample$DESS[i]<-NA
        summary_sample$Effective_sample[i]<-NA
        summary_sample$Sampling_type[i]<-"2 stages random - st1"
      }
    }
  }
  
  if(samp_type!="Cluster sampling"){
    le<-nrow(summary_sample)
    summary_sample$Cluster_size<-rep(NA,le)
    summary_sample$ICC<-rep(NA,le)
    summary_sample$DESS<-rep(NA,le)
    summary_sample$Effective_sample<-rep(NA,le)
  }
  
  tab<-paste0(titl,"_",samp_type,"_",conf_level,"_",e_marg,"_",cls,"_",gsub(":","_",Sys.time()),".csv")
  
  title_summary<-paste0("Summary_",tab)
  title_sampl<-paste0("Sample_",tab)
  

  write.csv(summary_sample,title_summary)
  
  write.csv(dbout,title_sampl)

  return(list(dbout,summary_sample))
  
}




