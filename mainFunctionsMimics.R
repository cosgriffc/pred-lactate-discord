createTimePoint<-function(dreg,timevar,uservar)
{
  dreg$dateTime<-dreg[,timevar]  
  dreg$user<-dreg[,uservar] 
  
  dreg<- dreg[with(dreg, order(user, dateTime)), ]
  
  dtp_long<-data.frame(matrix(ncol=3,nrow=0))
  colnames(dtp_long)<-c('user','dateTime','timepoint')
  
  for(sj in unique(dreg$user))
  {
    duser<-subset(dreg,user==sj)
    bas.user<-min(as.Date(duser$dateTime),na.rm = TRUE)
    top.user<-max(as.Date(duser$dateTime),na.rm = TRUE)
    
    dvec <-seq(as.Date(bas.user), as.Date(top.user), "days")
    
    tp<-data.frame(dateTime=sort(unique(dvec)),timepoint=seq(1,length(unique(dvec)),1))
    tp$user<-sj
    
    dtp_long<-rbind(dtp_long[,names(tp)],tp)
    
  }
  dtp_long$timepoint<-dtp_long$timepoint-1
  dtp_long$dateTime<-as.character(dtp_long$dateTime)
  
  colnames(dtp_long)[which(names(dtp_long)=='user')]<-uservar
  
  colnames(dtp_long)[which(colnames(dtp_long) %in% 'dateTime')]<-timevar
  
  return(dtp_long)
  
}

plotIndividualTrajectories_BP<-function(dgr,response='Blood Pressure',ylab="Blood pressure",auxt='',ncols,varPer)
{
  
  dgr<-subset(data_sepsis_icu_icd,subject_id %in% unique(data_sepsis_icu_icd$subject_id)[1:10])
  
  p<-ggplot(dgr, aes(timepoint_lactate, lactate)) + 
    geom_line(aes(group=subject_id,colour='lactate')) + 
    geom_point(aes(group=subject_id,colour='lactate'),size=2) + 
    
    facet_wrap(~ subject_id,ncol=3,scales = 'free')+
    scale_colour_manual(name="Line Color",values=c(SBP="#E69F00",DBP='#56B4E9'))+
    scale_shape_manual(values=c(8, 16, 17))+
    xlab("Days of follow-up") + 
    ylab("Blood Pressure") +       
    labs(title=paste('Blood Pressure Reading for the ',diagg, 'group','(',auxt,')',sep=' '))+
    theme_bw() +
    theme(plot.title = element_text(size = 14, family = "Helvetica", face = "bold"),
          text = element_text(size = 14, family = "Helvetica"),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 12),axis.text.y=element_text(size = 12))+
    theme(legend.position = "top")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    theme(strip.text.y = element_text(size = 10, colour = "black"))+
    theme(strip.text.x = element_text(size = 10, colour = "black"))
  
  file.pdf<-paste('images/',response,'_',gsub(" ", "", diagg, fixed = TRUE),'_',auxt,'.pdf',sep='')
  pdf(file.pdf,width=25,height=17) 
  print(p)
  dev.off()
  
}

covertComma2Period<-function(d)
{
  
  d<-as.character(d)
  d<-sapply(d, gsub, pattern = ",", replacement= ".")
  d<-as.numeric(d)
  return(d)
}

plotStatisticsWhole<-function(d,plot_title,source,sizeFont,groupname)
{
  # dobese<-subset(dvarPatient,bmi_who_obesity_c=='Obese')
  # cor(dobese$age,dobese$heart_rate)
  d$group<-d[,groupname]
  library(GGally)
  library(ggplot2)
  p1<-ggpairs(d, aes(color=group),
              upper = list(continuous = wrap("cor", size=sizeFont)),
              title=plot_title) +
    theme_bw()
  
  file<-paste('images/Distribution_continuous_',source,'.png',sep='')
  #postscript(file=file, paper="special", width=6, height=6)
  png(file=file,width=45,height=30,units="cm",res=150)
  
  print(p1)
  dev.off()
}