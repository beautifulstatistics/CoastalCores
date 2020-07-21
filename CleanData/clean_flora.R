library(readxl)

raw <- read_excel("NJFlora.xls")
raw <- data.frame(raw)

wittled_data <- raw[,1:37]

#lets clean "Site"
wittled_data$Site <- factor(wittled_data$Site)
levels(wittled_data$Site)

levels(wittled_data$Site)[c(1:3,14,17,18,21)] <- c(rep("Barnegat Inlet",4),rep("Shelter Island",3))



levels(wittled_data$Site)[c(2:6,8,9,11:13,15:17)] <- NA

wittled_data <- wittled_data[!is.na(wittled_data$Site),]

#now for habitat

wittled_data$Habitat <- factor(wittled_data$Habitat)

levels(wittled_data$Habitat) <- c(rep("Edge",3),rep("Interior",3))
levels(wittled_data$Habitat)

#remove the replicate column

wittled_data$Replicate <- NULL
wittled_data$sample.. <- NULL
wittled_data$Core.. <- NULL
wittled_data$leaf.pan.. <- NULL
wittled_data$algae <- NULL
wittled_data$below.pan.. <- NULL
wittled_data$detritus.pan <- NULL

#lets clean leaf.biomass, below.biomass,total, and below.dry

wd_asnumeric <- sapply(wittled_data[,-c(1,2,3)],as.numeric)

#lets look at the items that have been coarced to NA's
#we use the exclusive or bc it only looks at what is different

coarced_nas <- xor(is.na(wittled_data[,-c(1,2,3)]),is.na(wd_asnumeric))

wittled_data[,-c(1,2,3)][coarced_nas]

#looks like they are all just periods. no biggy.
#lets go ahead and repalce those columms of wittled_data

wittled_data[,-c(1,2,3)] <- wd_asnumeric

#ok it looks like the last thing we need to do is
#convert the date format to a continuous numeric also.

Leaf_AFDW <- wittled_data[,19]-wittled_data[,20]
Below_AFDW <- wittled_data[,21]-wittled_data[,22]
Algae_AFDW <- wittled_data[,23]-wittled_data[,24]
Detritus_AFDW <- wittled_data[,25]-wittled_data[,26]
Epipyte_AFDW <- wittled_data[,27]-wittled_data[,28]
Seed_AFDW <- wittled_data[,29]-wittled_data[,30]

AFDW <- data.frame(Leaf_AFDW,Below_AFDW,Algae_AFDW,
                     Detritus_AFDW,Epipyte_AFDW,Seed_AFDW)

AFDW[AFDW<0] <- NA
AFDW$Total_AFDW <- rowSums(AFDW,na.rm=T)

wittled_data$Date2 <- (wittled_data$Date-min(wittled_data$Date,na.rm=T))/(60*60*24*365.25) + 1998 +(31+28+30)/365.25
wittled_data$Date2 <- as.numeric(wittled_data$Date2)

wittled_data <- wittled_data[1:737,]
AFDW <- AFDW[1:737,]
wittled_data$Total.AFDW[wittled_data$Total.AFDW < 0] <- NA
wittled_data <- cbind(wittled_data,AFDW)

wittled_data$percent.wasting[wittled_data$percent.wasting>=1] <- NA
names(wittled_data)[9] <- "Percent.Wasting"
wittled_data$Habitat <- NULL

names(wittled_data)[c(16,17)] <- c("Ave.Leaf.Length","Ave.Leaf.Width")

cuts <- cut(wittled_data$Date2,1998:2011)
levels(cuts) <- 1998:2011

cuts <- factor(cuts)

wittled_data$Year <- cuts

############

flora <- wittled_data

flora <- flora[!is.na(flora$Year),]

write.csv(flora,file='flora.csv',row.names=F)


rm("AFDW",          "Algae_AFDW"    ,"Below_AFDW"   , "coarced_nas",  
   "cuts",          "Detritus_AFDW", "Epipyte_AFDW" , "Leaf_AFDW"   , 
   "raw",           "Seed_AFDW",     "wd_asnumeric" , "wittled_data" )

extrqc <- function(model,adjust='none',se='boot',
                   term=NA){
  sums <- summary(model,se=se)
  rn <- rownames(sums[[1]][[3]])[1]
  
  if(is.na(term)){
    if(rn == '(Intercept)'){
      term = 2
    } else {term=1}
  }
  
  df <- data.frame()
  for(i in sums){
    df <- rbind(df,i[[3]][term,])
  }
  names(df) <- attr(i[[3]][term,],'names')
  row.names(df) <- paste0(model$tau*100,'%')
  df[,4] <- p.adjust(df[,4],method=adjust)
  colnames(df)[1] <- rownames(sums[[1]][[3]])[term]
  round(df,3)
}

plotq <- function(model){
  if(nrow(coef(model))==1){inter=0}else{inter=NULL}
  ll <- length(model$tau)
  for(i in 1:ll){
    abline(c(inter,coef(model)[,i]),lty=ll-i+1)
  }
  name <- paste0(model$tau*100,'%')
  name <- rev(name)
  legend('topleft',
         legend=name,
         lty=1:ll)
}


