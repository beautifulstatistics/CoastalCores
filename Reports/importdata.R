library(ncdf4)
flora <- read.csv('flora.csv')

files <- list.files('~/Desktop/Projects/Flora/Daily')

time <- c()
tsi <- c()
for(f in files){
  si <- nc_open(paste0('./Daily/',f))
  tsi <- c(tsi,ncvar_get(si,'TSI'))
  time <- c(time,ncvar_get(si,'time'))
}

time <- as.numeric(as.Date(time,origin=as.Date('1610-01-01 00:00:00')))
flora$Date <- as.Date(flora$Date)

tsi_mean <- c()
for(i in flora$Date){
  tsi_mean <- c(tsi_mean,
                mean(tsi[which(time >= i-30 & time < i)]))
}

si <- data.frame(TSI=tsi_mean,Date=flora$Date,AFDW=flora$Total_AFDW)

rm('files','time','tsi','flora','i','f','tsi_mean')

######
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

