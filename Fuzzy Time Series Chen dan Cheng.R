data = as.vector(Data_Suku_Bunga$`BI rate`)
data


train = data[1:72]
test = data[73:84]

plot.ts(data, xlab="Waktu",ylab="BI rate", main="Time Series Plot Data Aktual n=84", cex.main=0.9)
points(data)


min_train = min(train) 
min_train
max_train = max(train) 
max_train

n_train=length(train)
n_train

k_train = round(1+(3.322*1.857332))
k_train

L = round(((max_train-min_train)/k_train),3)
L

intrv.1 = seq(min_train,max_train,len = k_train+1)
intrv.1


interval = data.frame(NA,nrow=length(intrv.1)-1,ncol=3)
names(interval) = c("bawah","atas","kel")
for (i in 1:length(intrv.1)-1) {
  interval[i,1]=intrv.1[i]
  interval[i,2]=intrv.1[i+1]
  interval[i,3]=i
}

interval = round(interval,3)


interval[2,'bawah']= interval[1,'atas']+0.001
interval[2,'atas']=interval[2,'bawah']+L

interval[3,'bawah']= interval[2,'atas']+0.001
interval[3,'atas']=interval[3,'bawah']+L

interval[4,'bawah']= interval[3,'atas']+0.001
interval[4,'atas']=interval[4,'bawah']+L

interval[5,'bawah']= interval[4,'atas']+0.001
interval[5,'atas']=interval[5,'bawah']+L

interval[6,'bawah']= interval[5,'atas']+0.001
interval[6,'atas']=interval[6,'bawah']+L

interval[7,'bawah']= interval[6,'atas']+0.001
interval[7,'atas']=interval[7,'bawah']+L

interval= round(interval,3)


n.tengah = data.frame(tengah=(interval[,1]+interval[,2])/2,kel=interval[,3])
n.tengah = round(n.tengah,3)


fuzifikasi=c() 
  for (i in 1:length(train)){
    for (j in 1:nrow(interval)){
      if (i!=which.max(train)){
        if (train[i]>=(interval[j,1])&train[i]<(interval[j,2])){
          fuzifikasi[i]=j
          break
        }
      }
      else {
        if (train[i]>=(interval[j,1])&train[i]<=(interval[j,2])){
          fuzifikasi[i]=j
          break
        }
      }
    }
  }
  
  flr <- data.frame(current_state=0, next_state=0)
  for(i in 1: length(fuzifikasi)){
    if(i < length(fuzifikasi)){
      flr[i,]=c(fuzifikasi[i],fuzifikasi[i+1])
    }
    else{
      flr[i,]=c(fuzifikasi[i],0)
    }
  }
  
  flrg = list()
  for (i in 1:nrow(interval)){
    flrgi=c()
    for (j in 1:(length(train)-1)){
      if (flr[j,1]==i){
        flrgi = c(flrgi,flr[j,2])
      }
    }
    flrg[[i]] = flrgi
  }
  uni = list()
  for (i in 1:nrow(interval)){
    y = flrg[[i]]
    r = unique(y)
    uni[[i]] = r
  }
  
  m = round(as.vector(rowMeans(interval[,1:2])),3)
  m
  
  jum = list()
  for (i in 1:nrow(interval)){
    jums = c()
    for (j in 1:length(uni[[i]])){
      jums = c(jums,m[uni[[i]][[j]]])
    }
    jum[[i]] = jums
  }
  
  meanpred = lapply(jum,mean)
 
  prediksi_chen_train = c()
  for (i in 1:length(train)){
    if (i==1){
      pred = NA
    }
    else{
      pred = meanpred[[fuzifikasi[(i-1)]]]
    }
      
     prediksi_chen_train = c(prediksi_chen_train, pred)
  }
  
  prediksi_chen_train

  galat_train_chen = c()
  for (i in 1:length(prediksi_chen_train)){
    if (i == 0){
      galat = NA
    }
    else {
      galat = train-prediksi_chen_train
    }
  }
    galat_train_chen = round(galat,3)

tabel_train_chen = cbind(train,fuzifikasi,prediksi_chen_train,galat_train_chen)
tabel_train_chen

RMSE_train_chen= sqrt(mean(galat_train_chen^2,na.rm = TRUE))
MAD_train_chen = mean((abs(galat_train_chen)),na.rm = TRUE)
MAPE_train_chen = mean(abs(galat_train_chen/train*100),na.rm = TRUE)
ketepatan_train_chen = cbind(RMSE_train_chen,MAD_train_chen,MAPE_train_chen)
ketepatan_train_chen = round(ketepatan_train_chen,3)
ketepatan_train_chen

###########################CHEN Testing###############################33

  
fuzifikasi_test=c() 
  for (i in 1:length(test)){
    for (j in 1:nrow(interval)){
      if (i!=which.max(test)){
        if (test[i]>=(interval[j,1])&test[i]<(interval[j,2])){
          fuzifikasi_test[i]=j
          break
        }
      }
      else {
        if (test[i]>=(interval[j,1])&test[i]<=(interval[j,2])){
          fuzifikasi_test[i]=j
          break
        }
      }
    }
  }

  flr_test <- data.frame(current_state=0, next_state=0)
  for(i in 1: length(fuzifikasi_test)){
    if(i < length(fuzifikasi_test)){
      flr_test[i,]=c(fuzifikasi_test[i],fuzifikasi_test[i+1])
    }
    else{
      flr_test[i,]=c(fuzifikasi_test[i],0)
    }
  }
  
  flrg_test = list()
  for (i in 1:nrow(interval)){
    flrgi_test=c()
    for (j in 1:(length(test)-1)){
      if (flr_test[j,1]==i){
        flrgi_test = c(flrgi_test,flr_test[j,1])
      }
    }
    flrg_test[[i]] = flrgi_test
  }
  
  prediksi_chen_test = c()
  for (i in 1:length(test)){
      pred_chen_test = meanpred[[fuzifikasi_test[(i)]]]
      prediksi_chen_test = c(prediksi_chen_test, pred_chen_test)
  }
  prediksi_chen_test


galat_chen_test = test-prediksi_chen_test
tabel_chen_test = cbind(test,prediksi_chen_test,galat_chen_test)
tabel_chen_test


RMSE_test = sqrt(mean(galat_chen_test^2, na.rm = TRUE))
MAD_test = mean((abs(galat_chen_test)))
MAPE_test = mean(abs(galat_chen_test/test))*100
ketepatan_chen_test = cbind(RMSE_test,MAD_test,MAPE_test)
ketepatan_chen_test = round(ketepatan_chen_test,3)
ketepatan_chen_test


######################cheng#############################

FLR = data.frame(fuzifikasi=0,left=NA,right =NA)
for (i in 1:length(fuzifikasi)) {
  FLR[i,1] = fuzifikasi[i]
  FLR[i+1,2] = fuzifikasi[i]
  FLR[i,3] = fuzifikasi[i]
}

FLR = FLR[-nrow(FLR),]
FLR

FLRG = table(FLR[,2:3])
FLRG

bobot = round(prop.table(table(FLR[,2:3]),1),5)
bobot

cheng=NULL
for (i in 1:nrow(FLR)){
  for (j in 1:nrow(bobot)){
    if (fuzifikasi[i]==j)
    {cheng[i]=sum(bobot[j,]*n.tengah[,1])} else
      if (fuzifikasi[i]==0)
      {cheng[i]=0}
  }
}

df = data.frame(cheng)
df[1,'cheng']= NA
df

Prediksi_cheng_train= c(df)
Prediksi_cheng_train = round(Prediksi_cheng_train$cheng,3)

#tabel pembanding


galat_cheng_train = c()
for (i in 1:length(Prediksi_cheng_train$cheng)){
  if (i == 0){
    eror = NA
  }
  else {
    eror = train-Prediksi_cheng_train$cheng
  }
}

galat_cheng_train = eror
tabel_cheng_train = cbind(train,fuzifikasi, Prediksi_cheng_train,galat_cheng_train)
tabel_cheng_train


#Uji ketepatan
RMSE_cheng_train= sqrt(mean(galat_cheng_train^2, na.rm = TRUE))
MAD_cheng_train = mean(abs(galat_cheng_train),na.rm = TRUE)
MAPE_cheng_train = mean(abs(galat_cheng_train/train*100), na.rm=TRUE)
ketepatan_cheng_train = cbind(RMSE_cheng_train,MAD_cheng_train,MAPE_cheng_train)
ketepatan_cheng_train = round (ketepatan_cheng_train,3)
ketepatan_cheng_train 


#############################testing cheng##############################

cheng_test=NULL
for (i in 1:nrow(flr_test)){
  for (j in 1:nrow(bobot)){
    if (fuzifikasi_test[i]==j)
    {cheng_test[i]=sum(bobot[j,]*n.tengah[,1])} else
      if (fuzifikasi_test[i]==0)
      {cheng_test[i]=0}
  }
}

Prediksi_cheng_test= cheng_test
Prediksi_cheng_test = round (Prediksi_cheng_test,3)


#tabel pembanding
galat_cheng_test = test-Prediksi_cheng_test
tabel_cheng_test = cbind(test,Prediksi_cheng_test,galat_cheng_test)
tabel_cheng_test


#Uji ketepatan
RMSE_cheng_test= sqrt(mean(galat_cheng_test^2))
MAD_cheng_test = mean(abs(galat_cheng_test))
MAPE_cheng_test = mean(abs(galat_cheng_test/test*100))
ketepatan_cheng_test = cbind(RMSE_cheng_test,MAD_cheng_test,MAPE_cheng_test)
ketepatan_cheng_test = round(ketepatan_cheng_test,3)
ketepatan_cheng_test


####tabel pebanding kriteria model#################
Kriteria_model_chen = cbind(ketepatan_chen_test,ketepatan_train_chen)
Kriteria_model_chen
Kriteria_model_cheng = cbind(ketepatan_cheng_test,ketepatan_cheng_train)
Kriteria_model_cheng

