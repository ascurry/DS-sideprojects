library(tidyverse)
library(lubridate)
library(dummies)

HAdat <-  as_tibble(read.csv("EVOreal_time_synth.csv.crdownload"))

head(HAdat)
nSubs <-as.numeric( length(unique(HAdat$ID)) )

#### Let's first take a look at range of PTA (pure tone audiometric thresholds)
sort(unique(HAdat$PTA4))
# 0 is a weird number?? remove these individuals
HAdat <- filter(HAdat,PTA4 != 0)
HAdat$ID <- factor(HAdat$ID)
str(HAdat)
sort(unique(HAdat$PTA4))
# make categorical variable based on severity of hearing loss
# 25 - 40 is mild
# 40 - 55 is mod
# 55 - 70 is mod-sev
# 70- 90 is severe

HAdat <- HAdat %>%
  mutate(HLtype = ifelse(PTA4 < 25,"Norm",
                         ifelse( (PTA4 >=25 & PTA4 < 40), "Mild",
                                 ifelse( (PTA4 >=40 & PTA4 < 55), "Mod",
                                         ifelse( (PTA4 >=55 & PTA4 < 70), "Mod-Sev",
                                                 ifelse(PTA4 >= 70, "Sev",
                                                "NoLab")))))) %>%
  print(n=10, width=Inf)

#### find # of participants in each HLtype group
HAdat %>%
  group_by(HLtype,ID) %>%
  summarise() %>%
  count()

######## Let's sort by ID then by time stamp in order to visualize HA use throughout a typical day

## want to get average daily so first want to break down timestamps 
HAdat_times <- HAdat %>%
  mutate(year = year(Timestamp), 
         month = month(Timestamp),
         day = mday(Timestamp),
         time = format( strptime(Timestamp, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S" ),
         hour = hour(Timestamp),
         min = minute(Timestamp)
         ) %>%
   arrange(ID, year, month, day, time) 


 ### histogram
ggplot(HAdat_times, aes(x = hour)) +
  geom_freqpoly(aes(color = SoundClass), binwidth = 1)

ggplot(HAdat_times) +
  geom_bar(aes(x = SoundClass))
 
ggplot(HAdat_times, aes(x = hour, y = ..density..)) +
  geom_freqpoly(aes(color=SoundClass), binwidth = 1, size = 1.5) +
  ggtitle("Sound Environment Type Throughout The Day")+
  labs(x = "Hour",y = "Standardized Count", title = "Sound environment throughout the day") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12),
        panel.background = element_rect(color = "black",fill = "white")) 

########################################
# repeat this but separate by HLtype
########################################
# define order of facets
wantedOrder <- c("Norm", "Mild", "Mod", "Mod-Sev")
HAdat_times$HLtype <- factor(HAdat_times$HLtype, levels = wantedOrder)

HAdat_times %>%
  group_by(HLtype)

ggplot(HAdat_times, aes(x = hour)) +
  geom_freqpoly(aes(color=SoundClass), binwidth = 1, size = 1.5) +
  facet_wrap(~HLtype, nrow = 4) +
  labs(x = "Hour",y = "Standardized Count", title = "Sound environment throughout the day") +
  theme(plot.title = element_text(size = 14),
        axis.title = element_text(size = 12),
        panel.background = element_rect(color = "black",
                                        fill = "white"))

HAdat_times %>%
  filter(hour > 5 | hour < 22) %>%
  group_by(HLtype,SoundClass,hour,ID) %>%
  summarise(ClassCount = n()) %>%
  group_by(HLtype,hour) %>%
  mutate(GroupAvg = mean(ClassCount)) %>%
  ggplot(aes(x=hour,y=GroupAvg,color = SoundClass)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~HLtype, nrow = 4)

  



########################################
# repeat this but separate by HLtype & just look at counts
########################################
ggplot(HAdat_times, aes(x = hour)) +
  geom_freqpoly(aes(color=SoundClass), binwidth = 1, size = 1.5) +
  facet_wrap(~HLtype, nrow = 4) +
  labs(x = "Hour",y = "Standardized Count", title = "Sound environment throughout the day") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"),
        panel.background = element_rect(color = "black",fill = "white")) 


# 4 Hearing Programs (selected by user) designed to enhance sound quality depenent on the listening environment
# Hearing Volume also can be actively adjusted by the user

print(nDays <- length(unique(HAdat_times$day)))

# Lets see which hearing program was used over time for different environments (environment determined by HAid)
HAdat_times %>%
  ggplot(aes(x = day, color = hProg)) +
  geom_freqpoly(binwidth = 1, size = 1) +
  facet_wrap(~SoundClass, nrow = 4) +
  theme(strip.text = element_text(size = 14))

HAdat_times %>%
  filter(hProg == "Medium")

############# create a new variables to understand usage time of HA
HAdat_times %>%
  filter(ID == 10) %>%
  print(n=10,width = Inf)



  

######### Let's next look at just hearing threshold for each person and this relationship with some parameters of interest
# Some variables most interested in:
#   SoundClass (sound environment as estimated by HA)
# hProg (hearing aid program)
# fbSPL (Sound Pressure Level in full bandwidth)
# fbNf (Noise floor measured in full bandwidth)
# fbME (Modulation envelope)
# fbSNR (signal to noise ratio)
# fbMI (modulation index)
# PTA4 (pure tone average across .5, 1, 2 and 4 kHz on best hearing ear)

HAdat_times %>%
  select(ID,PTA4,fbSPL,fbNf,fbME,fbSNR,fbMI,day,hour)

# Next, we want to be able to classify the sound environment based on the measured predictors
# Why?? How does this help??

# Want to classify HLtype based on variables --- idea is that hearing loss severity may affect 1) quality of HA output and 2) use of HA so....if we can classify the different HLtypes based on their environments, we may able to start to tweak HA function based on HL severity? 

# Run PCA
# remove categorical variables
str(HAdat_times)
# remove HLtype (classifier label)
# remove ID, Timestamp, year, month, day, time, min, SoundClass
# convert hProg

HAforPCA <- subset(HAdat_times, select = 
                     -c(HLtype, ID, Timestamp,
                        year, month, day,
                        time, min, SoundClass))

HAforPCA$hProg <- dummy(HAforPCA$hProg)

str(HAforPCA)

# check for missing values in each variable:
sapply(HAforPCA, function(x) sum(is.na(x)))
HAforPCA <- subset(HAforPCA, 
                   select = -c(LonRel, LatRel,hVol))
# multiple imputation for hVol na values


#principal component analysis
HApca_res <- prcomp(HAforPCA, scale. = T)
summary(HApca_res)

screeplot(HApca_res, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(HApca_res$sdev^2 / sum(HApca_res$sdev^2))

plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 9, col="blue", lty=5)
abline(h = 0.82467, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC9"),
       col=c("blue"), lty=5, cex=0.6)

# biplot(prin_comp, scale = 0)
# 
# homes.pcaScores <- data.frame(HApca_res$x[, 1:2]) # we only need the first two principal components
# ggplot(homes.pcaScores, aes(y = PC1, x = PC2)) + geom_point(col = 'tomato2')


HA_PCs <- as.data.frame(HApca_res$x[,1:9])
HA_PCs <- cbind(HA_PCs, HAdat_times$HLtype)
names(HA_PCs)[10] <- "HLtype"

library(class)

nobs <- dim(HA_PCs)[1]

set.seed(3)
train=sample(nobs,nobs*.8) # 80% training set
NotTrain = (1:nobs)[!(c(1:nobs)) %in% sort(train)]

train.X=HA_PCs[train,1:9]
test.X=HA_PCs[NotTrain,1:9]
train.Y=HA_PCs$HLtype[train]
test.Y = HA_PCs$HLtype[NotTrain]

#define an error rate function and apply it to obtain test/training errors
calc_error_rate <- function(predicted.value, true.value){
  return(mean(true.value!=predicted.value)) 
}

# set.seed(1)
# knn.pred=knn(train.X,test.X,train.HL_label,k=1)
# table(knn.pred,train.HL_label)
# mean(knn.pred == train.HL_label)
# mean(knn.pred != train.HL_label)
# 
# knn.pred=knn(train.X,test.X,train.HL_label,k=15)
# table(knn.pred,train.HL_label)
# mean(knn.pred==train.HL_label)
# mean(knn.pred != train.HL_label)

#k-fold CV to select k
nfold = 10
set.seed(1)
# cut() divides the range into several intervals
folds <-  seq.int(nrow(train.X)) %>%
  cut(breaks = nfold, labels=FALSE) %>%  
  sample

do.chunk <- function(chunkid, folddef, Xdat, Ydat, k){ 
  train = (folddef!=chunkid)# training index
  Xtr = Xdat[train,] # training set by the index
  Ytr = Ydat[train] # true label in training set
  Xvl = Xdat[!train,] # test set
  Yvl = Ydat[!train] # true label in test set
  predYtr = knn(train = Xtr, test = Xtr, cl = Ytr, k = k) # predict training labels
  predYvl = knn(train = Xtr, test = Xvl, cl = Ytr, k = k) # predict test labels
  data.frame(fold =chunkid, # k folds 
             train.error = calc_error_rate(predYtr, Ytr),#training error per fold 
             val.error = calc_error_rate(predYvl, Yvl)) # test error per fold
}

# set error.folds to save validation errors
error.folds=NULL

# create a sequence of data with an interval of 10
kvec = c(1, seq(10, 50, length.out=5))
set.seed(1)

for (j in kvec){
  tmp = ldply(1:nfold, do.chunk, # apply do.function to each fold
              folddef=folds, Xdat=train.X, Ydat=train.HL_label, k=j) # required arguments
  tmp$neighbors = j # track each value of neighbors
  error.folds = rbind(error.folds, tmp) # combine the results 
}

#melt() in the package reshape2 melts wide-format data into long-format data
errors <- melt(error.folds, id.vars = c("fold","neighbors"),
               value.name = "error")

# means <- c()
# for (k in 1:10) {
#   knn.pred = knn(train.X, 
#                  test.X, 
#                  train.HL_label,
#                  k = k)
#   means[k] = mean(knn.pred == train.HL_label)
#   errors[k] = mean(knn.pred !=train.HL_label)
# }

val.error.means <-  errors %>%
  #select all rows of validation errors
  filter(variable == "val.error") %>% 
  #group the selected data by neighbors
  group_by(neighbors, variable) %>%
  #cacluate CV error for each k
  summarise_each(list(mean), error) %>%
  #remove existing grouping
  ungroup() %>% 
  filter(error==min(error))
#the best number of neighbors
numneighbor <-  max(val.error.means$neighbors)
numneighbor


#training error
set.seed(20)
pred.YTtrain = knn(train=train.X, 
                   test=test.X, 
                   cl=train.Y, 
                   k=numneighbor)

knn_traing_error <- calc_error_rate(predicted.value=pred.YTtrain, true.value=train.Y)
knn_traing_error


#test error
set.seed(20)
pred.YTest = knn(train=train.X, 
                 test=test.X, 
                 cl=train.Y, 
                 k=numneighbor)

knn_test_error <- calc_error_rate(predicted.value=pred.YTest, true.value=test.Y)
knn_test_error

#confusion matrix
conf.matrix = table(predicted=pred.YTest, true=test.Y)
conf.matrix

# Test accuracy rate
sum(diag(conf.matrix)/sum(conf.matrix))

# Test error rate
1 - sum(diag(conf.matrix)/sum(conf.matrix))
