library(e1071)
library(MASS)
library(dplyr)
library(reshape2)
#library(caret)
library(missForest)
library(rpart)
library(party)
library(randomForest)
library(glmnet)
library(clusterGeneration)
library(mnormt)
set.seed(1)
setwd("/u/l/u/luqman/stat761/data/nhtsa/cv")
system("sh cleancode_driver.sh > log.txt")
############ read data into R
bar <- read.csv("barclean.psv",sep="|",header=TRUE,na.strings=c("",999,9999,99999))
occ <- read.csv("occ01clean.psv",sep="|",header=TRUE,na.strings=c("",999,9999,99999,100000))
rest <- read.csv("rest01clean.psv",sep="|",header=TRUE,na.strings="")
test <- read.csv("testclean.psv",sep="|",header=TRUE,na.strings=c("",999,9999,99999,999999,10000))
veh <- read.csv("veh1clean.psv",sep="|",header=TRUE,na.strings=c("",999,9999,99999,999999))

############# remove unnecessary variables from each data set
exvars <- c("VEHNO","BARRIGD","BARSHPD","BARCOM")
tmp <- bar[,!(names(bar) %in% exvars)]
bar <- tmp

## these variables are deleted because they are constant or redundant:
## VEHNO, OCCLOC, OCCLOCD, OCCTYPD, OCCSEXD, MTHCALD, DUMDSC, DUMMAN,
## DUMMOD, DUMSIZD, SEPOSND, CNTRH1D, CTRH2D, CNTRC1D, CNTRC2D,
## CNTRL1D, CNTRL2D, OCCCOM

## additionally remove columns that are alternative measures of HIC:
## CNTRH1, CNTRH2, CNTRH1D, CTRH2D, CNTRC1, CNTRC2, CNTRC1D, CNTRC2D,
## CNTRL1, CNTRL2, CNTRL1D, CNTRL2D, T1, T2, CLIP3M, LFEM, RFEM, CSI,
## LBELT, SBELT, TTI, PELVG

## remove variables that measure damage after the crash
## AX1, AX2, AX3, AX4, AX5, AX6, AX7 
## AX8, AX9, AX10, AX11, AX12, AX13, AX14  
## AX15, AX16, AX17, AX18, AX19, AX20, AX21 
## MTHCAL, LINK, BMPENG, SILENG, APLENG, DPD1, DPD2
## DPD3, DPD4, DPD5, DPD6, LENCNT, DAMDST, CRHDST


exvars <- c("VEHNO","OCCLOC","OCCLOCD","OCCTYPD","OCCSEXD","MTHCALD","DUMDSC","DUMMAN","DUMMOD","DUMSIZD",
            "SEPOSND","CNTRH1","CNTRH2","CNTRH1D","CTRH2D","CNTRC1","CNTRC2","CNTRC1D","CNTRC2D","CNTRL1",
            "CNTRL2","CNTRL1D","CNTRL2D","T1","T2","CLIP3M","LFEM","RFEM","CSI","LBELT","SBELT","TTI","PELVG",
            "OCCCOM","MTHCAL")
tmp <- occ[,!(names(occ) %in% exvars)]
occ <- tmp

## remove DEPLOY and RSTMNT because they are associated with individual restraints
exvars <- c("VEHNO","OCCLOC","OCCLOCD","RSTTYPD","RSTMNT","RSTMNTD","DEPLOY","DEPLOYD","RSTCOM")
tmp <- rest[,!(names(rest) %in% exvars)]
rest <- tmp

exvars <- c("VERNO","TITLE","TSTOBJ","TSTDAT","TSTPRF","TSTPRFD","TSTREF","CONNO","TSTTYP","TSTTYPD","TSTCFND","TKSURFD","TKCONDD","RECTYPD","LINKD","TOTCRV","TSTCOM","LINK")
tmp <- test[,!(names(test) %in% exvars)]
test <- tmp

## remove MAKE and MODEL but keep MAKED and MODELD so that R will treat them as factor variables
exvars <- c("VEHNO","MAKE","MODEL","NHTSANO","BODYD","VIN","ENGINED","TRANSMD","STRSEPD","COLMECD","MODINDD","MODDSC","BMPENGD","SILENGD","APLENGD","VDI","VEHCOM", "AX1", "AX2", "AX3", "AX4", "AX5", "AX6", "AX7","AX8", "AX9", "AX10", "AX11", "AX12", "AX13", "AX14","AX15",
            "AX16", "AX17", "AX18", "AX19", "AX20", "AX21","BMPENG", "SILENG", "APLENG","DPD1", "DPD2", "DPD3", "DPD4", "DPD5", "DPD6", 
            "LENCNT", "DAMDST", "CRHDST","YEAR")
tmp <- veh[,!(names(veh) %in% exvars)]
veh <- tmp
veh$VEHSPD[!is.na(veh$VEHSPD) & veh$VEHSPD > 900] <- NA

#################### transform angular variables
angle.change <- function(x){
  if(!is.na(x) & x > 180) x <- x-360
  return(x)
}

n <- dim(bar)[1]
tmp <- rep(NA,n)
for(i in 1:n){
  tmp[i] <- angle.change(bar$BARANG[i])
}
bar$BARANG <- tmp

n <- dim(test)[1]
tmp <- rep(NA,n)
for(i in 1:n){
  tmp[i] <- angle.change(test$IMPANG[i])
}
test$IMPANG <- tmp

n <- dim(veh)[1]
tmp <- rep(NA,n)
for(i in 1:n){
  tmp[i] <- angle.change(veh$CRBANG[i])
}
veh$CRBANG <- tmp

n <- dim(veh)[1]
tmp <- rep(NA,n)
for(i in 1:n){
  tmp[i] <- angle.change(veh$PDOF[i])
}
veh$PDOF <- tmp

n <- dim(veh)[1]
tmp <- rep(NA,n)
for(i in 1:n){
  tmp[i] <- angle.change(veh$CARANG[i])
}
veh$CARANG <- tmp

############ rest contains multiple rows for each TSTNO; combine the rows here
n <- dim(rest)[1]
rest.ncol <- dim(rest)[2]
u.RSTTYP <- unique(sort(rest$RSTTYP))  # unique RSTTYP
kk <- length(u.RSTTYP)
dum.matrix <- data.frame(matrix(0,nrow=n,ncol=kk))
names(dum.matrix) <- paste0("RST",u.RSTTYP)
for(i in 1:n){
  if(!is.na(rest$RSTTYP[i])){
    for(j in 1:(kk-1)){ 
      if(u.RSTTYP[j] == rest$RSTTYP[i]){
        dum.matrix[i,j] <- 1
        break
      }
    }
  }
}


u.TSTNO <- unique(sort(rest$TSTNO))  # unique TSTNO
rest.exp <- data.frame(matrix(0,nrow=length(u.TSTNO),ncol=kk+1))
j <- 0
for(i in u.TSTNO){
  gp <- rest$TSTNO == i
  j <- j+1
  rest.exp[j,1] <- i
  rest.exp[j,2:(kk+1)] <- apply(dum.matrix[gp,],2,sum)
}
names(rest.exp) <- c("TSTNO",names(dum.matrix))

for(j in 2:(kk+1)){ 
  rest.exp[,j] <- as.factor(rest.exp[,j])
}

############ merge data sets
merged <- merge(bar,occ,by="TSTNO",all=FALSE)
merged <- merge(merged,test,by="TSTNO",all=FALSE)
merged <- merge(merged,veh,by="TSTNO",all=FALSE)
merged <- merge(merged,rest.exp,by="TSTNO",all=FALSE)
n <- dim(merged)[1] #K=2802
merged$HIC2 <- rep(NA,n)
merged$HIC2[!is.na(merged$HIC) & merged$HIC > 999] <- 1
merged$HIC2[!is.na(merged$HIC) & merged$HIC <= 999] <- 0
merged$HIC3 <- rep(NA,n)
merged$HIC3[!is.na(merged$HIC) & merged$HIC <= 300] <- 1
merged$HIC3[!is.na(merged$HIC) & merged$HIC > 300 & merged$HIC <= 999] <- 2
merged$HIC3[!is.na(merged$HIC) & merged$HIC > 999] <- 3
############ write GUIDE description file
k <- dim(merged)[2] #K=111
merged.names <- names(merged)
role <- NULL
role <- rep("n",k)
for(j in 1:k){
  if(class(merged[,j]) == "factor") role[j] <- "c"
}
role[merged.names %in% "TSTNO"] <- "x"
role[merged.names %in% "HIC"] <- "x"
role[merged.names %in% "HIC3"] <- "x"
role[merged.names %in% "HIC2"] <- "d"
role[merged.names %in% "YEAR"] <- "x"

## Deleting rows where HIC2 is NA
merged_no <- merged[!is.na(merged$HIC2),] ##n =2793
merged_guide <- merged_no

########### write GUIDE data file
write.csv(merged_guide,"merged_driver.csv",row.names=FALSE)
write("merged_nhtsa_driver.txt","merged_driver.dsc")
write("NA",file="merged_driver.dsc",append=TRUE)
write("2",file="merged_driver.dsc",append=TRUE)
role[k+1] <- "w"
write.table(cbind(1:(k+1),c(names(merged_guide),"wt"),role),"merged_driver.dsc",row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE)
merged_no$gp_ind <- floor(sample(c(0:9), size = nrow(merged_no), replace = TRUE))

##Deleting columns HIC and HIC3 and storing TSTNO is a separate list for later use
merged_no$HIC <- NULL
merged_no$HIC3 <- NULL
x <- merged_no$TSTNO
merged_no$TSTNO <- NULL


##creating dummy records for MAKED since randomForest and MissForest cannot handle variables with more than 53 level
x <- nrow(merged_no)
y <- length(unique(merged_no$MAKED))

u.MAKED <- unique(sort(merged_no$MAKED))  
dum.matrix1 <- data.frame(matrix(0,nrow=x,ncol=y))
names(dum.matrix1) <- (u.MAKED)

for (i in 1:x) {
  for (t in 1:y) {
    if (names(dum.matrix1)[t] == merged_no$MAKED[i]) {
      dum.matrix1[i, t] <- 1
      break
    } else {
    }
  }
}

for (i in 1:y)
{
  dum.matrix1[,i] <- as.factor(dum.matrix1[,i])
}

merged_maked <- cbind(merged_no,dum.matrix1)
#str(dum.matrix1)

##drop varaibles that have more than 53 levels
merged_maked$MAKED <- NULL
merged_maked$MODELD <- NULL
merged_maked$OCCHT <- NULL
merged_maked$RECTYP <- NULL
merged_maked$HIC2 <- as.factor(merged_maked$HIC2)
#merged_maked$YEAR <- as.factor(merged_maked$YEAR)
#columns that missing value imputation
miss_impute <- merged_maked[colSums(is.na(merged_maked)) > 0]

#imputation for factor columns
miss_impute_factor <- merged_maked[,sapply(merged_maked, is.factor)]
miss_impute_numeric <- merged_maked[,sapply(merged_maked, is.numeric)]
colnames(miss_impute_factor)[which(names(miss_impute_factor) == "LAND ROVER")] <- "LAND_ROVER"
names_numeric <- names(miss_impute_numeric)
names_factor <- names(miss_impute_factor)

#miss_impute_factor[colSums(is.na(miss_impute_factor)) > 0]

miss_impute_factor$SEPOSN = factor(miss_impute_factor$SEPOSN, levels=c(levels(miss_impute_factor$SEPOSN), "MISSING"))
miss_impute_factor$SEPOSN[is.na(miss_impute_factor$SEPOSN)] <- "MISSING"
miss_impute_factor$CTRL2 = factor(miss_impute_factor$CTRL2, levels=c(levels(miss_impute_factor$CTRL2), "MISSING"))
miss_impute_factor$CTRL2[is.na(miss_impute_factor$CTRL2)] <- "MISSING"
miss_impute_factor$TKSURF = factor(miss_impute_factor$TKSURF, levels=c(levels(miss_impute_factor$TKSURF), "MISSING"))
miss_impute_factor$TKSURF[is.na(miss_impute_factor$TKSURF)] <- "MISSING"
miss_impute_factor$TKCOND = factor(miss_impute_factor$TKCOND, levels=c(levels(miss_impute_factor$TKCOND), "MISSING"))
miss_impute_factor$TKCOND[is.na(miss_impute_factor$TKCOND)] <- "MISSING"
#miss_impute_factor$YEAR = factor(miss_impute_factor$YEAR, levels=c(levels(miss_impute_factor$YEAR), "MISSING"))
#miss_impute_factor$YEAR[is.na(miss_impute_factor$YEAR)] <- "MISSING"
miss_impute_factor$BODY = factor(miss_impute_factor$BODY, levels=c(levels(miss_impute_factor$BODY), "MISSING"))
miss_impute_factor$BODY[is.na(miss_impute_factor$BODY)] <- "MISSING"
miss_impute_factor$ENGINE = factor(miss_impute_factor$ENGINE, levels=c(levels(miss_impute_factor$ENGINE), "MISSING"))
miss_impute_factor$ENGINE[is.na(miss_impute_factor$ENGINE)] <- "MISSING"
miss_impute_factor$TRANSM = factor(miss_impute_factor$TRANSM, levels=c(levels(miss_impute_factor$TRANSM), "MISSING"))
miss_impute_factor$TRANSM[is.na(miss_impute_factor$TRANSM)] <- "MISSING"
miss_impute_factor$STRSEP = factor(miss_impute_factor$STRSEP, levels=c(levels(miss_impute_factor$STRSEP), "MISSING"))
miss_impute_factor$STRSEP[is.na(miss_impute_factor$STRSEP)] <- "MISSING"
miss_impute_factor$COLMEC = factor(miss_impute_factor$COLMEC, levels=c(levels(miss_impute_factor$COLMEC), "MISSING"))
miss_impute_factor$COLMEC[is.na(miss_impute_factor$COLMEC)] <- "MISSING"
miss_impute_factor$MODIND = factor(miss_impute_factor$MODIND, levels=c(levels(miss_impute_factor$MODIND), "MISSING"))
miss_impute_factor$MODIND[is.na(miss_impute_factor$MODIND)] <- "MISSING"

merged_combi <- cbind(miss_impute_numeric,miss_impute_factor) ##n =2793
glmnet.err<- 0
ctree.err <- 0
rpart.err <- 0
guidetree.err <- 0
guideforest.err <- 0
guidetree0.err <- 0
rF.err <- 0
lmerr <- 0
lmnet.con.err <- 0
ldamod.err <- 0
qdamod.err <- 0
svmmod.err <- 0
cForest.err <-0

glmnet.time <- 0
ctree.time <- 0
rpart.time <- 0
guidetree.time <- 0
guideforest.time <- 0
guidetree0.time <- 0
rF.time <- 0
lmerr.time <- 0
lmnet.con.time <- 0
ldamod.time <- 0
qdamod.time <- 0
svmmod.time <- 0
cForest.time <-0

wt <- rep(0,nrow(merged_guide))
k <- ncol(merged_guide)
v<-9
for (i in 0:v)
{
  v
  test <- merged_no$gp_ind ==i
  train <-merged_no$gp_ind != i
  gp_ind_var <- merged_no$gp_ind
  #merged_no$gp_ind<-NULL

  wt[test] <- 0
  wt[train] <- 1
  ptm <- proc.time()
  write.table(cbind(merged_guide[,-(k+1)],wt),"merged_nhtsa_driver.txt",row.names=FALSE,quote=TRUE)
  system("guide < merged_nhtsa_driver.in > logtree_driver.txt")
  tmp <- read.table("merged_nhtsa_driver.fit",header=TRUE)
  guidetree.err <- guidetree.err+sum(tmp$observed[test] != tmp$predicted[test])
  x<-proc.time() - ptm
  guidetree.time <- guidetree.time + x[3]

  ptm <- proc.time()
  system("guide < merged_nhtsa_driver_forest.in > logforest_driver.txt")
  tmp.forest <- read.table("merged_nhtsa_driver_forest.fit",header=TRUE)
  guideforest.err <- guideforest.err+sum(tmp.forest$observed[test] != tmp.forest$predicted[test])
  x<-proc.time() - ptm
  guideforest.time <- guideforest.time + x[3]

  ptm <- proc.time()
  fit.rpart <- rpart(HIC2 ~ ., method="class", data=merged_maked[train,])
  valid.rpart = predict(fit.rpart, newdata=merged_maked[test,],type="class")
  rpart.err <-  rpart.err+sum(merged_guide[test,]$HIC2 != valid.rpart)
  x<-proc.time() - ptm
  rpart.time <- rpart.time + x[3]
  print("rpart done")
  
  test.df.m <- merged_combi[test,]
  train.df.m <- merged_combi[train,]
  train.numeric <- missForest(train.df.m[names_numeric],maxiter=1)
  train.numeric.data <- train.numeric$ximp
  combi_train_imp_data <- cbind(train.numeric.data,train.df.m[names_factor])
  combi_test_train_numeric <- rbind(train.numeric.data,test.df.m[names_numeric])
  combi_test_train_numeric_imp <- missForest(combi_test_train_numeric,maxiter=1)
  combi_test_train_numeric_imp_data <- combi_test_train_numeric_imp$ximp

  combi_test_train_factor <- rbind(train.df.m[names_factor],test.df.m[names_factor])
  combi_test_train <- cbind(combi_test_train_numeric_imp_data,combi_test_train_factor)
  test.df.m_imp <- combi_test_train[combi_test_train$gp==i,]
  test.df.m_imp$HIC2 <- as.factor(test.df.m_imp$HIC2)
  
  # loops through factors and standardizes the levels
  totalData <- rbind(combi_train_imp_data, test.df.m_imp)
  for (f in 1:length(names(totalData))) {
    levels(combi_train_imp_data[, f]) <- levels(totalData[, f])
  }

  ptm <- proc.time()
  fit.rF <- randomForest(HIC2 ~ ., data=combi_train_imp_data)
  valid.rF = predict(fit.rF, newdata=test.df.m_imp,type="class")
  rF.err <-  rF.err+sum(test.df.m_imp$HIC2 != valid.rF)
  x<-proc.time() - ptm
  rF.time <- rF.time + x[3]
  print("rF done")
  
  ptm <- proc.time()
  fit.ctree <- ctree(as.factor(HIC2) ~ ., data=combi_train_imp_data)
  # png("ctree_driver.png")
  # plot(fit.ctree)
  # dev.off()
  valid.ctree = predict(fit.ctree, newdata=test.df.m_imp,type="response")
  ctree.err <- ctree.err+sum(test.df.m_imp$HIC2 != valid.ctree)
  x<-proc.time() - ptm
  ctree.time <- ctree.time + x[3]  
  print("ct done")
  
  # ptm <- proc.time()
  # fit.cforest <- cforest(HIC2 ~ ., data=combi_train_imp_data)
  # valid.cforest = predict(fit.cforest, newdata=test.df.m_imp,type="response")
  # cForest.err <-  cForest.err+sum(test.df.m_imp$HIC2 != valid.cforest)
  # x<-proc.time() - ptm
  # ccForest.time <- cForest.time + x[3]
  # print("cF done")
  
  ## PCA for GLMNET,LDA,QDA and SVM models
  nhtsa.train.pca <- prcomp(train.numeric.data,
                   center = TRUE,
                   scale. = TRUE)

  nhtsa_linear_train_data <- nhtsa.train.pca$x

  nhtsa.all.pca <- prcomp(combi_test_train_numeric_imp_data,
                            center = TRUE,
                            scale. = TRUE)

  nhtsa_linear_all_data <- nhtsa.all.pca$x
  nhtsa_linear_data <- rbind(nhtsa_linear_train_data,nhtsa_linear_all_data[test,])


  for (i in 1:ncol(combi_test_train_factor)-1) {
    if(nlevels(combi_test_train_factor[,i])==1) {combi_test_train_factor[,i] <- NULL}
  }


  xy<- model.matrix(HIC2~ ., data=combi_test_train_factor )
  y <- combi_test_train_factor$HIC2


  nhtsa_glmnet <- cbind (xy[,-1], nhtsa_linear_data)
  #dim(nhtsa_linear_data)
  #dim(nhtsa_glmnet)
  #nhtsa_glmnet[2499:2500,]
  #names(nhtsa_glmnet)
  #row.names(nhtsa_linear_data)
  ptm <- proc.time()
  glmmod <- glmnet(x=nhtsa_glmnet[train,],y=y[train],alpha=1,family='binomial')
  cvfit <- cv.glmnet(x=nhtsa_glmnet[train,],y=y[train],alpha=1,family='binomial')
  xt <- nhtsa_glmnet[test,]
  yt <- y[test]
  pred <- predict(glmmod,newx=xt,s=cvfit$lambda.min,type="class")
  glmnet.err <- glmnet.err + sum(yt != pred)
  x<-proc.time() - ptm
  glmnet.time <- glmnet.time + x[3]
  print("glm done")

  xy.df <- as.data.frame(xy)
  xy.df.fact <- data.frame(lapply(xy.df, function(x) as.factor(as.character(x))))
  #lm_data <- cbind (xy.df.fact[,-1], nhtsa_linear_data)
 

  # ##linear regression
  exvars <- c("BARSHPUS3",
  "OCCSEXU",
  "DUMSIZUN",
  "SEPOSNMISSING",
  "CTRL2MISSING",
  "TKSURFMISSING",
  "TKCONDMISSING",
  "YEARMISSING",
  "BODYMISSING",
  "ENGINEMISSING",
  "TRANSMMISSING",
  "STRSEPMISSING",
  "COLMECMISSING",
  "MODINDMISSING",
  "RSTUNK1",
  "YUGO1")
  #tmp <- lm_data[,!(names(lm_data) %in% exvars)]
  #lm_data <- tmp
  #lm_data_train <- lm_data[train,]
  #for (i in 1:ncol(lm_data)-2) {
  #  if(nlevels(lm_data[,i])==1) {lm_data[,i] <- NULL}
  #}
  #for (i in 1:ncol(lm_data_train)-2) {
  #  if(nlevels(lm_data_train[,i])==1) {lm_data[,i] <- NULL}
  #}
  #lm_data_train <- lm_data[train,]
  #for (i in 1:ncol(lm_data[train,])) {a[i] <- nlevels(lm_data[train,][,i])}

  #lm.df <- cbind(y,lm_data)
  #lm.df <- data.frame(lapply(lm.df, function(x) as.numeric(as.character(x))))
  #lm_train.df <- lm.df[train,]

  #ifelse(n <- sapply(lm_train.df, function(x) length(levels(x))) == 1, "DROP", "NODROP")


  #Linear Regression with numeric data only
  ptm <- proc.time()
  lmmod <- lm(as.numeric(y[train])~.,data=as.data.frame(nhtsa_linear_data[train,]))
  pred <- predict(lmmod,newdata =as.data.frame(nhtsa_linear_data[test,]) )
  lmerr <- lmerr + sum(as.numeric(y[test]) != round(pred))
  x<-proc.time() - ptm
  lmerr.time <- lmerr.time + x[3]
  print("lm done")
  
  ptm <- proc.time()
  lm_data_con <- as.data.frame(nhtsa_linear_data)
  lm_data_con <- cbind(y,lm_data_con)
  lmmod <- lm(as.numeric(y[train])~.,data=lm_data_con[train,])
  pred <- predict(lmmod,newdata =lm_data_con[test,] )
  lmnet.con.err <- lmnet.con.err + sum(as.numeric(y[test]) != round(pred))
  x<-proc.time() - ptm
  lmnet.con.time <- lmnet.con.time + x[3]
  print("lmcon done")
  
  ##LDA, QDA and SVM
  ptm <- proc.time()
  ldamod <- lda(as.factor(y[train])~.,data=as.data.frame(nhtsa_linear_data[train,]))
  pred <- predict(ldamod,newdata =as.data.frame(nhtsa_linear_data[test,]) )
  ldamod.err <- ldamod.err + sum(y[test] != pred$class)
  x<-proc.time() - ptm
  ldamod.time <- ldamod.time + x[3]
  print("lda done")
  
  ptm <- proc.time()
  qdamod <- qda(as.factor(y[train])~.,data=as.data.frame(nhtsa_linear_data[train,]))
  pred <- predict(qdamod,newdata =as.data.frame(nhtsa_linear_data[test,]) )
  qdamod.err <- qdamod.err + sum(y[test] != pred$class)
  x<-proc.time() - ptm
  qdamod.time <- qdamod.time + x[3]
  print("qda done")

  ## svm
  ptm <- proc.time()
  svmmod <- svm(as.factor(y[train])~.,data=as.data.frame(nhtsa_linear_data[train,]))
  pred <- predict(svmmod,newdata =as.data.frame(nhtsa_linear_data[test,]) )
  svmmod.err <- svmmod.err + sum(y[test] != pred)
  x<-proc.time() - ptm
  svmmod.time <- svmmod.time + x[3]
  print("svm done")


}

write.csv(glmnet.err/v,"glmnet_driver.txt",row.names=FALSE)
write.csv(ctree.err/v,"ctree_driver.txt",row.names=FALSE)
write.csv(rpart.err/v,"rpart_driver.txt",row.names=FALSE)
write.csv(guidetree.err/v,"guidetree_driver.txt",row.names=FALSE)
write.csv(guideforest.err/v,"guideforest_driver.txt",row.names=FALSE)
write.csv(rF.err/v,"rF_driver.txt",row.names=FALSE)
write.csv(lmerr/v,"lmerr_driver.txt",row.names=FALSE)
write.csv(lmnet.con.err/v,"lmnetcon_driver.txt",row.names=FALSE)
write.csv(ldamod.err/v,"ldamod_driver.txt",row.names=FALSE)
write.csv(qdamod.err/v,"qdamod_driver.txt",row.names=FALSE)
write.csv(svmmod.err/v,"svmmod_driver.txt",row.names=FALSE)

write.csv(glmnet.time/v,"glmnet_driver_time.txt",row.names=FALSE)
write.csv(ctree.time/v,"ctree_driver_time.txt",row.names=FALSE)
write.csv(rpart.time/v,"rpart_driver_time.txt",row.names=FALSE)
write.csv(guidetree.time/v,"guidetree_driver_time.txt",row.names=FALSE)
write.csv(guideforest.time/v,"guideforest_driver_time.txt",row.names=FALSE)
write.csv(rF.time/v,"rF_driver_time.txt",row.names=FALSE)
write.csv(lmerr.time/v,"lmerr_driver_time.txt",row.names=FALSE)
write.csv(lmnet.con.time/v,"lmnetcon_driver_time.txt",row.names=FALSE)
write.csv(ldamod.time/v,"ldamod_driver_time.txt",row.names=FALSE)
write.csv(qdamod.time/v,"qdamod_driver_time.txt",row.names=FALSE)
write.csv(svmmod.time/v,"svmmod_driver_time.txt",row.names=FALSE)
