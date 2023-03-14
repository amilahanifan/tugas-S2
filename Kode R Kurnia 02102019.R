#2.1 Importing Dataset (pilih salah satu saja)

#From Text (base)
lbw <- read.csv("D:/Data/Health-i/Dataset/lbw.csv") #Ubah menjadi slash D:\Data\Health-i\Dataset
View(lbw)

#From Text (readr)
library(readr)
lbw <- read_csv("D:/Data/Health-i/Dataset/lbw.csv")
View(lbw) #Melihat struktur data 

#2.2.1 Sorting Mengurutkan data berdasarkan variabel usia dengan urutan
#"ascending"
newdata <- lbw[order(lbw$AGE),] #Membuat objek "newdata"
View(newdata) #Melihat struktur data

#Mengurutkan data berdasarkan variabel usia dengan urutan "ascending" 
#dan berat bayi lahir "descending"
newdata1 <- lbw[order(lbw$AGE,-lbw$BWT),] #Membuat objek "newdata1" 
View(newdata1) #Melihat struktur data

#2.2.2 Recoding variable
#Membuat kode untuk BBLR
#Kode 1 dan 0 untuk bekerja dengan regresi/model 
#atau menyesuaikan dengan package dengan pengaturan kode 1 dan 0

lbw$LBW <- NA
lbw$LBW[lbw$BWT<2500] <- 1 
lbw$LBW[lbw$BWT>=2500] <- 0

#Kode 1 dan 2 untuk penyajian/tabel kontingensi
lbw$LBW1 <- NA
lbw$LBW1[lbw$LBW==1] <- 1 
lbw$LBW1[lbw$LBW==0] <- 2

View(lbw) #Melihat data

#Membuat kode untuk kelompok usia 
lbw$AGEGROUP <- NA
lbw$AGEGROUP[lbw$AGE<20] <- 1
lbw$AGEGROUP[lbw$AGE>=20] <- 0

#Membuat kode untuk riwayat kelahiran prematur 
lbw$PTL1 <- NA
lbw$PTL1[lbw$PTL==0] <- 0  
lbw$PTL1[lbw$PTL==1] <- 1
lbw$PTL1[lbw$PTL>=2] <- 2

#Membuat kode untuk kelompok kunjungan ke dokter
lbw$FTV1 <- NA
lbw$FTV1[lbw$FTV==0] <- 0
lbw$FTV1[lbw$FTV==1] <- 1
lbw$FTV1[lbw$FTV>=2] <- 2

#2.2.3 Changing the types of the variables
lbw$AGE <- as.numeric(lbw$AGE)
lbw$LWT <- as.numeric(lbw$LWT)
lbw$BWT <- as.numeric(lbw$BWT)
View(lbw)

#2.2.4.1 Selecting (keeping) the variables
#Pilihan pertama
varselect <- c("AGE", "LWT", "RACE")
newdata <- lbw[varselect]
View(newdata)

#Pilihan kedua
newdata <- lbw[c(2,3,4)]
View(newdata)

#2.2.4.2 Excluding (dropping) the variables
newdata1 <- lbw[c(-2,-3,-4)]
View(newdata1)

#2.2.4.3 Excluding (dropping) one observation
newdata2 <- lbw[-10,]
View(newdata2)

#2.2.4.4 Selecting observations
newdata3 <- lbw[1:20,]
View(newdata3)

#3.1 Creating factor variables, ordered factor variables and labeling the values
lbw$SMOKE <- factor(lbw$SMOKE,levels = c(0,1),labels = c("No", "Yes")) #skala nominal/binary
lbw$LBW <- factor(lbw$LBW,levels = c(0,1),labels = c("No", "Yes"))#skala nominal/binary
lbw$PTL1 <- ordered(lbw$PTL1,levels = c(0,1,2), labels = c("None", "One", "Two or more")) #skala ordinal
lbw$RACE <- factor(lbw$RACE,levels = c(1,2,3), labels = c("White", "Black", "Other")) #skala nominal
lbw$HT <- factor(lbw$HT,levels = c(0,1),labels = c("No", "Yes")) #skala nominal/binary
lbw$UI <- factor(lbw$UI,levels = c(0,1),labels = c("No", "Yes")) #skala nominal/binary
lbw$AGEGROUP <- factor(lbw$AGEGROUP,levels = c(0,1), labels = c("greater or equal 20 years","< 20 years")) #skala nominal/binary
lbw$FTV1 <- ordered(lbw$FTV1,levels = c(0,1,2),labels = c("None", "One", "Two or more")) #skala ordinal
View(lbw)

#3.2 Histogram
x <- lbw$LWT
h <- hist(x, xlab = "Weight of the mother (pounds)",
col = "green", border = "blue",
main = "Histogram with normal curve") 
xfit <- seq(min(x), max(x), length = 40) 
yfit <- dnorm(xfit, mean = mean(x),sd = sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col = "red", lwd = 2) 

#3.3 Stem and leaf plot
stem(lbw$BWT)

#3.4 Quantile-quantile plot
qqnorm(lbw$BWT)
qqline(lbw$BWT)

#3.5 Box and whisker plot or boxplot
boxplot(lbw$BWT~lbw$RACE,
col = c("red","green","blue"),
xlab = "Race of the  mother",
ylab = "Birthweight (grams)",
main = "Comparison of birthweight based on race")

#3.6 Scatter plot
plot(lbw$AGE,lbw$BWT,
xlab = "Age of the mother (years)",
ylab = "Birthweight (grams)",
main = "Scatter plot between age and birthweight")

#3.7 Bar graph
counts <- table(lbw$LBW,lbw$FTV1) #Membuat objek "counts"
barplot(counts,
beside = F,
col = c("darkblue","green"),
xlab = "Physician visits during first trimester",
main = "Low birthweight distribution by physician visits",
legend = rownames(counts)) 

#3.8 Pie chart
mytable <- table(lbw$FTV1)
pct <- round(mytable/sum(mytable)*100, digits = 0)
lbls <- paste(names(mytable),"\n", mytable, sep = "")
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep = "")
pie(mytable, labels = lbls, clockwise = T, col = rainbow(length(lbls)),
main = "Physician visits during first trimester") 

#4.1.1 Normality test
shapiro.test(lbw$BWT) #Uji normalitas variabel berat bayi lahir
mean(lbw$BWT) #rerata berat bayi lahir
sd(lbw$BWT) #simpangan baku berat bayi lahir
summary(lbw$BWT) #nilai minimum, kuartil 1, median, rerata, kuartil 3, dan maksimum
IQR(lbw$BWT) #Jarak interkuartil (Q3-Q1)

#4.2.1 Prevalence ratio
library(epitools) #paket "epitools" diperlukan; coding yang dipakai 1 = Yes dan 0 = No
alpha = 0.05
epitab(lbw$SMOKE,lbw$LBW, method = "riskratio",
conf.level = (1-alpha), pvalue = "chi2")

#4.2.2 Odds ratio
epitab(lbw$SMOKE, lbw$LBW, method = "oddsratio",
conf.level = 0.95, pvalue = "chi2")

#Tiga level; Baris pertama secara default menjadi referensi
epitab(lbw$RACE, lbw$LBW, method = "oddsratio",
conf.level = 0.95, pvalue = "chi2")

epitab(lbw$RACE, lbw$LBW, method = "oddsratio",
conf.level = 0.95, rev = "both", pvalue = "chi2")

#Mengubah posisi urutan baris dan kolom untuk tabel 2x2
lbw$LBW1 <- NA
lbw$LBW1[lbw$BWT<2500] <- 1
lbw$LBW1[lbw$BWT>=2500] <- 2
lbw$LBW1 <- factor(lbw$LBW1,levels = c(1,2),labels = c("Yes", "No"))

lbw$SMOKE1 <- NA
lbw$SMOKE1[lbw$SMOKE=="Yes"] <- 1
lbw$SMOKE1[lbw$SMOKE=="No"] <- 2
lbw$SMOKE1 <- factor(lbw$SMOKE1,levels = c(1,2),labels = c("Yes", "No"))

#Membuat tabel 2x2
library(gmodels) #paket "gmodels" diperlukan
CrossTable(lbw$SMOKE1, lbw$LBW1, prop.c = F, prop.t = F,digits = 1,
prop.chisq = F, format = c("SPSS"),chisq = T,
dnn = c("Maternal Smoking Status", "Low Birthweight")) 

CrossTable(lbw$SMOKE1, lbw$LBW1, prop.r = T, prop.t = F, prop.c = F,
prop.chisq = F, format = c("SAS"),
dnn = c("Maternal Smoking Status", "Low Birthweight")) 

#4.3.1.1 Test of equality of variances
library(lawstat) #paket "lawstat" diperlukan
levene.test(lbw$BWT, lbw$RACE, location = "median", kruskal.test = F, 
correction.method = "none") #Uji homogenitas varians tiga kelompok dengan uji Levene 

#4.3.1.2 Unpaired or independent t test
levene.test(lbw$BWT, lbw$UI, location = "mean", kruskal.test = F, 
correction.method = "none") #Uji homogenitas varians dua kelompok dengan uji Levene

with(lbw,tapply(BWT, UI, mean))

with(lbw,tapply(BWT, UI, shapiro.test)) #Uji normalitas data untuk dua kelompok


t.test(lbw$BWT~lbw$UI, var.equal = T, conf.level = 0.95) #uji t tidak berpasangan dua arah
meandiff <- mean(lbw$BWT[lbw$UI=="No"])- mean(lbw$BWT[lbw$UI=="Yes"]) #mencari selisih rerata
meandiff

t.test(lbw$BWT~lbw$UI, var.equal = T, conf.level = 0.95, alternative="greater") #uji t tidak berpasangan satu arah
meandiff <- mean(lbw$BWT[lbw$UI=="No"])- mean(lbw$BWT[lbw$UI=="Yes"]) #mencari selisih rerata
meandiff

#4.3.1.3 Wilcoxon rank sum test 
wilcox.test(BWT~UI, data = lbw, paired = F, conf.level = 0.95,
conf.int = T, alternative = "two.sided") #Uji non-parametrik alternatif dari uji t tidak berpasangan

with(lbw,tapply(BWT, UI, summary))

#4.3.1.4 Chi-square test
#Chi square test(1)
CrossTable(lbw$SMOKE1, lbw$LBW1, prop.r = T, prop.c = F, prop.t = F,
prop.chisq = F, chisq = T, format = c("SAS"),
dnn = c("Maternal Smoking Status", "Low Birthweight")) 

#Chi square test(2)
smokinglbw <- matrix(c(31, 28, 43, 87), nrow = 2,
dimnames = list("Maternal Smoking Status" = c("Yes", "No"),
                "Low Birthweight" = c("Yes", "No")))
smokinglbw
chisq.test(smokinglbw, correct = F)
fisher.test(smokinglbw)

#4.3.1.5 Fisher’s exact test
#Subsetting, memilih yang kunjungan PNC "None" dan ras ibu "White"
fisher <- subset(lbw, FTV1!="None" & RACE=="White" ,) #Membuat objek baru "fisher" 

#Fisher exact test(1)
CrossTable(fisher$FTV1, fisher$LBW1, prop.r = T, prop.c = F, prop.t = F,expected = T,
prop.chisq = F, chisq=F, fisher = T, format = c("SPSS"),
dnn = c("Physician Visits", "Low Birthweight")) 

#Fisher exact test(2)
visitlbw <- matrix(c(10, 0, 15, 21,1,2), nrow = 3)
dimnames = list("Physician Visit" = c("One", "Two or more"),
                "Low Birthweight" = c("Yes", "No"))
visitlbw
fisher.test(visitlbw)

#4.3.2.1 Paired t test
#Impor dataset "sbp"
pair <- read.delim("D:/Data/Health-i/Dataset/sbp.txt")
View(pair)

diff <- pair$month1 - pair$baseline #selisih dari sbp
shapiro.test(diff) #Uji normalitas dari selisih
  
t.test(pair$month1, pair$baseline, paired = T, conf.level = 0.95) #uji t berpasangan dua arah
mean(pair$baseline);sd(pair$baseline)
mean(pair$month1);sd(pair$month1)

t.test(pair$month1, pair$baseline, paired = T, conf.level = 0.95, alternative = "less") 
#uji t berpasangan satu arah SBP month 1 < SBP baseline


#4.3.2.2 Wilcoxon signed rank test
5
#4.3.2.3 McNemar test
#Impor dataset "survival"
pair1 <- read.delim("D:/Data/Health-i/Dataset/survival.txt")

pair1$treatA <- factor(pair1$treatA, levels = c(1,2),labels = c("Survive", "Die")) #pemberian label
pair1$treatB <- factor(pair1$treatB, levels = c(1,2),labels = c("Survive", "Die")) #pemberian label

library(gmodels)
#McNemar test(1)
CrossTable(pair1$treatA, pair1$treatB, prop.r = F, prop.c = F, prop.t = F,
prop.chisq = F, mcnemar = T, format = c("SPSS"),
dnn = c("Outcome of treatment A", "Outcome of treatment B"))

#McNemar test(2)
treatment <- matrix(c(510, 5, 16, 90), nrow = 2,
dimnames = list("Outcome of treatment A" = c("Survive", "Die"),
                "Outcome of treatment B" = c("Survive", "Die")))
treatment
mcnemar.test(treatment)

#4.3.3.1 One-way ANOVA
#Error bar 
library(gplots) #paket "gplots" diperlukan
plotmeans(lbw$BWT~lbw$RACE, ylim = c(2200,3500), pch = 19,connect=F,text.n.label="n = ",	
xlab = "Race of the mothers",ylab = "Birthweight (grams)",
main = "Mean plot with 95% CI") 
abline(h = mean(lbw$BWT), lty = 2, col = "red")

library(lawstat)
levene.test(lbw$BWT, lbw$RACE, location = "mean", kruskal.test = F, 
correction.method = "none") #Uji homogenitas varians tiga kelompok dengan uji Levene

with(lbw,tapply(BWT,RACE,var)) #varians dari tiga kelompok

with(lbw,tapply(BWT,RACE,shapiro.test)) #Uji normalitas data untuk tiga kelompok

#One-Way ANOVA
oneway <- aov(BWT~RACE, data = lbw) 
summary(oneway) #Tabel ANOVA
TukeyHSD(oneway) #uji post hoc Tukey HSD
plot(TukeyHSD(oneway, "RACE"))

#4.3.3.2 Kruskal-Wallis test
library(dunn.test) #paket "dunn.test" diperlukan
dunn.test(lbw$BWT, lbw$RACE, kw = T, method = "bonferroni")

a = 0.05/3

diff12<- subset(lbw, RACE!="Other",) #membuat objek "diff12"
wilcox.test(BWT~RACE, data = diff12, paired = F, conf.level = a,
conf.int = T, alternative = "two.sided") #mencari selisih median antara ras kulit putih dan kulit hitam

diff13<- subset(lbw, RACE!="Black",) 
wilcox.test(BWT~RACE, data = diff13, paired = F, conf.level = a,
conf.int = T, alternative = "two.sided") #mencari selisih median antara ras kulit putih dan lainnya

diff23<- subset(lbw, RACE!="White",) 
wilcox.test(BWT~RACE, data = diff23, paired = F, conf.level = a,
conf.int = T, alternative = "two.sided") #mencari selisih median antara ras kulit hitam dan lainnya

#4.3.4 Correlation methods 
#4.3.4.1 Normally distributed variables
# Impor dataset "estriol"
slr <- read.delim("D:/Data/Health-i/Dataset/estriol.txt")
View(slr)

shapiro.test(slr$est) #uji normalitas variabel est
shapiro.test(slr$bwt) #uji normalitas variabel bwt

library(RVAideMemoire)
mshapiro.test(slr) #Uji normalitas data bivariate

#diagram pencar
plot(bwt~est,data=slr,xlab="Estriol (mg/24h)",pch=19,col="blue",
ylab="Birthweight (g)",main="Scatter plot of birthweight and estriol")

cor.test(slr$bwt,slr$est, method="spearman") #korelasi Spearman

library(RVAideMemoire)
spearman.ci(slr$bwt,slr$est,nrep=5000,conf.level=0.95)


#4.3.4.2 Not normally distributed variables
#Impor dataset "apgar"
spearman <- read.delim("D:/Data/Health-i/Dataset/apgar.txt")
View(spearman)

cor.test(spearman$apg1,spearman$apg5,method="spearman") # uji korelasi Spearman rank

library(RVAideMemoire)#paket"RVAideMemoire" diperlukan
spearman.ci(spearman$apg1,spearman$apg5,nrep=1000,conf.level=0.95) 

#4.3.5 Simple linear regression
#diagram pencar
plot(bwt~est,data=slr,xlab="Estriol (mg/24h)",pch=19,col="blue",
ylab="Birthweight (g)",main="Scatter plot of birthweight and estriol")
abline(lm(bwt~est,data=slr),col="red")

lreg <- lm(bwt ~ est, data = slr)#membuat objek "lreg"
anova(lreg) #tabel ANOVA
summary(lreg) #nilai estimasi parameter/koefisien regresi

## Silakan mempelajari sendiri kode-kode di bawah ini dibantu dengan panduan yang telah diberikan
#Diagnostics plot
res <- residuals(lreg) #Residuals
std.res <- rstandard(lreg) #Standardized residuals 
stud.res <- rstudent(lreg) #Studentized deleted residuals
pred.bwt <- predict(lreg) #Fitted value
x <- slr$est

plot(lreg,which=c(2)) #QQ-plot residuals

plot(x,stud.res,pch=19,col="red",main="Residuals vs Predictor",
xlab="Estriol (mg/24h)",ylab="Studentized deleted residuals")
abline(h=0)

plot(pred.bwt,stud.res,pch=19,col="red",main="Residuals vs Predicted",
xlab="Predicted birtweight (g)",ylab="Studentized deleted residuals")
abline(h=0)

#Formal test
shapiro.test(res)
library(car)
outlierTest(lreg)
library(lmtest)
harvtest(bwt~est,order.by=~est,data=slr)
bptest(bwt~est,studentize=F,data=slr)

library(MASS)
boxcox(bwt~est,data=slr,lambda = seq(-3,3, length = 10))

slr$bwt1 <- log(slr$bwt) #Transformasi variabel terikat
lreg1 <- lm(bwt1~est,data=slr)
summary(lreg1)

res1 <- residuals(lreg1) #Residuals
std.res1 <- rstandard(lreg1) #Standardized residuals 
stud.res1 <- rstudent(lreg1) #Studentized deleted residuals
pred.bwt1 <- predict(lreg1) #Fitted value
x <- slr$est

plot(lreg1,which=c(2)) #QQ-plot residuals

plot(x,stud.res1,pch=19,col="red",main="Residuals vs Predictor",
xlab="Estriol (mg/24h)",ylab="Studentized deleted residuals")
abline(h=0)

plot(pred.bwt1,stud.res1,pch=19,col="red",main="Residuals vs Predicted",
xlab="Log predicted birtweight (g)",ylab="Studentized deleted residuals")
abline(h=0)

#Formal test
shapiro.test(res1)
library(car)
outlierTest(lreg1)
library(lmtest)
harvtest(bwt1~est,order.by=~est,data=slr)
bptest(bwt1~est,studentize=F,data=slr)


lbw$RACE1[lbw$RACE=="Black"] <- 1
lbw$RACE1[lbw$RACE!="Black"] <- 0

lbw$RACE2[lbw$RACE=="Other"] <- 1
lbw$RACE2[lbw$RACE!="Other"] <- 0

lreg <- lm(BWT~RACE1+RACE2,data=lbw)
summary(lreg)
confint(lreg)

mean(lbw$BWT[lbw$RACE==1])
mean(lbw$BWT[lbw$RACE==2])
mean(lbw$BWT[lbw$RACE==3])


#Regresi Logistik
#konsep odd
library(readxl)
odd <- read_excel("D:/Data/MKWP MAD/odd dan log odd.xlsx")
View(odd)

hist(odd$log_odd)
shapiro.test(odd$log_odd)

odd$probit <- qnorm(odd$p)

hist(odd$probit)x <- c(0,1)
y <- c(-6,6)
plot(x, y, type ="n",
     ylim = c(-6,6),
     bty = "L",
     ylab = "Transfomasi",
     xlab = "Proporsi")
lines(odd$log_odd ~ odd$p, type = "p", col = "blue")
lines(odd$probit ~ odd$p, type = "p", col = "red")
abline(v = 0.5, h = 0)
shapiro.test(odd$probit)



lbw <- read_csv("D:/Data/MKWP MAD/Dataset/lbw.csv")
View(lbw)

lbw <- read.csv("D:/Data/Health-i/Dataset/lbw.csv") #Ubah menjadi slash D:\Data\Health-i\Dataset
View(lbw)

lbw$LBW <- NA
lbw$LBW[lbw$BWT<2500]  <- 1
lbw$LBW[lbw$BWT>=2500] <- 0
lbw$LBW <- factor(lbw$LBW,levels = c(0,1),labels = c("No", "Yes"))

lbw$RACE <- factor(lbw$RACE,levels = c(1,2,3), labels = c("White", "Black", "Other"))
lbw$SMOKE <- factor(lbw$SMOKE,levels = c(0,1),labels = c("No", "Yes"))

lbw$PTL1 <- NA
lbw$PTL1[lbw$PTL==0] <- 0
lbw$PTL1[lbw$PTL>=1] <- 1
lbw$PTL1 <- factor(lbw$PTL1,levels = c(0,1), labels = c("None", "One or more"))

lbw$HT <- factor(lbw$HT,levels = c(0,1),labels = c("No", "Yes"))
lbw$UI <- factor(lbw$UI,levels = c(0,1),labels = c("No", "Yes"))

lbw$FTV1 <- NA
lbw$FTV1[lbw$FTV==0] <- 0
lbw$FTV1[lbw$FTV==1] <- 1
lbw$FTV1[lbw$FTV>=2] <- 2
lbw$FTV1 <- factor(lbw$FTV1,levels = c(0,1,2),labels = c("None", "One", "Two or more"))

View(lbw)

uniage <- glm(LBW ~ AGE, data = lbw, binomial)
summary(uniage)
anova(uniage)

exp(cbind(OR=coef(uniage), confint(uniage)))

unilwt <- glm(LBW~LWT,binomial,data=lbw)
summary(unilwt)
exp(cbind(OR=coef(unilwt), confint(unilwt)))

unismoke <- glm(LBW~SMOKE,binomial,data=lbw)
summary(unismoke)
anova(unismoke)

exp(cbind(OR=coef(unismoke), confint(unismoke)))
anova(unismoke)

library(epitools)
epitab(lbw$RACE, lbw$LBW, method = "oddsratio",
conf.level = 0.95, pvalue = "chi2")

epitab(lbw$SMOKE, lbw$LBW, method = "oddsratio",
conf.level = 0.95, pvalue = "chi2")

epitab(lbw$PTL1, lbw$LBW, method = "oddsratio",
conf.level = 0.95, pvalue = "chi2")

epitab(lbw$HT, lbw$LBW, method = "oddsratio",
conf.level = 0.95, pvalue = "fisher.exact")

epitab(lbw$UI, lbw$LBW, method = "oddsratio",
conf.level = 0.95, pvalue = "chi2")

epitab(lbw$FTV1, lbw$LBW, method = "oddsratio",
rev = "rows", conf.level = 0.95, pvalue = "chi2")

uniftv <- glm(LBW~FTV1,binomial,data=lbw)
summary(uniftv)
exp(cbind(OR=coef(uniftv), confint(uniftv)))

logreg1 <- glm(LBW~LWT+SMOKE+FTV,binomial,data=lbw)
summary(logreg1)
logLik(logreg1)

logreg2 <- glm(LBW ~ SMOKE + FTV, binomial, data = lbw)
summary(logreg2)
logLik(logreg2)

G = -2*(-111.8396-(-111.3962))
dchisq(G, df=1, ncp = 0, log = FALSE)


b1 = 0.757502 #Effect of SMOKE with LWT
b2 = -0.347246 #Effect of FTV with LWT

B1 = 0.7688 #Effect of SMOKE without LWT
B2 =  -0.3582 #Effect of FTV without LWT

((B1-b1)/b1)*100 #Delta for SMOKE
((B2-b2)/b2)*100 #Delta for FTV

logreg3 <- glm(LBW~AGE+SMOKE+FTV,binomial,data=lbw)
summary(logreg3)
logLik(logreg3)

logreg4 <- glm(LBW~RACE+SMOKE+FTV,binomial,data=lbw)
summary(logreg4)
logLik(logreg4)

logreg5 <- glm(LBW~PTL1+SMOKE+FTV,binomial,data=lbw)
summary(logreg5)
logLik(logreg5)

logreg6 <- glm(LBW~HT+SMOKE+FTV,binomial,data=lbw)
summary(logreg6)
logLik(logreg6)

logreg7 <- glm(LBW~UI+SMOKE+FTV,binomial,data=lbw)
summary(logreg7)
logLik(logreg7)

logreg8 <- glm(LBW~FTV,binomial,data=lbw)
x <- lbw$FTV
y <- predict(logreg8)

plot(x,y,type="n",main="Linearity assumption",
xlab="Physician visits during first trimester",
ylab="Estimated log-odds of low birthweight")
lines(lowess(x,y),type="b")

logreg9 <- glm(LBW~SMOKE*FTV,binomial,data=lbw)
summary(logreg9)






