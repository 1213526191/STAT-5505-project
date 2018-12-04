library(tidyverse)
library(vcd)
library(MASS)

load("../data/Crime.RData")


# Figure 1 ----------------------------------------------------------------

age <- crime %>%
  dplyr::select(Victim.Age)

pdf("../image/1.pdf", width = 10, height=6.18)

ggplot(age) +
  geom_histogram(aes(Victim.Age), color = "grey20", bins = 30)

dev.off()

# Figure 2 ----------------------------------------------------------------

temp <- crime$Time.Occurred
temp2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(temp))
temp <- paste0(temp2, temp)
e <- strptime(temp, format="%H%M")

# Break the time in to hourly intervals

sys_date<-as.character(Sys.Date())
u <- table(cut(e, breaks=seq(as.POSIXlt(paste(sys_date,"01:00:00")), as.POSIXlt(paste(sys_date,"24:00:00")),"hours")))
interval <- as.character(matrix(unlist(strsplit(names(u)," ")), ncol=2, byrow=TRUE)[,2])
freq=as.vector(u)
df <- as.data.frame(cbind(interval, freq))

# Basic barplot

pdf("../image/2.pdf", width = 10, height=6.18)
p<-ggplot(data=df, aes(x=interval, y=freq)) +
  geom_bar(stat="identity")
p + coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

# Figure 3 ----------------------------------------------------------------

age_count <- age %>%
  group_by(Victim.Age) %>%
  summarise(freq = n()) %>%
  rename(counts = Victim.Age) %>%
  mutate(counts = counts) %>%
  dplyr::select(freq, counts)
pdf("../image/3.pdf", width = 10, height=6.18)
distplot(as.matrix(age_count), type = c("poisson"))
dev.off()

# Test 1 ------------------------------------------------------------------


freq = c(rep(0, 10), age_count$freq)
lambda = sum(age_count$counts*age_count$freq)/sum(age_count$freq)
age_max = max(age_count$counts)
p = dpois(c(0:age_max), lambda)
N = sum(age_count$freq)
chisq.test(freq, N*p)


# Test 2 ------------------------------------------------------------------

x <- freq
p_chi <- function(x,y){
  a=sum(((x-mean(x))^2)/mean(x))
  return(a)
}
p_chi(x,y)
1-pchisq(p_chi(x,y), df=23)


# Figure 4 ----------------------------------------------------------------

crime$Month_Yr=format(as.Date(crime$Date.Occurred,"%m/%d/%Y"),"%Y-%m")
area1=subset(crime,Area.ID==12)
area1$Month_Yr=as.factor(area1$Month_Yr)
area1.mon.ct=as.data.frame(table(area1$Month_Yr))
n=dim(area1.mon.ct)[1]
area1.mon.ct$Freq=as.factor(area1.mon.ct$Freq)
k=as.data.frame(table(area1.mon.ct$Freq))
k$Var1=as.character(k$Var1)
k$Var1=as.integer(k$Var1)

k1 = as.tibble(k)
pdf("../image/4.pdf", width = 10, height=6.18)
ggplot(k1) +
  geom_histogram(aes(Var1), color = "grey20")
dev.off()

# Figure 5 ----------------------------------------------------------------

k=k[-1,]

pdf("../image/5.pdf", width = 10, height=6.18)
distplot(k[,c(2,1)], type = c("poisson"),
         conf_int=FALSE, conf_level=FALSE,
         main = "Poissonness plot of Area 12")
dev.off()



