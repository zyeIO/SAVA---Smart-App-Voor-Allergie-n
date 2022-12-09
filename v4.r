#install.packages('haven')
library(haven)
#install.packages('ltm')
library(ltm)

surveydata = read_sav("app1.sav")
surveydata2 = read_sav("app2.sav")


#install.packages('tidyverse')
library(tidyverse)
data_app1 <- (surveydata %>% select(18:43))
data_app1 <- na.omit(data_app1)

library(tidyverse)
data_app2 <- (surveydata2 %>% select(17:42))
data_app2 <- na.omit(data_app2)

names(data_app1) = c("v1.1","v1.2","v1.3","v2.1","v2.2","v2.3","v2.4","v3.1","v3.2","v3.3","v3.4","v3.5","v4","v5.1","v5.2","v5.3","v6.1","v6.2","v6.3","v7","v8","v9","10.1","v10.2","v10.3","v11")
names(data_app2) = c("v1.1","v1.2","v1.3","v2.1","v2.2","v2.3","v2.4","v3.1","v3.2","v3.3","v3.4","v3.5","v4","v5.1","v5.2","v5.3","v6.1","v6.2","v6.3","v7","v8","v9","10.1","v10.2","v10.3","v11")


#----------------------------------------------------------------------
#validity test, herhaalbaarheid
cronbach.alpha(data_app1)
cronbach.alpha(data_app2)

#----------------------------------------------------------------------
#shapiro
shapiro <- apply(data_app1,2,shapiro.test)
unlist(lapply(shapiro, function(x) x$p.value))

shapiro <- apply(data_app2,2,shapiro.test)
unlist(lapply(shapiro, function(x) x$p.value))


#----------------------------------------------------------------------
#Kolmogorov
library('nortest')
dflillie <- apply(data_app1,2,lillie.test)
unlist(lapply(dflillie, function(x) x$p.value))

dflillie <- apply(data_app2,2,lillie.test)
unlist(lapply(dflillie, function(x) x$p.value))


#----------------------------------------------------------------------
data_app1$som = ((rowSums(data_app1[1:26]))/(26*9))*100
data_app2$som = ((rowSums(data_app2[1:26]))/(26*9))*100


#----------------------------------------------------------------------
#shapiro2.0
shapiro.test(data_app1$som)
shapiro.test(data_app2$som)
#kolmogorov-smoriv
lillie.test(data_app1$som)
lillie.test(data_app2$som)


#----------------------------------------------------------------------
df_list <- rbind(data_app1, data_app2)

df_list$design = c("d1","d1","d1","d1","d1","d1","d1","d1","d1","d1","d1","d1","d2","d2","d2","d2","d2","d2","d2","d2","d2","d2","d2","d2","d2","d2","d2","d2","d2")


#----------------------------------------------------------------------
library(car)
leveneTest(df_list$som ~ design, data = df_list)



#----------------------------------------------------------------------
t.test(df_list$som ~ design, data = df_list, var.equal = TRUE)


#----------------------------------------------------------------------
#plot

install.packages('ggplot')
library('ggplot2')
ggplot(df_list, aes(x=som , y=design)) + geom_boxplot() + coord_flip()  + geom_jitter(shape=16, position=position_jitter(0.2)) +
  ggtitle("Boxplot per design")

