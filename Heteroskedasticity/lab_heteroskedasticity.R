

# Humberto Martínez García
# hmartinez@colmex.mx
# Licence Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)

# ----------------------------------
# Topic: Heteroskedsaticity.
# ----------------------------------

# Obtener la ruta de trabajo actual
getwd()

# Para fijar el directorio de trabajo deben usar el código de abajo. 
# Utilicen el directorio de la carpeta con su nombre

setwd("C://Users/Humberto Martínez/Documents/GitHub/Lab_Econ2_2018/Heteroskedasticity")

# Verificar que el directorio se fijó correctamente
getwd()

# Para guardar el script sólo es: ctrl+s

# ---------------------------------------------------------
# Dataframes & plots

#Instalando los paquetes que usaremos
install.packages("wooldridge")



# start with an empty workspace
rm(list=ls())

# load necessary packages for importing the function
library(RCurl)

# import the function
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

# Cargamos la librería y la base de datos
library("wooldridge")


# Example 8.1
data(wage1)
head(wage1)
help(wage1)

wage1$marrmale = 0
wage1$marrfem = 0
wage1$singmale = 0
wage1$singfem = 0

wage1$marrmale[wage1$married==1 & wage1$female==0] = 1
wage1$marrfem[wage1$married==1 & wage1$female==1] = 1
wage1$singmale[wage1$married==0 & wage1$female==0] = 1
wage1$singfem[wage1$married==0 & wage1$female==1] = 1

# estimate simple linear model
reg <- lm(lwage ~ marrmale+marrfem+ singfem + educ+exper+expersq + tenure + tenursq, 
          data=wage1)
# use new summary function
summary(reg)
summary(reg,robust = T)

# save robust standard errors
robust_se <- as.vector(summary(reg,robust = T)$coefficients[,"Std. Error"])

install.packages("stargazer")
library("stargazer", lib.loc="~/R/win-library/3.4")
# print stargazer output with robust standard errors
stargazer(reg,type = "text",se = list(robust_se))



#### Example 8.2

data(gpa3)
head(gpa3)
help(gpa3)

# estimate simple linear model
unrestricted <- lm(cumgpa ~ sat+hsperc+ tothrs + female +black+white, 
          data=gpa3,subset=spring==1)
restricted <- lm(cumgpa ~ sat+hsperc+ tothrs + female, 
                   data=gpa3,subset=spring==1)
# use new summary function
summary(reg)
summary(reg,robust = T)

# to obtain a F test 
#install.packages("lmtest")
#install.packages("plm")
library("lmtest", lib.loc="~/R/win-library/3.4")
library("plm", lib.loc="~/R/win-library/3.4")

help(vcovHC)
#"HC0") gives White's estimator, the other estimators are refinements of this

waldtest(restricted, unrestricted, vcov=vcovHC(unrestricted))
# We fail to reject the null hypothesis using either test


#### Example 8.3
data(crime1)
head(crime1)
help(crime1)

avgsensq = crime1$avgsen^2

lm.8.3 = lm(narr86 ~ pcnv + avgsen + avgsensq + ptime86 + qemp86 +
             inc86 + black + hispan, data=crime1)
summary(lm.8.3)
coeftest(lm.8.3, vcov=vcovHC(lm.8.3,type='HC0'))

### Example 8.4
data(hprice1)
head(hprice1)
help(hprice1)

# estimate simple linear model
reg <- lm(price ~ lotsize+sqrft+ bdrms, 
          data=hprice1)

# Testing for heteroskedasticity
# via F statistic
hprice1$resid = (residuals(reg))^2
regF = lm(resid ~ lotsize+sqrft+ bdrms, 
          data=hprice1)
summary(regF)

# via "LM" statistic, or Breusch-Pagan Test
bptest(reg, ~ lotsize+sqrft+ bdrms, 
       data=hprice1)
########################################################################3
# below based on https://econometricswithr.wordpress.com/wooldridge-2013/chapter-8/
#8.5
data(hprice1)
head(hprice1)
help(hprice1)

reg5 = lm(lprice ~ llotsize + lsqrft + bdrms, data=hprice1)
ressq = reg5$residuals^2
fitted = reg5$fitted.values
fitted_sq = fitted^2
reg_res = lm(ressq ~ fitted + fitted_sq)
rsq = summary(reg_res)$r.squared

#NOTA: en este caso (especial de White test) no funciona el comando bptest
# with F
summary(reg_res)

# with LM
rsq*88
1-pchisq(3.447286,2)

#######################       #####  #8.6
data(k401ksubs)
head(k401ksubs)
help(k401ksubs)

ksubs = k401ksubs


lm.8.6.1<-lm(nettfa ~ inc, data=ksubs, subset=(fsize==1))
coeftest(lm.8.6.1, vcov=vcovHC(lm.8.6.1,type='HC0'))

lm.8.6.2<-lm(nettfa ~ inc, weights=1/inc, data=ksubs, subset=(fsize==1))
summary(lm.8.6.2)

age.25sq<-(ksubs$age-25)^2
lm.8.6.3<-lm(nettfa ~ age.25sq + male + e401k, data=ksubs, subset=(fsize==1))
coeftest(lm.8.6.3, vcov=vcovHC(lm.8.6.3,type='HC0'))

lm.8.6.4<-lm(nettfa ~ inc + age.25sq + male + e401k, 
             weights=1/inc, data=ksubs, subset=(fsize==1))
summary(lm.8.6.4)

##############################################    8.7
data(smoke)
head(smoke)
help(smoke)

lm.8.7<-lm(cigs ~ lincome + lcigpric + educ + age + agesq + restaurn, data=smoke)
summary(lm.8.7)

# Breusch-Pagan
summary(
  lm.8.7<-lm(
    lm.8.7$residuals^2 ~ lincome + lcigpric + educ + 
      age + agesq + restaurn,data=smoke
  )
)
summary(lm.8.7)$r.squared*807
1-pchisq(summary(lm.8.7)$r.squared*807,6)

# Recall the first column of the table
summary(lm.8.6.4)
# Column 2
coeftest(lm.8.6.4, vcov=vcovHC(lm.8.6.4,type='HC0'))