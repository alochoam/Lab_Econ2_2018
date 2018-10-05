

# Humberto Martínez García
# hmartinez@colmex.mx
# Licence Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)

# ----------------------------------
# Tercera sesión de laboratorio en R
# ----------------------------------

# Obtener la ruta de trabajo actual
getwd()

# Para fijar el directorio de trabajo deben usar el código de abajo. 
# Utilicen el directorio de la carpeta con su nombre

setwd("C://Users/Humberto Martínez/Documents/GitHub/Lab_Econ2_2018/Heteroskedasticity")

# Verificar que el directorio se fijó correctamente
getwd()

# Para guardar el script sólo es: ctrl+s

# -------------------------------------------------------------------------

# En esta sesión

# Data frames
# Plots

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
reg <- lm(cumgpa ~ sat+hsperc+ tothrs + female +black+white, 
          data=gpa3,subset=spring==1)
# use new summary function
summary(reg)
summary(reg,robust = T)

#### Example 8.3