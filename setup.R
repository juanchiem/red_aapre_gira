library(tidyverse) 
library(ggpmisc)
library(ggstance)
library(huxtable)
# library(patchwork)
library(lme4)
library(nlme)
library(emmeans)
library(multcomp)
library(performance)

conflicted::conflicts_prefer(ggplot2::annotate)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflict_prefer("select", "dplyr")

theme_set(theme_bw(base_size = 12)) 
# knitr::opts_chunk$set(echo = F, warning = FALSE, message = FALSE)

# asin_tran <- make.tran("asin.sqrt", 100)

# quarto::quarto_render("document.qmd") # defaults to html
# usethis::browse_github()
# tinytex::install_tinytex(repository = "http://mirrors.tuna.tsinghua.edu.cn/CTAN/", version = "latest")

# library(gistr)
# gist_auth()
# gists('minepublic', per_page = 2)

# inputs
# rinde_humedo <- 5000  # Rendimiento de grano en kg/ha
# humedad <- 8
# hum_recibo <- 11
# porcentaje_aceite <- 45  # Porcentaje de aceite
# bonif <- 42
# 
# # agua <- rinde_humedo*humedad/100
# rinde_seco <- rinde_humedo - (rinde_humedo*humedad/100)
# rinde_bonif <- rinde_seco * (1 + (porcentaje_aceite - 42) * 2/100)
# rinde_bonif_recibo <-  rinde_bonif*(1+hum_recibo/100)
# rinde_bonif_recibo
# # 
