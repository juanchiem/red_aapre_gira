library(tidyverse) 
# library(ggpmisc)
# library(ggstance)
library(huxtable)
# library(patchwork)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(multcomp)
library(performance)
library(rio)
library(skimr)

conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::recode)
conflicted::conflicts_prefer(lme4::lmer)
conflicted::conflicts_prefer(broom::tidy)
theme_set(theme_bw(base_size = 12)) 

hux <- function(data, x) {
  data %>%
    as_hux() %>% 
    theme_article() %>% 
    set_font_size(9) %>%    
    set_top_padding(.2) %>%
    set_bottom_padding(.2) %>% 
    set_number_format(x)
}
# quarto::quarto_render("document.qmd") # defaults to html
# usethis::browse_github()
# tinytex::install_tinytex(repository = "http://mirrors.tuna.tsinghua.edu.cn/CTAN/", version = "latest")
