# ----
# Function for adding mean and CV to hybrids x locations tables
# Agustin Alesso
# ----

create_tbl_genxloc <- function(
    genotypes,           # genotypes column
    locations,           # locations column
    response,            # yield column
    data,                # dataframe
    path = NULL,         # Save to excel file
    caption = NULL,      # Table caption
    dec = 0
) {
  
  require(tidyverse)
  require(gt)
  require(rio)
  
  cv <- function(X, ...) {sd(X, na.rm = T)/abs(mean(X, na.rm = T))*100}
  
  genotypes <- enquo(genotypes)
  locations <- enquo(locations)
  response <- enquo(response)
  
  # make variable selection
  tbl <- data %>% 
    select(!!genotypes, !!locations, !!response)
  
  # Relative responses
  tbl <- tbl %>% 
    group_by(!!locations) %>% 
    mutate(
      rel = round(!!response/mean(!!response, na.rm = T)*100)
    )
  
  # Genotype averages and CV
  avg_geno <- tbl %>%
    group_by(!!genotypes) %>%
    summarise(
      !!locations := "Promedio",
      across(c(!!response, rel), mean, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(!!genotypes := fct_reorder(!!genotypes, rel, mean, .desc = T))
  
  cv_geno <- tbl %>% 
    group_by(!!genotypes) %>%
    summarise(
      across(c(!!response, rel), cv, na.rm =T)
    ) %>%
    ungroup() %>%
    mutate(
      across(where(is.double), round),
      CV =  paste0(formatC(rel, flag = "0", width = 2), " (", !!response, ")")
    ) %>% 
    select(!!genotypes, CV)
  
  # Locations averages and CV
  avg_loc <- tbl %>%
    group_by(!!locations) %>%
    summarise(
      !!genotypes := "Promedio",
      across(c(!!response, rel), mean, na.rm = T),
    )
  
  # Table with margins
  tbl <- bind_rows(tbl, avg_geno, avg_loc) %>%
    mutate(
      across(where(is.double), round),
      resp_rel = paste0(formatC(rel, flag = "0", width = 3), " (", !!response, ")")
    )
  
  # Save cross table ----
  if(!is.null(path)) {
    
    tbl_abs <- tbl %>% 
      pivot_wider(  
        id_cols = !!genotypes,
        names_from = !!locations,
        values_from = !!response
      ) %>% 
      arrange(desc(Promedio))
    
    tbl_rel <- tbl %>% 
      pivot_wider(  
        id_cols = !!genotypes,
        names_from = !!locations,
        values_from = rel
      ) %>% 
      arrange(desc(Promedio))
    
    tbl_lst <- list(
      "Absolutos" = tbl_abs,
      "Relativos" = tbl_rel
    )
    rio::export(tbl_lst, file = path)
  }
  
  # Cross table for gt table -----
  tbl <- tbl %>% 
    pivot_wider(  
      id_cols = !!genotypes,
      names_from = !!locations,
      values_from = resp_rel
    ) %>% 
    left_join(x = ., y = cv_geno, by = as_label(genotypes)) %>% 
    arrange(desc(Promedio))
  
  # Colorify
  pal <- RColorBrewer::brewer.pal(n = 11, name = "RdYlGn")
  tbl <- tbl %>%
    gt(rowname_col = as_label(genotypes), rownames_to_stub = F, caption = caption) %>%
    data_color(
      columns = -c(!!genotypes, CV),
      colors = scales::col_factor(pal, domain = NULL)
      #colors = ~ col_tmp(as.numeric(str_extract(!!resolved_column, "(?<=\\().+?(?=\\))")))
    ) %>%
    data_color(
      columns = CV,
      colors = scales::col_factor(pal, domain = NULL, reverse = T),
      #sacol_tmp(tmp %>% filter(Localidad == "Promedio") %>% pull(rel) %>% sort(., decreasing = F))
    ) %>%
    cols_align(
      align = "right",
      columns = -!!genotypes
    ) %>%
    sub_missing(everything()) %>%
    text_transform(
      locations = cells_body(columns = -c(!!genotypes)),
      fn = function(x) {
        str_remove(x, "^0+")
      }
    )
  
  # Add column spanner and stubhead
  tbl <- tbl %>%
    tab_spanner(label = as_label(locations), columns= -c(!!genotypes, Promedio, CV))
  
  # Sticky headers
  # tbl <- tbl |>
  #   tab_options(
  #     container.overflow.y = T,
  #     container.height = px(400)
  #   ) |> 
  #   tab_style(
  #     style = css(position = "sticky", top = 0),
  #     locations = cells_column_labels()
  #   )
  
  # Return gt table
  tbl
  # avg_geno
  # cv_geno
  # avg_loc

}
