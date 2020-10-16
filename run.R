

# packages and functions -----
library(tidyverse)
list.files('src/', pattern = "*.R", full.names = T, recursive = T) %>% 
  purrr::map(~source(.x, encoding = 'utf-8'))



# import absolute data -----
load('dados/input/rdata/cities_indicators.RData')

idgm <- generate_idgm(city_ind)


rankings <- c("Infraestrutura", "Saúde", "Segurança", "Educação", "IDGM") %>% 
  purrr::imap(~idgm %>%
                dplyr::select(COD,{{.y}}) %>%
                
                dplyr::left_join(
                  get_time_var(data = idgm, 
                                   area = .x, 
                                   type = "ano")) %>%
                dplyr::rename(Ano = VAR) %>%
                
                dplyr::left_join(
                  get_time_var(data = idgm, 
                                   area = .x, 
                                   type = "decada")) %>%
                dplyr::rename(`Década` = VAR)
              
  )

advances <- c("Infraestrutura", "Saúde", "Segurança", "Educação", "IDGM") %>% 
  purrr::imap(~idgm %>%
                dplyr::select(COD,{{.y}}) %>%
                
                dplyr::left_join(
                  get_position_var(data = idgm, 
                                   area = .x, 
                                   type = "ano")) %>%
                dplyr::rename(Ano = VAR) %>%
                
                dplyr::left_join(
                  get_position_var(data = idgm, 
                                   area = .x, 
                                   type = "decada")) %>%
                dplyr::rename(`Década` = VAR)
              
  )
