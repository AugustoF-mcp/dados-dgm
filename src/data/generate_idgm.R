
generate_idgm <- function(data) {
  
  # packages -----
  require(tidyverse)
  
  
  
  # assert -----
  assertthat::assert_that(exists("dgm_ind"), msg = 'Defina os indicadores do DGM')
  assertthat::assert_that(exists("ind_attr"), msg = 'Carregue os parâmetros dos indicadores do DGM')
  assertthat::assert_that(exists("area_weights"), msg = 'Carregue os pesos das áreas do DGM')
  
  
  
  # data scaling -----
  pad_data <- data %>%
    magrittr::extract(dgm_ind) %>%
    purrr::imap(~ tibble::add_column(.x, !!!(ind_attr %>%
                                               dplyr::filter(NOME == .y) %>%
                                               dplyr::select(MIN,MAX,PESO,SINAL,AREA))) %>%
                  dplyr::mutate(PAD_TX = ifelse(SINAL == -1,
                                                (TX - MIN)/(MIN - MAX),
                                                (TX - MIN)/(MAX - MIN)
                  ),
                  IND = .y,
                  ANO = ANO %>% as.numeric()) %>%
                  dplyr::filter(ANO >= ind_attr %>%
                                  dplyr::filter(NOME == .y) %>%
                                  dplyr::select(INI) %>%
                                  as.numeric(),
                                ANO <= ind_attr %>%
                                  dplyr::filter(NOME == .y) %>%
                                  dplyr::select(FIM) %>%
                                  as.numeric())
    ) %>%
    dplyr::bind_rows()
  
  
  
  # idgm -----
  dgm_area <- pad_data %>%
    dplyr::group_by(COD,ANO,AREA) %>%
    dplyr::summarise(IDGM = weighted.mean(PAD_TX, w = PESO, na.rm = T)) # conferir NA's no caso do SNNIS
  
  sinthetic <- dgm_area %>%
    dplyr::left_join(area_weights) %>%
    dplyr::group_by(COD,ANO) %>%
    dplyr::summarise(IDGM = weighted.mean(IDGM, w = PESO, na.rm = T)) # conferir NA's no caso do SNNIS
  
  
  
  # output -----
  dgm_area %>% 
    tidyr::pivot_wider(id_cols = c('COD','ANO'),
                       names_from = 'AREA', 
                       values_from = 'IDGM') %>%
    dplyr::left_join(sinthetic) %>%
    dplyr::ungroup()
  
}
