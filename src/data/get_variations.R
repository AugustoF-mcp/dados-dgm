
get_time_var <- function(data, idgm, type) {
  
  # packages -----
  require(tidyverse)
  
  
  
  # assert -----
  assertthat::assert_that(is.character("idgm"), msg = 'Defina o IDGM')
  assertthat::assert_that(is.character("type"), msg = 'Defina o intervalo de variação')
  
  
  # variation interval
  if (type == 'decada') {
    years <- c(max(data$ANO), min(data$ANO))
  } else {
    years <- c(max(data$ANO), max(data$ANO)-1)
  }
  
  
  
  # output ----
  data %>%
    dplyr::select(COD, ANO, {{idgm}}) %>%
    dplyr::filter(ANO %in% years) %>%
    tidyr::pivot_wider(id_cols = COD,
                       names_from = ANO,
                       values_from = -c(COD,ANO)) %>%
    dplyr::rename(INI = 2,
                  END = 3) %>%
    dplyr::mutate(VAR = (END/INI-1)*100) %>%
    dplyr::select(COD, VAR)
  
}




get_position_var <- function(data, idgm, type) {
  
  # packages -----
  require(tidyverse)
  
  
  
  # assert -----
  assertthat::assert_that(is.character("idgm"), msg = 'Defina o IDGM')
  assertthat::assert_that(is.character("type"), msg = 'Defina o intervalo de variação')
  
  
  # variation interval
  if (type == 'decada') {
    years <- c(max(data$ANO), min(data$ANO))
  } else {
    years <- c(max(data$ANO), max(data$ANO)-1)
  }
  
  
  
  # output ----
  data %>%
    dplyr::select(COD, ANO, {{idgm}}) %>%
    dplyr::filter(ANO %in% years) %>%
    tidyr::pivot_wider(id_cols = COD,
                       names_from = ANO,
                       values_from = -c(COD,ANO)) %>%
    dplyr::rename(INI = 2,
                  END = 3) %>%
    dplyr::mutate(POS_INI = dplyr::row_number(INI),
                  POS_END = dplyr::row_number(END),
                  VAR = END-INI) %>%
    dplyr::select(COD, VAR)
  
}

