gen_text_ideb_ai <- function(data) {
  
}

gen_text_ideb_af <- function(data) {
  
}

gen_text_creche <- function(data) {
  
}

gen_text_pre_escola <- function(data) {
  
}

gen_text_rdo <- function(data) {
  
}

gen_text_agua <- function(data) {
  
}

gen_text_esgoto <- function(data) {
  
}

gen_text_esgoto_tratado <- function(data) {
  
}

gen_text_agua_perdida <- function(data) {
  
}

gen_text_mort_infantil <- function(data) {
  
}

gen_text_pre_natal <- function(data) {
  
}

gen_text_cobertura_ab <- function(data) {
  
}

gen_text_dcnt <- function(data) {
  
}

gen_text_homicidio <- function(data) {
  ind <- 'TX_DATASUS_HOMICIDIO_UF'
  
  ini <- data_dict %>%
    dplyr::filter(Nome == glue::glue('AML_{ind}')) %>%
    dplyr::select(`Início`) %>%
    dplyr::pull()
  
  end <- data_dict %>%
    dplyr::filter(Nome == glue::glue('AML_{ind}')) %>%
    dplyr::select(Fim) %>%
    dplyr::pull()
  
  
  data <- data %>%
    purrr::map(~.x %>%
                 magrittr::extract2(ind)) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(ANO >= ini,
                  ANO <= end)
  
  
  # inputs
  tx_state <- data %>%
    dplyr::filter(ANO == end, COD == coduf) %>%
    dplyr::select(TX) %>%
    dplyr::pull()
  
  tx_aml <- data %>%
    dplyr::filter(ANO == end, COD == 6) %>%
    dplyr::select(TX) %>%
    dplyr::pull()
  
  tx_br <- data %>%
    dplyr::filter(ANO == end, COD == 0) %>%
    dplyr::select(TX) %>%
    dplyr::pull()
  
  tx_state_old <- data %>%
    dplyr::filter(ANO == ini, COD == coduf) %>%
    dplyr::select(TX) %>%
    dplyr::pull()
  
  tx_aml_old <- data %>%
    dplyr::filter(ANO == ini, COD == 6) %>%
    dplyr::select(TX) %>%
    dplyr::pull()
  
  tx_br_old <- data %>%
    dplyr::filter(ANO == ini, COD == 0) %>%
    dplyr::select(TX) %>%
    dplyr::pull()
  
  var_state <- (tx_state/tx_state_old-1)*100
  var_aml <- (tx_aml/tx_aml_old-1)*100
  var_br <- (tx_br/tx_br_old-1)*100
  
  abs <- state_data %>%
    magrittr::extract2('DATASUS_HOMICIDIO') %>%
    dplyr::filter(COD == coduf, ANO == end) %>%
    dplyr::select(HOMICIDIO) %>%
    dplyr::pull()
  
  abs_old <- state_data %>%
    magrittr::extract2('DATASUS_HOMICIDIO') %>%
    dplyr::filter(COD == coduf, ANO == ini) %>%
    dplyr::select(HOMICIDIO) %>%
    dplyr::pull()
  
  # text
  text <- glue::glue( 
    '<p class="info-result"> A Taxa de homicídios {uf_attr$PREPOS} {uf_attr$NOME} variou de {values_formatter(tx_state_old)} por 100 mil habitantes para {values_formatter(tx_state)} por 100 mil habitantes entre 2008 e 2018. Nesse último ano, o estado apresentou uma taxa {
      case_when(
        round(tx_state,1) - round(tx_aml) < -0.2 ~ "menor que a",
        round(tx_state,1) - round(tx_aml) >  0.2 ~ "maior que a",
        T ~ "próxima à"
      )
    } média do resto da região e {
        case_when(
          round(tx_state,1) - round(tx_br,1) < -0.2 ~ "abaixo",
          round(tx_state,1) - round(tx_br,1) >  0.2 ~ "acima",
          T ~ "próxima"
        )
    } do resto do Brasil.
    O número de homicídios no estado passou de {values_formatter(abs_old)}, em 2008, para {values_formatter(abs)}, em 2018, uma variação de {percent_formatter((abs/abs_old-1)*100)} no período. </p>'
  )
  
}

gen_text_transito <- function(data) {
  
}