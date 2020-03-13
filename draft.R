
calcular_carteira <- function(entrada = NULL,
                              renda_fixa = NULL,
                              renda_variavel = NULL){
  
  # Caso input = entrada
  if(is.null(renda_fixa) & is.null(renda_variavel)){
    renda_fixa <- entrada * .6
    renda_variavel <- entrada * .4
  }
  
  # Caso input = renda_variavel
  if(is.null(renda_fixa) & is.null(entrada)){
    renda_fixa <- (renda_variavel * 0.6) / 0.4
    entrada <- renda_fixa + renda_variavel
  }
  
  return(
    c(entrada = entrada, renda_fixa = renda_fixa, renda_variavel = renda_variavel)
  )
}

calcular_carteira(renda_variavel = 5000)

carteira = c("BTC-USD", "ITSA4.SA","PETR4.SA",  "GOAU4.SA", "MYPK3.SA", "MGLU3", "COGN3.SA")

request <- map(carteira, ~quantmod::getSymbols(.x, auto.assign = F)) %>% `names<-`(carteira)

names(request)
acao_selecionada = 5
request[[acao_selecionada]] %>% highcharter::hchart()

# Previsoes

atual <- c(15.56, 5.98, 14.30, 42.35, 7.55)
qntd <- c(66, 180, 70, 30, 140)
atual*qntd
sum(atual*qntd)

pred <- c(39, 13.5, 29, 62, 16)
sum(pred*qntd)


