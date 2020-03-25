#' Prepara a carteira de acordo com a regra (ou lei) dos 80
#'
#' Subtraia da sua idade o número 80. O resultado dessa conta 
#' vai indicar o percentual a ser investido em renda variável.
#' A funcao aceita a entrada de apenas um valor e o restante 
#' sera calculado em funcao dele
#'
#' @param entrada Valor total que deseja entrar para montar a carteira
#' @param variavel Valor de inicio desejado para a renda variavel
#' @param fixa Valor de inicio desejado para a renda fixa
#' @param idade Idade do investidor
#' @param return Se retornara um vetor (TRUE) ou apenas print (FALSE)
#' @return Um vetor ou um print da distribuicao da carteira de acordo com a regra
#' @export

montagem80 <- function(variavel = NULL,
                     fixa = NULL,
                     entrada = NULL,
                     idade = NULL,
                     return = F){
  
  if(is.null(idade)) stop("Informe a idade do investidor")
  
  moeda_real <- function(x){
    paste0("R$", format(x, big.mark = ".", decimal.mark = ",", nsmall = 2, digits = 2))
  }
  
  # Definir percentual com regra dos 80:
  perc_variavel <- (80 - idade)/100
  perc_fixa <- 1 - perc_variavel
  
  if(!is.null(fixa)){ 
    variavel = (perc_variavel*fixa)/perc_fixa
  }
  
  if(!is.null(variavel)){
    fixa = (perc_fixa * variavel) / perc_variavel
  }
  
  if(!is.null(fixa)  & !is.null(variavel)){
    entrada = fixa + variavel
  }
  
  if(!is.null(entrada)){ 
    fixa = entrada * perc_fixa 
    variavel = entrada * perc_variavel
  } 
  
  
  if(return == T){
    return(
      c(entrada = entrada,
        fixa = fixa,
        variavel = variavel))
  }else{
    cat(paste0("• Entrada: R$",
               moeda_real(entrada), "\n"))
    cat(paste0("  └─ Renda fixa:     ",
               moeda_real(fixa), "\n"))
    cat(paste0("  └─ Renda variavel: ",
               moeda_real(variavel), "\n"))
    cat(paste0("       (Acao + Cryptomoeda): \n       ",
               moeda_real(variavel*.9), " + ",
               moeda_real(variavel*.1), "\n"))
  }
  
}