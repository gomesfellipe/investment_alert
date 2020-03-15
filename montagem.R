montagem <- function(variavel = NULL,
                     fixa = NULL,
                     entrada = NULL,
                     idade = NULL,
                     return = F){
  
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