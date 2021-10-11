
# autor: Francisco d'Albertas

#===============================================================================

# analisando equivalencia ecologica dos hexagonos usando as métricas calculadas

#===============================================================================

# abrindo os dados

p <- dirname(getwd()) #diretorio principal

pasta <- "doutorado_Clarice" # pasta onde a tabela esta salva


# abrindo df

df <- read.table(file = file.path(p,pasta,
                              'Tabela_variaveis_cenarios_atr_ecologicos.txt'),
                 sep ="\t" ,header = T)

summary(df[,c(17,18,19)]) # tem uma linha com NAs em tudo, menos na area deficit

dfNA <- df[is.na(df$riqueza_aves),] # linha com NAs

# removendo a linha

df <- df[!is.na(df$riqueza_aves),]



# dividindo metricas em 12 classes


f <- function(x)cut(x,12) # funcao q divide variavel continua em classes

df <- cbind(df,lapply(df[,c(17,18,19)],FUN = f)) # aplicando funcao nas metricas


# nomeando as novas colunas com as classes
names(df)[c(20,21,22)] <- c("riqueza_aves_cl","conect_PC400m_cl","polinizacao_cl")

summary(df[,c(20,21,22)]) # ok

#==== encontrando hexagonos equivalentes =======================================

lista_hexagonos <- list() # lista vazia para guardar os resultados

for(i in seq(1,nrow(df))){
  hex <- df[i,]
  
  #---- subset de todos os hexagonos com mesmos valores de classe --------------
  
  eq <- df[df$riqueza_aves_cl==hex$riqueza_aves_cl&
             df$conect_PC400m_cl==hex$conect_PC400m_cl&
             df$polinizacao_cl==hex$polinizacao_cl&df$id_hexagono!=hex$id_hexagono,]
  
  #---- armazenando ids dos hexagonos ------------------------------------------
  
  ids <- eq$id_hexagono # quais ids sao equivalentes
  hex$eq_hex_ids <- paste(ids,collapse = ",") # inserindo ids em uma coluna
  hex$n_eq <- length(ids) # colocando n de hexagonos equivalentes
  #---- guarando resultados na lista vazia
  lista_hexagonos[[i]] <- hex
}


#---- transformando lista em um df novamente -----------------------------------

df2 <- do.call(rbind,lista_hexagonos)


# como alguns hexagonos nao tem equivalentes, incluir valor NA


df2$eq_hex_ids[df2$n_eq==0] <- NA


# numero de hexagonos em equivalencia:


nrow(df2[df2$n_eq==0,])

# 80 hexagonos

# exportando resultado para uma tabela


write.csv(df2,file.path(p,pasta,"equivalencias.csv"),row.names = F)


