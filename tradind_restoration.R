#===============================================================================

# arranjos de compensacao de RL para o Estado de SP usando restauracao, quando
# as compensacoes de vegetacao em pe ja se esgotaram

#===============================================================================

#==== pacotes ==================================================================

library(dplyr)
library(tidyr)
library(stringr)
#===============================================================================

# abrindo os dados

p <- dirname(getwd()) #diretorio principal

pasta <- "doutorado_Clarice" # pasta onde a tabela esta salva

# abrindo df

df <- read.csv(file = file.path(p,
      pasta,'Tabela_variaveis_cenarios_3atributos_pos_comp.csv'))

# ordenando dados pelo custo de restauracao, decrescente

df2 <-df %>% arrange(desc(preco_CO))


df3 <- df2 # df para ser modificado pela funcao!

# limite superior do for

i=4 # tem area deficit maior q area disponivel pra restaurar
i= 38  # deficit menor q area disponivel!
i=1
# checar hex 1941
#  hex 1978

i=679

which(df3$id_hexagono==4187)


lim <- nrow(df2)

for(i in seq(1,lim)){
  
  
  
  # hex q recebera compensacao via restauracao
  hex <- df3[i,]
  # se area disponivel pra restaurar == 0, pular
  if(hex$area_restauracao==0){next}else{
    
    #---- subset hexagonos  com mesmos valores de classe -------------------------
    
    # selecionando apenas hexagonos com deficit, com equivalencia ecologica
    eq <- df3[df3$riqueza_aves_cl==hex$riqueza_aves_cl&
                df3$conect_PC400m_cl==hex$conect_PC400m_cl&
                df3$polinizacao_cl==hex$polinizacao_cl&df3$area_deficit_oficial>0,]
    
    if(nrow(eq)==0){
      # substituindo df original!
      df3$valor_rest[df3$id_hexagono==hex$id_hexagono] <- 0
      df3$id_hex_rest[df3$id_hexagono==hex$id_hexagono] <- NA
      
    } else{
      #---- ordenando hexagonos equivalentes pela area de deficit ------------------
      eq <- eq %>% arrange(desc(area_deficit_oficial))
      
      #---- selecionando valores pra serem compensados -----------------------------
      
      # se a area de deficit do primeiro hex for maior ou igual  a area de excedente
      # isto é, usa toda a area disponivel a ser restaurada:
      if (eq$area_deficit_oficial[1]>=hex$area_restauracao){
        
        # selecionar apenas 1 hexagono
        
        def <- eq[1,] # deficit a ser compensado 
        
        #---- atualizando valores do dataframe -----------------------------------
        # descontando valor compensado
        df3$area_restauracao[df3$id_hexagono==hex$id_hexagono] <- 0
        # valor compensado
        df3$valor_rest[df3$id_hexagono==hex$id_hexagono] <- hex$area_restauracao*hex$preco_restauracao
        df3$id_hex_rest[df3$id_hexagono==hex$id_hexagono] <- def$id_hexagono
        # descontando valor deficit! aqui precisa ser no hexagono q foi compensado!
        df3$area_deficit_oficial[df3$id_hexagono==def$id_hexagono] <- 
          df3$area_deficit_oficial[df3$id_hexagono==def$id_hexagono]-
          hex$area_restauracao
        
        
      }else{
        # se a area de deficit for menor que a area disponivel pra restautar, 
        # pode ser q um hexagono compense mais de 1, ai precisa incluir na coluna.
        
        # area de excedente:
        area_rest <- hex$area_restauracao
        eq$cumsum <- cumsum(eq$area_deficit_oficial)
        # subset apenas com areas q completam o excedente. Caso nao complete, o hexagono
        # continua na proxima
        def <- eq[eq$cumsum<=area_rest,]
        # area total compensada
        area_restaurada <- sum(def$area_deficit_oficial)
        # descontando valor restaurado 
        df3$area_restauracao[df3$id_hexagono==hex$id_hexagono] <- area_rest-
          area_restaurada
        # valor compensado
        df3$valor_rest[df3$id_hexagono==hex$id_hexagono] <- area_restaurada*hex$preco_restauracao
        # o erro tava aqui, eu tava indexando o id do eq, nao do def!
        df3$id_hex_rest[df3$id_hexagono==hex$id_hexagono] <- paste(def$id_hexagono,collapse = ",")
        #print("vai dar certo")
        # descontando valor deficit! restaura o deficit todo, logo é 0
        df3$area_deficit_oficial[df3$id_hexagono %in%def$id_hexagono] <- 0
        
      }
    }
  }
}


summary(df3)

# 


write.csv(df3,"Tabela_variaveis_cenarios_3atributos_pos_rest.csv",row.names = F)


#===============================================================================

# checando hexagonos q compensam mtos hexagonos com restauracao

# simplificando df

df_c <- df3[,c(1,30)]

which(df_c$id_hexagono==1312)

i=679

for(i in seq(1,nrow(df_c))){
  
  x <- df_c[i,2]
  
  x2 <- unlist(strsplit(x,","))
  df_compensa <- df2[df2$id_hexagono==df_c$id_hexagono[i],]
  df_compensado <- df2[df2$id_hexagono %in% x2,]
  area_disponivel_compensacao <- df_compensa$area_restauracao 
  area_compensada <- sum(df_compensado$area_deficit_oficial) 
  
  }

df2$area_restauracao[df2$id_hexagono==1312]

# parece q tem um descompasso e mais area sendo compensada doq disponivel!
# supostamente eh so olhar no obj eq

# fazer a mesma conta pra compensacao!!!








# checar hex 1941
#  hex 1978

df194 <- df3[df3$id_hexagono==1941,]
df194_antes <- df2[df2$id_hexagono==1941,]
df779 <- df3[df3$id_hexagono==779,]
df779_antes <- df2[df2$id_hexagono==779,]

which(df3$id_hexagono==1941)

# o 1941 parece estar ok, mas o 1978 nao, pq o deficit dele some, mas nao tem
# 

df1978 <- df3[df3$id_hexagono==1978,]
df1978_antes <- df2[df2$id_hexagono==1978,]


# tem q achar quem compenseou esse deficit do 1941, ele precisa aparecer na
# coluna

which(df3$id_hex_def==1941)
which(df3$id_hex_rest==1646)

df3_su <- df3[,c(1,28)]

df3_su <- df3_su %>% separate(id_hex_def,paste0(seq(1:300)))

df3_su <- df3_su[,1:112]

which(df3_su[1,]==1941)


# ids equivalentes a 1941
ids <- eq$id_hexagono
#subset dos ids equivalentes a 1941
df_sub <- df3[df3$id_hexagono %in% ids,]

comp1941 <- df_sub[37,]

for(i in seq(1,nrow(df_sub))){
  
  x <- df_sub[37,30]
  
  x2 <- unlist(strsplit(x,","))
  pat <- "1941"
  str_detect(x2,pat)
  
}

# hexagono 1312 compensa deficit do 1941! ok, mas eh estranho, compensa de mtos!

df_confere <- df2[df2$id_hexagono %in% x2,]

sum(df_confere$area_deficit_oficial) # 7445.583!

df1312 <- df2[df2$id_hexagono==1312,]
