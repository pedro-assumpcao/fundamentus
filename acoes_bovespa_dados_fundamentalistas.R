#-------------------------------------------------------------
# Web Scraping Fundamentus
# Autor: Pedro Assumpcao
# Data: 01/02/2023
# Proposta: Baixar informacao fundamentalista de todas acoes listadas na bolsa
#-------------------------------------------------------------


library(pacman)
p_load(xml2)
p_load(rvest)
p_load(rlist)
p_load(data.table)
p_load(tidyr)
p_load(stringr)
p_load(lubridate)
p_load(ggplot2)
p_load(stringi)

#caminho onde vai salvar o arquivo final -----
caminho_salvar = 'C:\\Users\\pedro_jw08iyg\\OneDrive\\Documentos'

#delimitador decimal no arquivo -----
decimal = ','

#separador de colunas ----
separador_colunas = '\t' #tab

#parametros para exclusao de acoes antigas (nao alterar)----
numero_dias_ultimo_balanco = 30*6
numero_dias_ultima_cotacao = 7*2


#obtendo os ticker de todos os ativos disponiveis ----

url_fundamentus_acoes = "https://www.fundamentus.com.br/detalhes.php?papel="
acoes_disponiveis_xml = xml2::read_html(url_fundamentus_acoes)

nos_importantes_acoes = xml_find_all(acoes_disponiveis_xml,"//html/body/div/div/div/div/table/tbody/tr/td/a")
nos_importantes_acoes_lista = as_list(nos_importantes_acoes)

acoes_disponiveis = as.data.table(xml_text(nos_importantes_acoes))
rm(acoes_disponiveis_xml,nos_importantes_acoes_lista,nos_importantes_acoes)

colnames(acoes_disponiveis)="acoes"
acoes_disponiveis[,acoes:=str_trim(acoes)]

#backup caso tenha de rodar o script novamente
if(exists("dados_importantes")){
  
  acoes_buscadas = unique(dados_importantes_agregados[,ACAO])  
  acoes_disponiveis = acoes_disponiveis[!(acoes %in% acoes_buscadas)]
  
  
}



#busca individual para dada ativos
for(papel in acoes_disponiveis[,acoes]){
  
  print(papel)
  
  
  url_fundamentus_especifico = paste0(url_fundamentus_acoes,papel)
  
  
  
  pagina = xml2::read_html(url_fundamentus_especifico)
 
  
  nos_importantes = xml_find_all(pagina,"//html/body/div/div/table/tr/td/span")
  nos_importantes_lista = as_list(nos_importantes)
 
  
  
  dados_importantes = as.data.table(xml_text(nos_importantes))
  colnames(dados_importantes) = c("COLUNA_1")
  linhas_index = as.data.table(rownames(dados_importantes))
  colnames(linhas_index) = "NUMERO_LINHA"                         
  dados_importantes = as.data.table(cbind(dados_importantes, linhas_index))
  
  
  dados_importantes[,NUMERO_LINHA:=as.numeric(NUMERO_LINHA)]
  dados_importantes[,COLUNA_2:=ifelse(NUMERO_LINHA%%2!=0,NA,shift(COLUNA_1,type = "lag"))]
  dados_importantes = dados_importantes[!is.na(COLUNA_2),.(COLUNA_2,COLUNA_1)]
  dados_importantes[,ACAO:=papel]
  dados_importantes = dados_importantes[,.(ACAO,COLUNA_1,COLUNA_2)]
  dados_importantes[,COLUNA_1:=str_trim(COLUNA_1,side = "both")]
  dados_importantes[,COLUNA_2:=str_trim(COLUNA_2,side = "both")]
  
  
  #limpeza de caracteres especiais ----
  
  dados_importantes = dados_importantes[,COLUNA_1:= stri_trans_general(str = COLUNA_1, id = "Latin-ASCII")]
  dados_importantes = dados_importantes[,COLUNA_2:= stri_trans_general(str = COLUNA_2, id = "Latin-ASCII")]
  
  
  #desfazendo os cruzamentos de valores ----
  dados_importantes[,VALOR_EM_X_COLUNA_2:=ifelse( (COLUNA_2=="?"&shift(COLUNA_1,type = "lead")=="?"),shift(COLUNA_2,type = "lead"),NA) ]
  dados_importantes[,VALOR_EM_X_COLUNA_1:=ifelse( (COLUNA_1=="?"&shift(COLUNA_2,type = "lead")=="?"),shift(COLUNA_1,type = "lead"),NA) ]
  
  #linhas problematicas ----
  dados_importantes[,linhas_problematicas:=ifelse(!is.na( shift(VALOR_EM_X_COLUNA_2, type = "lag")  ) | !is.na( shift(VALOR_EM_X_COLUNA_1,type = "lag") ),T,F )]
  
  
  
  
  #prenchendo o valor correto ----
  dados_importantes[!is.na(VALOR_EM_X_COLUNA_2),COLUNA_2:=VALOR_EM_X_COLUNA_2]
  dados_importantes[!is.na(VALOR_EM_X_COLUNA_1),COLUNA_1:=VALOR_EM_X_COLUNA_1]
  
  #corrigindo os titulos no lugar do valor variavel ----
  colunas_titulos= c("Oscilacoes",
                     "Indicadores fundamentalistas",
                     "Dados Balanco Patrimonial",
                     "Dados demonstrativos de resultados",
                     "Ultimos 12 meses",
                     "Ultimos 3 meses")
  
  dados_importantes[,TITULO_1:= ifelse(COLUNA_1 %in% colunas_titulos,T,F)]
  dados_importantes[,TITULO_2:= ifelse(COLUNA_2 %in% colunas_titulos,T,F)]
  
  #preenchendo os valores que estao nas colunas de titulos ----
  dados_importantes[,TITULO_EMBAIXO_1:=shift(TITULO_1, type = "lead")]
  dados_importantes[,TITULO_EM_CIMA_1:=shift(TITULO_1, type = "lag")]
  dados_importantes[,TITULO_EMBAIXO_2:=shift(TITULO_2, type = "lead")]
  dados_importantes[,TITULO_EM_CIMA_2:=shift(TITULO_2, type = "lag")]
  
  #fazendo a alternancia ----
  dados_importantes[,COLUNA_2:=ifelse(TITULO_EMBAIXO_1==T & COLUNA_2=="?",shift(COLUNA_2, type = "lead"),COLUNA_2)]
  dados_importantes[,COLUNA_2:=ifelse(TITULO_EM_CIMA_1==T & COLUNA_2=="?",shift(COLUNA_2, type = "lag"),COLUNA_2)]
  dados_importantes[,COLUNA_1:=ifelse(TITULO_EMBAIXO_2==T & COLUNA_1=="?",shift(COLUNA_1, type = "lead"),COLUNA_1)]
  dados_importantes[,COLUNA_1:=ifelse(TITULO_EM_CIMA_2==T & COLUNA_1=="?",shift(COLUNA_1, type = "lag"),COLUNA_1)]
  
  
  #zerando as variaveis de titulos para facilitar o filtro posterior ----
  dados_importantes = dados_importantes[TITULO_1==F]
  dados_importantes = dados_importantes[TITULO_2==F]
  dados_importantes = dados_importantes[COLUNA_1!="?"]
  dados_importantes = dados_importantes[COLUNA_2!="?"]
  dados_importantes = dados_importantes[COLUNA_1!=""]
  dados_importantes = dados_importantes[COLUNA_2!=""]
  dados_importantes = dados_importantes[,.(ACAO,COLUNA_1,COLUNA_2)]
  
  
  
  #separando em nome de variavel e valor da variavel
  decisao_1 = nrow(copy(dados_importantes[ACAO==COLUNA_1]))>0
  decisao_2 = nrow(copy(dados_importantes[ACAO==COLUNA_2]))>0
  
  if(decisao_1==T){
    
    dados_importantes = dados_importantes[,.(ACAO,INDICADOR=COLUNA_2,VALOR=COLUNA_1)]
  } else if (decisao_2==T) {
    dados_importantes = dados_importantes[,.(ACAO,INDICADOR=COLUNA_1,VALOR=COLUNA_2)]
  } else{
    
    print("formatacao da pagina inconsistente")
    dados_importantes = dados_importantes[,.(ACAO,INDICADOR=COLUNA_1,VALOR=COLUNA_2)]
    
    
  }
  
  
  dados_importantes[,CONTAGEM_NUMEROS_VALOR:=str_count(VALOR, pattern="[:digit:]")]
  dados_importantes[,CONTAGEM_NUMEROS_INDICADOR:=str_count(INDICADOR, pattern="[:digit:]")]
  dados_importantes[,CONTAGEM_LETRAS_VALOR:=str_count(VALOR, pattern="[:alpha:]")]
  dados_importantes[,CONTAGEM_LETRAS_INDICADOR:=str_count(INDICADOR, pattern="[:alpha:]")]
  dados_importantes[,CONTAGEM_VIRGULA_VALOR:=str_count(VALOR, pattern=",")]
  dados_importantes[,CONTAGEM_VIRGULA_INDICADOR:=str_count(INDICADOR, pattern=",")]
  dados_importantes[,CONTAGEM_PONTOS_VALOR:=str_count(VALOR, pattern="\\.")]
  dados_importantes[,CONTAGEM_PONTOS_INDICADOR:=str_count(INDICADOR, pattern="\\.")]
  dados_importantes[,CONTAGEM_PORCENTAGEM_VALOR:=str_count(VALOR, pattern="%")]
  dados_importantes[,CONTAGEM_PORCENTAGEM_INDICADOR:=str_count(INDICADOR, pattern="%")]
  dados_importantes[,CONTAGEM_PORCENTAGEM_INDICADOR:=str_count(INDICADOR, pattern="%")]
  dados_importantes[,CONTAGEM_PORCENTAGEM_BARRA:=str_count(INDICADOR, pattern="/")]
  
  
  # decisoes manuais ----
  #porcetangem
  dados_importantes[CONTAGEM_PORCENTAGEM_INDICADOR==1,SWAP:=T]
  
  #numeros decimais
  dados_importantes[is.na(SWAP) & CONTAGEM_NUMEROS_INDICADOR>=3 & CONTAGEM_LETRAS_INDICADOR==0 & (CONTAGEM_VIRGULA_INDICADOR==1 | CONTAGEM_PONTOS_INDICADOR>=1 | CONTAGEM_PORCENTAGEM_BARRA==2),SWAP:=T]
  
  #troca numericos remanescentes
  dados_importantes[is.na(SWAP) & CONTAGEM_NUMEROS_INDICADOR>=1 & CONTAGEM_LETRAS_INDICADOR==0,SWAP:=T]
  
  #variaveis que sao somente string
  indicadores_padrao = c("Papel",
                         "Cotacao",
                         "Tipo",
                         "Data ult cot",
                         "Empresa",
                         "Min 52 sem",
                         "Setor",
                         "Max 52 sem",
                         "Subsetor",
                         "Vol $ med (2m)",
                         "Valor de mercado",
                         "Ult balanco processado",
                         "Valor da firma",
                         "Nro. Acoes",
                         "Dia",
                         "P/L",
                         "LPA",
                         "Mês",
                         "P/VP",
                         "VPA",
                         "30 dias",
                         "P/EBIT",
                         "Marg. Bruta",
                         "12 meses",
                         "PSR",
                         "Marg. EBIT",
                         "P/Ativos",
                         "Marg. Liquida",
                         "P/Cap. Giro",
                         "EBIT / Ativo",
                         "P/Ativ Circ Liq",
                         "ROIC","Div. Yield",
                         "ROE",
                         "EV / EBITDA",
                         "Liquidez Corr",
                         "EV / EBIT",
                         "Div Br/ Patrim",
                         "Cres. Rec (5a)",
                         "Giro Ativos",
                         "Ativo",
                         "DDiv. Bruta",
                         "Disponibilidades",
                         "Div. Liquida",
                         "Ativo Circulante",
                         "Patrim. Liq",
                         "Receita Liquida",
                         "EBIT",
                         "Lucro Liquido")
  
  #fazendo troca para os indicadores acima
  dados_importantes[is.na(SWAP) & VALOR %in% indicadores_padrao,SWAP:=T]
  
  indicadores_de_valores_string = c("Tipo","Empresa","Setor","Subsetor")
  
  dados_importantes[is.na(SWAP) & VALOR %in% indicadores_de_valores_string,SWAP:=T]
  
  
  #aplicando a inversao
  dados_importantes[SWAP==T, INDICADOR_SWAP:=VALOR]
  dados_importantes[SWAP==T, VALOR_SWAP:=INDICADOR]
  dados_importantes[SWAP==T, INDICADOR:=INDICADOR_SWAP]
  dados_importantes[SWAP==T, VALOR:=VALOR_SWAP]
  
  
  dados_importantes = dados_importantes[,.(ACAO,INDICADOR,VALOR)]
  
  
  
  
  
  if(!exists("dados_importantes_agregados")){
    
    dados_importantes_agregados = copy(dados_importantes)
    
  } else {
    
    dados_importantes_agregados = rbindlist(list(dados_importantes,dados_importantes_agregados))  
    
  }
  
  rm(linhas_index,nos_importantes,nos_importantes_lista,pagina,papel)
  
}



#salvando um backup caso tenha algum problema
dados_importantes_agregados_bkp = copy(dados_importantes_agregados)

#arrumando as datas -----
dados_importantes_agregados = copy(dados_importantes_agregados_bkp)
dados_importantes_agregados[INDICADOR=="Ult balanco processado",DATA_ULTIMO_BALANCO:=dmy(VALOR)]
dados_importantes_agregados[,DATA_ULTIMO_BALANCO:=max(DATA_ULTIMO_BALANCO,na.rm = T),by=ACAO]
dados_importantes_agregados[,DESATUALIZACAO:=today()-DATA_ULTIMO_BALANCO]


dados_importantes_agregados = dados_importantes_agregados[DESATUALIZACAO<=numero_dias_ultimo_balanco]

dados_importantes_agregados[INDICADOR=="Data ult cot",DATA_ULTIMA_COTACAO:=dmy(VALOR)]
dados_importantes_agregados[,DATA_ULTIMA_COTACAO:=max(DATA_ULTIMA_COTACAO,na.rm = T),by=ACAO]
dados_importantes_agregados[,DESATUALIZACAO_COTACAO:=today()-DATA_ULTIMA_COTACAO]
dados_importantes_agregados = dados_importantes_agregados[DESATUALIZACAO_COTACAO<=numero_dias_ultima_cotacao]


#renomenando indicadores repetidos
dados_importantes_agregados[,CONTAGEM_INDICADOR:=.N, by=.(ACAO,INDICADOR)]
dados_importantes_agregados[,VALOR:=str_remove_all(VALOR,"\\.")]
dados_importantes_agregados[,VALOR:=str_remove(VALOR,"%")]
dados_importantes_agregados[,VALOR:=str_replace(VALOR,",","\\.")]
dados_importantes_agregados[,VALOR_NUMERICO:=as.numeric(VALOR)]
dados_importantes_agregados[!is.numeric(VALOR_NUMERICO),VALOR:=VALOR_NUMERICO]
dados_importantes_agregados[,ID_LINHA:=rowid(INDICADOR),by=.(ACAO)]
dados_importantes_agregados[ID_LINHA==1 & CONTAGEM_INDICADOR==2,INDICADOR:=paste(INDICADOR,"12_MESES")]
dados_importantes_agregados[ID_LINHA==2 & CONTAGEM_INDICADOR==2,INDICADOR:=paste(INDICADOR,"3_MESES")]
dados_importantes_agregados = dados_importantes_agregados[,.(ACAO,INDICADOR,VALOR)]
dados_importantes_agregados = pivot_wider(dados_importantes_agregados,names_from = INDICADOR, values_from = VALOR)
setDT(dados_importantes_agregados)




#padronizando os nomes
dados_importantes_agregados[,ACAO:=Papel]
dados_importantes_agregados[,COTACAO_ATUAL:=as.numeric(Cotacao)]
dados_importantes_agregados[,TIPO_ACAO:=toupper(Tipo)]
dados_importantes_agregados[,ULTIMA_COTACAO:=dmy(`Data ult cot`)]
dados_importantes_agregados[,EMPRESA:=toupper(Empresa)]
dados_importantes_agregados[,COT_MIN_52_SEM:=as.numeric(`Min 52 sem`)]
dados_importantes_agregados[,SETOR:=toupper(Setor)]
dados_importantes_agregados[,COT_MAX_52_SEM:=as.numeric(`Max 52 sem`)]
dados_importantes_agregados[,SUBSETOR:=toupper(Subsetor)]
dados_importantes_agregados[,VOL_MEDIO_2_MESES:=as.numeric(`Vol $ med (2m)`)]
dados_importantes_agregados[,VALOR_MERCADO_EMPRESA:=as.numeric(`Valor de mercado`)]
dados_importantes_agregados[,ULTIMO_BALANCO_EMPRESA:=dmy(`Ult balanco processado`)]
dados_importantes_agregados[,VALOR_FIRMA:=as.numeric(`Valor da firma`)]
dados_importantes_agregados[,NUMERO_ACOES_TOTAIS_TODOS_TIPOS:=as.numeric(`Nro. Acoes` )]
dados_importantes_agregados[,OSCILACAO_HOJE:=as.numeric(Dia)/100]
dados_importantes_agregados[,P_L:=as.numeric(`P/L`)]
dados_importantes_agregados[,LPA:=as.numeric(LPA)]
dados_importantes_agregados[,OSCILACAO_MES:=as.numeric(Mes)/100]
dados_importantes_agregados[,P_VP:=as.numeric(`P/VP`)]
dados_importantes_agregados[,VPA:=as.numeric(VPA)]
dados_importantes_agregados[,OSCILACAO_ULTIMOS_30_DIAS:=as.numeric(`30 dias`)/100]
dados_importantes_agregados[,P_EBIT:=as.numeric(`P/EBIT`)]
dados_importantes_agregados[,MARGEM_BRUTA:=as.numeric(`Marg. Bruta`)]
dados_importantes_agregados[,OSCILACAO_ULTIMOS_12_MESES:=as.numeric(`12 meses`)]
dados_importantes_agregados[,PSR:=as.numeric(PSR)]
dados_importantes_agregados[,MARGEM_EBIT:=as.numeric(`Marg. EBIT`)]
dados_importantes_agregados[,P_ATIVOS_TOTAIS:=as.numeric(`P/Ativos`)]
dados_importantes_agregados[,MARGEM_LIQUIDA:=as.numeric(`Marg. Liquida` )/100]
dados_importantes_agregados[,P_CAP_GIRO:=as.numeric(`P/Cap. Giro`)]
dados_importantes_agregados[,EBIT_POR_ATIVOS_TOTAIS:=as.numeric(`EBIT / Ativo`)/100]
dados_importantes_agregados[,P_ATIVO_CIRC_LIQ:=as.numeric(`P/Ativ Circ Liq`)]
dados_importantes_agregados[,ROIC:=as.numeric(ROIC)/100]
dados_importantes_agregados[,DIVIDEND_YIELD:=as.numeric(`Div. Yield`)/100]
dados_importantes_agregados[,ROE:=as.numeric(ROE)/100]
dados_importantes_agregados[,EV_EBITDA:=as.numeric(`EV / EBITDA`)/100]
dados_importantes_agregados[,LIQUIDEZ_CORRENTE:=as.numeric(`Liquidez Corr`)]
dados_importantes_agregados[,EV_EBIT:=as.numeric(`EV / EBIT`)]
dados_importantes_agregados[,DIVIDA_BRUTA_POR_PATRIMONIO_LIQUIDO:=as.numeric(`Div Br/ Patrim`)]
dados_importantes_agregados[,VARIACAO_RECEITA_LIQUIDA_5_ANOS:=as.numeric(`Cres. Rec (5a)`)]
dados_importantes_agregados[,GIRO_ATIVOS:=as.numeric(`Giro Ativos`)]
dados_importantes_agregados[,ATIVO:=as.numeric(Ativo)]
dados_importantes_agregados[,DIVIDA_BRUTA:=as.numeric(`Div. Bruta`)]
dados_importantes_agregados[,DISPONIBILIDADES:=as.numeric(Disponibilidades)]
dados_importantes_agregados[,DIVIDA_LIQUIDA:=as.numeric(`Div. Liquida`)]
dados_importantes_agregados[,ATIVO_CIRCULANTE:=as.numeric(`Ativo Circulante`)]
dados_importantes_agregados[,PATRIMONIO_LIQUIDO:=as.numeric(`Patrim. Liq` )]
dados_importantes_agregados[,RECEITA_LIQUIDA_12_MESES:=as.numeric(`Receita Liquida 12_MESES`)]
dados_importantes_agregados[,RECEITA_LIQUIDA_3_MESES:=as.numeric(`Receita Liquida 3_MESES`)]
dados_importantes_agregados[,EBIT_12_MESES:=as.numeric(`EBIT 12_MESES`)]
dados_importantes_agregados[,EBIT_3_MESES:=as.numeric(`EBIT 3_MESES`)]
dados_importantes_agregados[,LUCRO_LIQUIDO_12_MESES:=as.numeric(`Lucro Liquido 12_MESES`)]
dados_importantes_agregados[,LUCRO_LIQUIDO_3_MESES:=as.numeric(`Lucro Liquido 3_MESES`)]
dados_importantes_agregados[,DEPOSITOS:=as.numeric(Depositos)]
dados_importantes_agregados[,CARTEIRA_DE_CREDITO:=as.numeric(`Cart. de Credito`)]
dados_importantes_agregados[,RESULTADO_INTERMEDICAO_FINANCEIRA_12_MESES:=as.numeric(`Result Int Financ 12_MESES`)]
dados_importantes_agregados[,RESULTADO_INTERMEDICAO_FINANCEIRA_3_MESES:=as.numeric(`Result Int Financ 3_MESES`)]
dados_importantes_agregados[,RECEITA_SERVICOS_12_MESES:=as.numeric(`Receita Liquida 12_MESES`)]
dados_importantes_agregados[,RECEITA_SERVICOS_3_MESES:=as.numeric(`Receita Liquida 3_MESES`) ]


dados_importantes_agregados = dados_importantes_agregados[,.(ACAO,COTACAO_ATUAL,TIPO_ACAO,ULTIMA_COTACAO,EMPRESA,COT_MIN_52_SEM,SETOR,COT_MAX_52_SEM,SUBSETOR,VOL_MEDIO_2_MESES,VALOR_MERCADO_EMPRESA,ULTIMO_BALANCO_EMPRESA,VALOR_FIRMA,NUMERO_ACOES_TOTAIS_TODOS_TIPOS,OSCILACAO_HOJE,P_L,LPA,OSCILACAO_MES,P_VP,VPA,OSCILACAO_ULTIMOS_30_DIAS,P_EBIT,MARGEM_BRUTA,OSCILACAO_ULTIMOS_12_MESES,PSR,MARGEM_EBIT,P_ATIVOS_TOTAIS,MARGEM_LIQUIDA,P_CAP_GIRO,EBIT_POR_ATIVOS_TOTAIS,P_ATIVO_CIRC_LIQ,ROIC,DIVIDEND_YIELD,ROE=as.numeric(ROE)/100,EV_EBITDA,LIQUIDEZ_CORRENTE,EV_EBIT,DIVIDA_BRUTA_POR_PATRIMONIO_LIQUIDO,VARIACAO_RECEITA_LIQUIDA_5_ANOS,GIRO_ATIVOS,ATIVO,DIVIDA_BRUTA,DISPONIBILIDADES,DIVIDA_LIQUIDA,ATIVO_CIRCULANTE,PATRIMONIO_LIQUIDO,RECEITA_LIQUIDA_12_MESES,RECEITA_LIQUIDA_3_MESES,EBIT_12_MESES,EBIT_3_MESES,LUCRO_LIQUIDO_12_MESES,LUCRO_LIQUIDO_3_MESES, DEPOSITOS, CARTEIRA_DE_CREDITO, RESULTADO_INTERMEDICAO_FINANCEIRA_12_MESES,RESULTADO_INTERMEDICAO_FINANCEIRA_3_MESES,RECEITA_SERVICOS_12_MESES,RECEITA_SERVICOS_3_MESES)]
dados_importantes_agregados[,DIA_EXTRACAO:=today()]

#salvando arquivo em csv
fwrite(dados_importantes_agregados,paste0(caminho_salvar,"\\indicadores_fundamentalistas_",Sys.Date(),".txt"),
                                          sep = separador_colunas, dec = decimal)

