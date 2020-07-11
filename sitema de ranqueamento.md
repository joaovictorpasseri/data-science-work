---
title: "Sistema de Ranqueamento para Equipes de Futebol Sul-Americanos"
author: "João Victor Passeri Medeiros Amorim"
output:  
  github_document
---
<style>
  body{
    font-family: 'Oxygen', sans-serif;
    font-size: 16px;
    line-height: 24px;
  }

h1,h2,h3,h4 {
  font-family: 'Raleway', sans-serif;
}

.container { width: 1000px; }
h3 {
  background-color: #D4DAEC;
    text-indent: 100px; 
}
h4 {
  text-indent: 100px;
}

g-table-intro h4 {
  text-indent: 0px;
}
</style>

### Introdução e Objetivo.

  Este trabalho foi desenvolvido como objetivo criar um modelo de ranqueamento baseado no [Sistema Elo](https://pt.wikipedia.org/wiki/Rating_ELO) para os times de futebol da América do Sul. O modelo de ranqueamento foi aplicado em mais de 70 mil partidas de futebol dos principais campeonatos nacionais e internacionais da América do Sul. Ao final foi obtido um ranking de clubes e algumas análise e curiosidades do futebol sul-americano.

  Para alcançar o objetivo, foram realizadas as seguintes tarefas:

* Webscraping.
* Manipulação, limpeza e tratamento dos dados.
* Modelagem e aplicação do modelo.

Foi realizada uma raspagem de dados do site <https://www.worldfootball.net>. Em todo o trabalho foram utilizadas as seguintes bibliotecas:

```{r include=F, message=F, warning=FALSE}
library(tibble)
library(httr)
library(rvest)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(plyr)
library(comprehenr)
library(scales) 
library(gridExtra)
library(DT) 

```


### Raspagem e obtenção de dados do WorldFooteboll.

  Foram criadas funções que facilitam e e automatizam o trabalho de webscraping do site <https://www.worldfootball.net>. Tais funções organizam os campeonatos em tabelas e exportam arquivos em formato csv.

```{r eval = F, results = 'asis'}
pegar_tabela_de_jogos = function(site,nome){
  jogos = read_html(site)
  jogos =  html_table(jogos)
  jogos = jogos[[2]]
  names(jogos) = c("Data","Hora","Mandante","Placar","Visitante","HT","Campeonato","Apagar")
  jogos$Placar=jogos$HT
  jogos$HT = NULL
  jogos$Apagar = NULL
  jogos$Hora = NULL
  jogos$Campeonato = nome
  return(jogos)
}

pegar_sites = function(site){
  x = read_html(site)
  x = html_nodes(x,'form')
  x = xml_child(x[[3]], 1)
  x = xml_children(x)
  x = lapply(1:length(x), function(i) xml_attrs(x[[i]])[["value"]]) %>% unlist(use.names=FALSE)
  x = lapply(1:length(x), function(i) str_c('https://www.worldfootball.net',x[i])) %>% unlist(use.names=FALSE)
  return(x)

scrap_to_csv = function(site,nome_campeonato){
  x = pegar_sites(site)
  campeonatos = lapply(1:length(x), function(i) pegar_tabela_de_jogos(x[i],nome_campeonato))
  campeonatos = join_all(campeonatos,type = "full")
  nome_campeonato = str_c(nome_campeonato,".csv")
  write.csv(campeonatos,nome_campeonato)
  
  
 
  return(campeonatos)
  
  
}
```

  As funções feitas foram aplicadas aos campeonados e fedarações de interesse do trabalho.

### Manipulação dos dados.

  Após a raspagem e união de todos os 28 campeonatos e temporadas em uma única planilha, a seguinte base foi obtida:

```{r include=FALSE, message=F, warning=F}
jogos = list.files("C:/Users/joao passeri/Desktop/projeto final/final 2",full.names = TRUE)
arquivos = sapply(jogos, read.csv,simplify = F)
jogos = join_all(arquivos,type = "full")

```
```{r echo=FALSE, message=FALSE }
jogos %>% head(5) %>% DT::datatable()

```

  Foi necessário completar as datas dos jogos, uma vez que são importantes para o modelo. Os placares são da classe string e informados de várias formas diferentes (alguns erros foram encontrados nos placares). O código abaixo realiza todo o tratamento da base de dados e separa em data frames que facilitam o trabalho. Os principais data frames são:


* df.times
* jogos
* nacao

#### Estas funções organizam a base e retornam data.frames.

```{r eval=F, message=F, warning=FALSE}
###Limpando os dados e completando datas faltantes 
limpar_dados = function(jogos){
  jogos = mutate(jogos,id = 1:nrow(jogos))
  x = jogos[str_detect(jogos$Mandante,'.Round'),] %>% subset(id) %>% unlist()
  jogos =jogos[-x,]
  ####################################################################################
  jogos = completar_data(jogos)
  ##################################################################################### 
  jogos$Data=str_remove_all(jogos$Data,"[/:]")
  jogos$Data = as.numeric(jogos$Data)
  jogos$Data=dmy(jogos$Data)
  jogos$Placar[jogos$Placar=="-:-"] = NA
  jogos = na.omit(jogos)
  
  return(jogos)
}

completar_data = function(games){
  for (i in 1:length(games$Data)){
    if (games$Data[i]==""){
      games$Data[i]=games$Data[i-1]
      
      
    }
  }
  return(games)
}

jogos = limpar_dados(jogos)
 
jogos$X = NULL
jogos$Mandante = as.character(jogos$Mandante)
jogos$Placar = as.character(jogos$Placar)
jogos$Visitante = as.character(jogos$Visitante)
jogos$Campeonato = as.character(jogos$Campeonato)
jogos = mutate(jogos, Placar = str_replace_all(Placar,' ',''))

######Correção dos placares#######################
palavras = c('dnp','dec','annul','pso','aet','WO','abor.','resch.')
linhas = to_list(for(i in 1:nrow(jogos)) for(j in palavras) if(str_detect(jogos$Placar[i],j)) jogos[i,]) %>%
  join_all(type = 'full')
jogos = anti_join(jogos,linhas, by ='id')
jogos = mutate(jogos, tamanho = str_length(jogos$Placar))
jogos = subset(jogos, tamanho==3 | tamanho==8) 
jogos = mutate(jogos, GH = as.numeric(str_sub(jogos$Placar,1,1)), GA = as.numeric(str_sub(jogos$Placar,3,3)))
jogos$tamanho = NULL
jogos$Placar = NULL
jogos = arrange(jogos,Data)



######Criando dataframes por paises################
df_Brazil = jogos[str_detect(jogos$Campeonato,'Brazil'),] %>% mutate(pais = 'Brazil')
df_Argentina = jogos[str_detect(jogos$Campeonato,'Argentina'),] %>% mutate(pais = 'Argentina')
df_Chile = jogos[str_detect(jogos$Campeonato,'Chile'),] %>% mutate(pais = 'Chile')
df_Bolivia = jogos[str_detect(jogos$Campeonato,'Bolivia'),] %>% mutate(pais = 'Bolivia')
df_Colombia = jogos[str_detect(jogos$Campeonato,'Colombia'),] %>% mutate(pais = 'Colombia')
df_Ecuador = jogos[str_detect(jogos$Campeonato,'Ecuador'),] %>% mutate(pais = 'Ecuador')
df_Paraguay = jogos[str_detect(jogos$Campeonato,'Paraguay'),] %>% mutate(pais = 'Paraguay')
df_Peru = jogos[str_detect(jogos$Campeonato,'Peru'),] %>% mutate(pais = 'Peru')
df_Uruguay = jogos[str_detect(jogos$Campeonato,'Uruguay'),] %>% mutate(pais = 'Uruguay')
df_Venezuela = jogos[str_detect(jogos$Campeonato,'Venezuela'),] %>% mutate(pais = 'Venezuela')
df_Sulamericana = jogos[str_detect(jogos$Campeonato,'Copa Sudamericana'),] %>% mutate(pais = 'Sudamericana')
df_Libertadores = jogos[str_detect(jogos$Campeonato,'Copa Libertadores'),] %>% mutate(pais = 'Libertadores')

df = list(df_Brazil,df_Argentina,df_Chile,df_Bolivia,df_Colombia,
             df_Ecuador,df_Paraguay,df_Peru,df_Uruguay,df_Venezuela, 
             df_Sulamericana,df_Libertadores) 

times_paises = function(df){
  x =  as.vector(df$Mandante) %>% unique()
  y = as.vector(df$Visitante) %>% unique()
  z = as.vector(df$pais) %>% unique()
  xy = c(x,y) %>% unique()
  xy = data.frame(nome = xy,pais=z) 
  
  return(xy)
}

df.times = lapply(1:10,function(i) times_paises(df[[i]])) %>% join_all(type = 'full')
inter = times_paises(df_Libertadores)
inter = anti_join(inter,df.times,by='nome')
inter$pais = 'Convidado'
x = data.frame(nome = c('Brazil','Argentina','Chile','Bolivia','Colombia','Ecuador','Paraguay','Peru','Uruguay','Venezuela','Convidado'),
               pais = c('Brazil','Argentina','Chile','Bolivia','Colombia','Ecuador','Paraguay','Peru','Uruguay','Venezuela','Convidado'))
df.times = rbind(df.times,inter) %>% rbind(x)
nacao = inner_join(df_Libertadores,df.times,by=c('Mandante'='nome'))
nacao = inner_join(nacao,df.times,by=c('Visitante'='nome')) %>% arrange(Data) 
nacao = select(nacao,Data,pais.y,pais,Campeonato,id,GH,GA)
names(nacao) = c("Data",'Mandante',"Visitante","Campeonato","id",'GH',"GA")
nacao = nacao[nacao$Mandante!=nacao$Visitante,]  

```
```{r include=F}
setwd("C:/Users/joao passeri/Desktop/projeto final/markdown")
jogos = read.csv('jogos.csv')
jogos = jogos[,-1]
df.times = read.csv('dftimes.csv')
df.times = df.times[,-1]
nacao = read.csv('nacao.csv')
nacao = nacao[,-1]


```

#### Problemas.

Alguns times internacionais tinham o mesmo nome na base de dados, e não possuíam nenhuma informação para diferenciá-los. Entrei em contato com o site para propor uma solução simples, que foi negada. Pela Copa Libertadores apenas duas equipes diferentes e em uma edição aparecem com mesmo nome, River Plate URU e River Plate ARG, porém pela Copa Sudamericana isso ocorre com mais equipes e em mais edições, impossibilitando uma correção manual. Portanto optei por não trabalhar mais com este campeonato.

### Modelo.

O modelo [Elo](https://pt.wikipedia.org/wiki/Rating_ELO) supõe que o rate do jogador é uma variável aleatória gaussiana. Sendo $R_a$ e $R_b$ os rates atuais de cada jogador, a função logística que retorna a esperança de vitória de cada jogador é:

$E_a = \cfrac{1}{1 + 10^\frac{R_b - R_a}{400}}$

$E_b = 1 - E_a$

Já a fórmula pára atualizar o rate do jogador após o resultado da partid é:

$R'_a = R_a + K(S - E_a)\sqrt{dif}$

$K$ assume dois valores $\{20,40\}$, sendo $20$ para os campeonatos nacionais e $40$ para os internacionais. $S$ assume três valores, $\{1,0.5,0\}$, em caso de vitória, empate e derrota respectivamente.
$dif$ é a diferença de gols entre vencedor e perdedor e assume o valor $1$ em caso de empate.

#### Neste script criamos o modelo e aplicamos a base de dados, separando os resultados pra posterior análise.

```{r eval=F, message=F, warning=FALSE}
serieA = lapply(1:10, function(i) df[[i]][str_detect(df[[i]][,4],'Serie A|Copa'),] )
serieA[[11]] = df[[12]]


fun_logistica = function(RA,RB,Ra,Rb,k,dif){
  EA = 1/(1+10^((RB-RA)/400))
  EB = 1 - EA
  if(dif<0){
    dif = -dif
  }else if(dif>0){
    dif = dif
    
  }else{
    dif = 1
  }
  RA1 = RA + k*(Ra-EA)*(dif^(1/2))
  RB1 = RB + k*(Rb-EB)*(dif^(1/2))
  return(list(RA1,RB1,RA,RB))
}

elo_rating = function(df){
  tab = df.times %>% mutate(novo_rate = 1600) %>% mutate(Data = dmy('01-01-1000'))
  tab[,1] = as.character(tab[,1])
  tab[,2] = as.character(tab[,2])
  tib = tab
  as = data.frame(Data = dmy('01-01-1000'),Mandante = "Casa" , GH = 0, GA = 0,
                  Visitante = "Fora",rateH=1600,rateA=1600)
  as$Mandante = as.character(as$Mandante)
  as$Visitante = as.character(as$Visitante)
  for(i in 1:nrow(df)){
    x = tab[tab$nome==df[i,2],]
    y = tab[tab$nome==df[i,3],]
    jogo = df[i,]
    camp = df[i,4]
    if(str_detect(camp,'Libertadores')){
      k = 40
    } else {
      k = 20
    }
    if(jogo[1,6]-jogo[1,7]>0){
      elo= fun_logistica(x[1,3],y[1,3],1,0,k,jogo[1,6]-jogo[1,7])
      x = list(x[1,1],x[1,2],elo[[1]],jogo[1,1])
      y = list(y[1,1],y[1,2],elo[[2]],jogo[1,1])
      z = list(jogo[1,1],jogo[1,2],jogo[1,6],jogo[1,7],jogo[1,3],elo[[3]],elo[[4]]) 
      as = rbind(as,z)
      tab = rbind(x,y,tab)
      tab = arrange(tab,desc(Data))
      tab = tab[!duplicated(tab$nome),]
      tib = rbind(x,y,tib)
      tib = arrange(tib,desc(Data))
      
    } else if (jogo[1,6]-jogo[1,7]<0){
      elo= fun_logistica(x[1,3],y[1,3],0,1,k,jogo[1,6]-jogo[1,7])
      x = list(x[1,1],x[1,2],elo[[1]],jogo[1,1])
      y = list(y[1,1],y[1,2],elo[[2]],jogo[1,1])
      z = list(jogo[1,1],jogo[1,2],jogo[1,6],jogo[1,7],jogo[1,3],elo[[3]],elo[[4]]) 
      as = rbind(as,z)
      tab = rbind(x,y,tab)
      tab = arrange(tab,desc(Data))
      tab = tab[!duplicated(tab$nome),]
      tib = rbind(x,y,tib)
      tib = arrange(tib,desc(Data))
      
    } else {
      elo= fun_logistica(x[1,3],y[1,3],0.5,0.5,k,0)
      x = list(x[1,1],x[1,2],elo[[1]],jogo[1,1])
      y = list(y[1,1],y[1,2],elo[[2]],jogo[1,1])
      z = list(jogo[1,1],jogo[1,2],jogo[1,6],jogo[1,7],jogo[1,3],elo[[3]],elo[[4]])
      as = rbind(as,z)
      tab = rbind(x,y,tab)
      tab = arrange(tab,desc(Data))
      tab = tab[!duplicated(tab$nome),]
      tib = rbind(x,y,tib)
      tib = arrange(tib,desc(Data))
      
    }
  
  }
  as = as[-1,] %>% arrange(desc(Data))
  tab = tab[tab$Data!=dmy('01-01-1000'),]
  tib = tib[tib$Data!=dmy('01-01-1000'),]
  tib = arrange(tib,desc(Data))
  return(list(as,tib,tab))
  
}
```
```{r eval = F, results = 'asis'}

###Esta funcao demora bastante, pode ir almoçar e depois tomar um café!
sulamerica = lapply(1:10, function(i) elo_rating(serieA[[i]]))
sulamerica[[11]] = elo_rating(serieA[[11]])

```

Finalmente o objetivo! A tabela a seguir exibe o Ranking Sul-Americano de equipes de futebol.

```{r include=F }
setwd("C:/Users/joao passeri/Desktop/projeto final/markdown")
kaka = read.csv("total_jogos.csv")
kaka = kaka[,-1]
```
```{r echo=FALSE, message=FALSE, warning=F}
kaka %>% head(10) %>%DT::datatable()
```

### Análise Exploratória.

Nesta seção será realizado um passeio pelos dados verificando importantes resultados e algumas curiosidades.
Os gráficos foram feitos utilizando a bliblioteca *ggplot2*.

##### Quantidade de partidas por ano.

````{r echo=F}
rate_por_ano = function(df,x){
  df = lapply(1:length(x), function(i) df[str_detect(df$nome,x[i]),])
  df = lapply(1:length(x), function(i) mutate(df[[i]], Data = year(df[[i]]$Data)))
  df = lapply(1:length(x), function(i) df[[i]][!duplicated(df[[i]]$Data),]) %>% join_all(type = 'full')
  return(df)
}

kaka = mutate(jogos, ano = year(Data))

dance = ggplot(kaka,aes(ano))+
  geom_histogram(binwidth = 1,color="black",fill="darkslategray")+
  labs(title = "Histograma dos Dados",
       x = "Ano",
       y = "Quantidade de dados")+
  guides(fill=FALSE)+
  theme_bw() 

dance
```
```{r include=F}
setwd("C:/Users/joao passeri/Desktop/projeto final/markdown")
sulamerica = read.csv("sulamerica12.csv")
```

##### Desempenho dos clubes do Rio de Janeiro.

```{r echo=FALSE, message=FALSE}
kaka = sulamerica
times = c('Flamengo RJ',"Vasco da Gama","Fluminense RJ",'Botafogo - RJ')
kaka = rate_por_ano(kaka,times)

rio = ggplot(data = kaka, mapping = aes(x = Data, y = novo_rate, color = nome)) +
  geom_line() +
  geom_point(aes(shape = nome))  +
  labs(title = "Evolução do Rate:",
       subtitle = "Rio de Janeiro",
       x = "Ano",
       y = "Rate",
       color = "Equipe",
       shape = "Equipe")+
  theme_bw() + 
  scale_color_manual(values=c("black", "red", "green",'blue'))

rio
```

##### Desempenho dos clubes de São Paulo.

```{r echo=FALSE, message=FALSE}
kaka = sulamerica
times = c('Palmeiras',"São Paulo FC","Santos FC",'Corinthians SP')
kaka = rate_por_ano(kaka,times)

sp = ggplot(data = kaka, mapping = aes(x = Data, y = novo_rate, color = nome)) +
  geom_line() +
  geom_point(aes(shape = nome))  +
  labs(title = "Evolução do Rate:",
       subtitle = "São Paulo",
       x = "Ano",
       y = "Rate",
       color = "Equipe",
       shape = "Equipe")+
  theme_bw() + 
  scale_color_manual(values=c("black",'green',"red",'blue'))

sp
```

##### Desempenho Clube dos 13.

```{r echo=FALSE, message=FALSE}
kaka = sulamerica
times = c('Palmeiras',"São Paulo FC","Santos FC",'Corinthians SP','Flamengo RJ',
          "Vasco da Gama","Fluminense RJ",'Botafogo - RJ','Cruzeiro','Atlético Mineiro',
          'Grêmio Porto Alegre','Internacional','Bahia - BA')
kaka = rate_por_ano(kaka,times)

rjsp = ggplot(data = kaka, mapping = aes(x = Data, y = novo_rate, color = nome)) +
  geom_line() +
  labs(title = "Evolução do Rate:",
       subtitle = "Clube dos 13",
       x = "Ano",
       y = "Rate",
       color = "Equipe",
       shape = "Equipe")+
  theme_bw()

rjsp
```

##### Mais meses na liderança do ranking brasileiro.

```{r echo=FALSE, message=FALSE}
kaka = sulamerica %>% mutate(ano = year(Data),mes = month(Data),
                                       semana=ceiling(day(Data)/7))
x = to_vec(for(i in 1:nrow(kaka)) if(kaka[i,7]==5) i)
y = kaka[x,] %>% mutate(semana = 4)
kaka = kaka[-x,]
kaka = rbind(kaka,y)

kaka = mutate(kaka,Data2 = as.numeric(str_c(kaka$ano,kaka$mes,kaka$semana)),
              anomes = as.numeric(str_c(kaka$ano,kaka$mes))) %>%
  arrange(desc(Data2),desc(novo_rate))
kaka = kaka[!duplicated(kaka$anomes),] %>% arrange(desc(Data))
kaka = kaka[kaka$mes>3,]
kaka = count(kaka$nome) %>% arrange(desc(freq))
names(kaka) = c('Equipes','freq')


lideres =ggplot(kaka, aes(x=reorder(Equipes,-freq), y=freq)) +
  geom_bar(aes(fill = Equipes), stat="identity") +
  coord_flip()+
  labs(title="Meses na liderança",
       subtitle = "Ranking brasileiro",
       x = 'Equipes',
       y = 'Meses')+
  theme_bw()


lideres


```

##### Aproveitamento dos mandantes pela federação.


```{r include=F}
setwd("C:/Users/joao passeri/Desktop/projeto final/markdown")
mandantes_federacao = read.csv("mandantes_federacao.csv")

```
```{r echo=FALSE, message=FALSE}
mp =ggplot(mandantes_federacao, aes(x = reorder(pais,aproveitamento_mandantes),
                                    y = aproveitamento_mandantes)) +
  geom_col(aes(fill = pais)) +
  scale_y_continuous(labels = percent_format())+
  labs(title = "Aproveitamento Mandantes",
       subtitle = "Por Federações",
       x = "País",
       y = "Aproveitamento") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

mp
```

##### Tabela de Aproveitamento mandante, empate e visitante pelas fedarações.


```{r include=F}
setwd("C:/Users/joao passeri/Desktop/projeto final/markdown")
kaka0 = read.csv("kaka0.csv")
```
```{r echo=FALSE, message=FALSE }
kaka0 %>% DT::datatable()
```

##### Aproveitamento em porcentagem de cada federação pela Copa Libertadores

  Para calcular este aproveitamento não foram considerados os empates. O aproveitamento está linha x coluna.
Exemplo:
  Na linha 1 com coluna 2 está representando aproveitamento dos clubes brasileiros contra os clubes argentinos pela Copa Libertadores. Os brasileiros ganharam 50,28% dos jogos contra os argentinos, excluindo os empates.

    
```{r include=F, message=F }

setwd("C:/Users/joao passeri/Desktop/projeto final/markdown")
kaka1 = read.csv("kaka1.csv")

```
```{r echo=F, message=FALSE }
kaka1 %>% DT::datatable()

```

##### Os dez placares que mais se repetiram considerando todos os jogos e campeonatos.

```{r include=F, message=F}
setwd("C:/Users/joao passeri/Desktop/projeto final/markdown")
kaka2 = read.csv("kaka2.csv")

```
```{r echo=FALSE, message=FALSE}
placares = lideres =ggplot(kaka2, aes(x=reorder(x,-freq), y=freq))+
  geom_bar(aes(fill = x), stat="identity")+
  labs(title="Dez placares que mais se repetiram",
       x = 'Placares',
       y = 'Frequência')+
  guides(fill=FALSE)+
  theme_bw()


placares

```

##### Média de gols por jogo a cada ano, e linha de tendência.

```{r include=F}
setwd("C:/Users/joao passeri/Desktop/projeto final/markdown")
kaka3 = read.csv("kaka3.csv")

```
```{r echo=FALSE, message=FALSE}
g = ggplot(kaka3, aes(y = x, x = Group.1)) + 
  geom_point(aes(colour = Ponto)) +
  scale_color_manual(values = 'black') +
  geom_smooth(mapping = aes(linetype = 'Tendência'),
              color = 'red') +
  labs(title = "Média de Gols por Ano",
       subtitle = "América do Sul",
       x = "Ano",
       y = "Média de Gols",
       color = "",
       linetype = '') +
  theme_bw()

g

```

Com o modelo e a análise realizada sobre os dados podemos ter uma ótima refêrencia sobre o desempenho dos clubes e ainda matar alguamas curiosidades. Como foi o desempenho do seu time do coração ?  