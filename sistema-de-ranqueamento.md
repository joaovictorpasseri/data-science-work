Sistema de Ranqueamento para Equipes de Futebol Sul-Americanos
================
João Victor Passeri Medeiros Amorim

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

Este trabalho foi desenvolvido como objetivo criar um modelo de
ranqueamento baseado no [Sistema
Elo](https://pt.wikipedia.org/wiki/Rating_ELO) para os times de futebol
da América do Sul. O modelo de ranqueamento foi aplicado em mais de 70
mil partidas de futebol dos principais campeonatos nacionais e
internacionais da América do Sul. Ao final foi obtido um ranking de
clubes e algumas análise e curiosidades do futebol sul-americano.

Para alcançar o objetivo, foram realizadas as seguintes tarefas:

  - Webscraping.
  - Manipulação, limpeza e tratamento dos dados.
  - Modelagem e aplicação do modelo.

### Raspagem e obtenção de dados do WorldFooteboll.

Foram criadas funções que facilitam e e automatizam o trabalho de
webscraping do site <https://www.worldfootball.net>. Tais funções
organizam os campeonatos em tabelas e exportam arquivos em formato csv.

``` r
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

As funções feitas foram aplicadas aos campeonados e fedarações de
interesse do trabalho.

### Manipulação dos dados.

Após a raspagem e união de todos os 28 campeonatos e temporadas em uma
única planilha, a seguinte base foi obtida:

<table class="table table-condensed">

<thead>

<tr>

<th style="text-align:right;">

Data

</th>

<th style="text-align:right;">

Mandante

</th>

<th style="text-align:right;">

Placar

</th>

<th style="text-align:right;">

Visitante

</th>

<th style="text-align:right;">

Campeonato

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

27/07/2019

</td>

<td style="text-align:right;">

Racing Club

</td>

<td style="text-align:right;">

0:0 (0:0)

</td>

<td style="text-align:right;">

Unión de Santa Fe

</td>

<td style="text-align:right;">

Argentina Serie A

</td>

</tr>

<tr>

<td style="text-align:right;">

</td>

<td style="text-align:right;">

Colón de Santa Fe

</td>

<td style="text-align:right;">

0:1 (0:1)

</td>

<td style="text-align:right;">

Patronato de Paraná

</td>

<td style="text-align:right;">

Argentina Serie A

</td>

</tr>

<tr>

<td style="text-align:right;">

</td>

<td style="text-align:right;">

San Lorenzo

</td>

<td style="text-align:right;">

3:2 (1:0)

</td>

<td style="text-align:right;">

Godoy Cruz

</td>

<td style="text-align:right;">

Argentina Serie A

</td>

</tr>

<tr>

<td style="text-align:right;">

</td>

<td style="text-align:right;">

Lanús

</td>

<td style="text-align:right;">

1:1 (1:0)

</td>

<td style="text-align:right;">

Gimnasia de La Plata

</td>

<td style="text-align:right;">

Argentina Serie A

</td>

</tr>

<tr>

<td style="text-align:right;">

28/07/2019

</td>

<td style="text-align:right;">

Argentinos Juniors

</td>

<td style="text-align:right;">

1:1 (1:0)

</td>

<td style="text-align:right;">

River Plate

</td>

<td style="text-align:right;">

Argentina Serie A

</td>

</tr>

</tbody>

</table>

Foi necessário completar as datas dos jogos, uma vez que são importantes
para o modelo. Os placares são da classe string e informados de várias
formas diferentes (alguns erros foram encontrados nos placares). O
código abaixo realiza todo o tratamento da base de dados e separa em
data frames que facilitam o trabalho. Os principais data frames são:

  - df.times
  - jogos
  - nacao

#### Estas funções organizam a base e retornam data.frames.

``` r
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

#### Problemas.

Alguns times internacionais tinham o mesmo nome na base de dados, e não
possuíam nenhuma informação para diferenciá-los. Entrei em contato com o
site para propor uma solução simples, que foi negada. Pela Copa
Libertadores apenas duas equipes diferentes e em uma edição aparecem com
mesmo nome, River Plate URU e River Plate ARG, porém pela Copa
Sudamericana isso ocorre com mais equipes e em mais edições,
impossibilitando uma correção manual. Portanto optei por não trabalhar
mais com este campeonato.

### Modelo.

O modelo [Elo](https://pt.wikipedia.org/wiki/Rating_ELO) supõe que o
rate do jogador é uma variável aleatória gaussiana. Sendo \(R_a\) e
\(R_b\) os rates atuais de cada jogador, a função logística que retorna
a esperança de vitória de cada jogador é:

\(E_a = \cfrac{1}{1 + 10^\frac{R_b - R_a}{400}}\)

\(E_b = 1 - E_a\)

Já a fórmula pára atualizar o rate do jogador após o resultado da partid
é:

\(R'_a = R_a + K(S - E_a)\sqrt{dif}\)

\(K\) assume dois valores \(\{20,40\}\), sendo \(20\) para os
campeonatos nacionais e \(40\) para os internacionais. \(S\) assume três
valores, \(\{1,0.5,0\}\), em caso de vitória, empate e derrota
respectivamente. \(dif\) é a diferença de gols entre vencedor e perdedor
e assume o valor \(1\) em caso de empate.

#### Neste script criamos o modelo e aplicamos a base de dados, separando os resultados pra posterior análise.

``` r
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

``` r
###Esta funcao demora bastante, pode ir almoçar e depois tomar um café!
sulamerica = lapply(1:10, function(i) elo_rating(serieA[[i]]))
sulamerica[[11]] = elo_rating(serieA[[11]])
```

Finalmente o objetivo\! A tabela a seguir exibe o Ranking Sul-Americano
de equipes de futebol.

<table class="table table-condensed">

<thead>

<tr>

<th style="text-align:right;">

nome

</th>

<th style="text-align:right;">

pais

</th>

<th style="text-align:right;">

novo\_rate

</th>

<th style="text-align:right;">

Data

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

Club Olimpia

</td>

<td style="text-align:right;">

Paraguay

</td>

<td style="text-align:right;">

2038.709

</td>

<td style="text-align:right;">

2020-03-12

</td>

</tr>

<tr>

<td style="text-align:right;">

Libertad

</td>

<td style="text-align:right;">

Paraguay

</td>

<td style="text-align:right;">

1983.740

</td>

<td style="text-align:right;">

2020-03-11

</td>

</tr>

<tr>

<td style="text-align:right;">

Flamengo RJ

</td>

<td style="text-align:right;">

Brazil

</td>

<td style="text-align:right;">

1965.579

</td>

<td style="text-align:right;">

2020-03-12

</td>

</tr>

<tr>

<td style="text-align:right;">

Nacional

</td>

<td style="text-align:right;">

Uruguay

</td>

<td style="text-align:right;">

1952.757

</td>

<td style="text-align:right;">

2020-03-12

</td>

</tr>

<tr>

<td style="text-align:right;">

River Plate

</td>

<td style="text-align:right;">

Argentina

</td>

<td style="text-align:right;">

1940.176

</td>

<td style="text-align:right;">

2020-03-11

</td>

</tr>

<tr>

<td style="text-align:right;">

Peñarol

</td>

<td style="text-align:right;">

Uruguay

</td>

<td style="text-align:right;">

1927.767

</td>

<td style="text-align:right;">

2020-03-11

</td>

</tr>

<tr>

<td style="text-align:right;">

Boca Juniors

</td>

<td style="text-align:right;">

Argentina

</td>

<td style="text-align:right;">

1924.796

</td>

<td style="text-align:right;">

2020-03-11

</td>

</tr>

<tr>

<td style="text-align:right;">

Palmeiras

</td>

<td style="text-align:right;">

Brazil

</td>

<td style="text-align:right;">

1913.997

</td>

<td style="text-align:right;">

2020-03-11

</td>

</tr>

<tr>

<td style="text-align:right;">

Santos FC

</td>

<td style="text-align:right;">

Brazil

</td>

<td style="text-align:right;">

1870.755

</td>

<td style="text-align:right;">

2020-03-10

</td>

</tr>

<tr>

<td style="text-align:right;">

Grêmio Porto Alegre

</td>

<td style="text-align:right;">

Brazil

</td>

<td style="text-align:right;">

1866.876

</td>

<td style="text-align:right;">

2020-03-13

</td>

</tr>

</tbody>

</table>

### Análise Exploratória.

Nesta seção será realizado um passeio pelos dados verificando
importantes resultados e algumas curiosidades. Os gráficos foram feitos
utilizando a bliblioteca *ggplot2*.

##### Quantidade de partidas por ano.

![](sistema-de-ranqueamento_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

##### Desempenho dos clubes do Rio de Janeiro.

![](sistema-de-ranqueamento_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

##### Desempenho dos clubes de São Paulo.

![](sistema-de-ranqueamento_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

##### Desempenho Clube dos 13.

![](sistema-de-ranqueamento_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

##### Mais meses na liderança do ranking brasileiro.

![](sistema-de-ranqueamento_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

##### Aproveitamento dos mandantes pela federação.

![](sistema-de-ranqueamento_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

##### Tabela de Aproveitamento mandante, empate e visitante pelas fedarações.

<table class="table table-condensed">

<thead>

<tr>

<th style="text-align:right;">

X

</th>

<th style="text-align:right;">

Brazil

</th>

<th style="text-align:right;">

Argentina

</th>

<th style="text-align:right;">

Chile

</th>

<th style="text-align:right;">

Bolivia

</th>

<th style="text-align:right;">

Colombia

</th>

<th style="text-align:right;">

Ecuador

</th>

<th style="text-align:right;">

Paraguay

</th>

<th style="text-align:right;">

Peru

</th>

<th style="text-align:right;">

Uruguay

</th>

<th style="text-align:right;">

Venezuela

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

Mandante

</td>

<td style="text-align:right;">

49.57

</td>

<td style="text-align:right;">

44.47

</td>

<td style="text-align:right;">

46.35

</td>

<td style="text-align:right;">

56.70

</td>

<td style="text-align:right;">

47.78

</td>

<td style="text-align:right;">

47.69

</td>

<td style="text-align:right;">

39.89

</td>

<td style="text-align:right;">

52.60

</td>

<td style="text-align:right;">

40.66

</td>

<td style="text-align:right;">

47.03

</td>

</tr>

<tr>

<td style="text-align:right;">

Empate

</td>

<td style="text-align:right;">

25.85

</td>

<td style="text-align:right;">

28.84

</td>

<td style="text-align:right;">

24.63

</td>

<td style="text-align:right;">

21.35

</td>

<td style="text-align:right;">

29.05

</td>

<td style="text-align:right;">

25.85

</td>

<td style="text-align:right;">

26.37

</td>

<td style="text-align:right;">

25.97

</td>

<td style="text-align:right;">

24.58

</td>

<td style="text-align:right;">

28.13

</td>

</tr>

<tr>

<td style="text-align:right;">

Visitante

</td>

<td style="text-align:right;">

24.58

</td>

<td style="text-align:right;">

26.69

</td>

<td style="text-align:right;">

29.02

</td>

<td style="text-align:right;">

21.95

</td>

<td style="text-align:right;">

23.17

</td>

<td style="text-align:right;">

26.46

</td>

<td style="text-align:right;">

33.74

</td>

<td style="text-align:right;">

21.43

</td>

<td style="text-align:right;">

34.76

</td>

<td style="text-align:right;">

24.84

</td>

</tr>

</tbody>

</table>

##### Aproveitamento em porcentagem de cada federação pela Copa Libertadores

Para calcular este aproveitamento não foram considerados os empates. O
aproveitamento está linha x coluna. Exemplo: Na linha 1 com coluna 2
está representando aproveitamento dos clubes brasileiros contra os
clubes argentinos pela Copa Libertadores. Os brasileiros ganharam 50,28%
dos jogos contra os argentinos, excluindo os empates.

<table class="table table-condensed">

<thead>

<tr>

<th style="text-align:right;">

X

</th>

<th style="text-align:right;">

Brazil

</th>

<th style="text-align:right;">

Argentina

</th>

<th style="text-align:right;">

Chile

</th>

<th style="text-align:right;">

Bolivia

</th>

<th style="text-align:right;">

Colombia

</th>

<th style="text-align:right;">

Ecuador

</th>

<th style="text-align:right;">

Paraguay

</th>

<th style="text-align:right;">

Peru

</th>

<th style="text-align:right;">

Uruguay

</th>

<th style="text-align:right;">

Venezuela

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

Brazil

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

50.28

</td>

<td style="text-align:right;">

65.00

</td>

<td style="text-align:right;">

72.63

</td>

<td style="text-align:right;">

66.67

</td>

<td style="text-align:right;">

66.33

</td>

<td style="text-align:right;">

67.11

</td>

<td style="text-align:right;">

79.59

</td>

<td style="text-align:right;">

67.18

</td>

<td style="text-align:right;">

91.55

</td>

</tr>

<tr>

<td style="text-align:right;">

Argentina

</td>

<td style="text-align:right;">

49.72

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

68.60

</td>

<td style="text-align:right;">

69.88

</td>

<td style="text-align:right;">

64.93

</td>

<td style="text-align:right;">

66.34

</td>

<td style="text-align:right;">

57.50

</td>

<td style="text-align:right;">

70.94

</td>

<td style="text-align:right;">

62.50

</td>

<td style="text-align:right;">

82.09

</td>

</tr>

<tr>

<td style="text-align:right;">

Chile

</td>

<td style="text-align:right;">

35.00

</td>

<td style="text-align:right;">

31.40

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

64.44

</td>

<td style="text-align:right;">

46.05

</td>

<td style="text-align:right;">

59.72

</td>

<td style="text-align:right;">

39.37

</td>

<td style="text-align:right;">

64.38

</td>

<td style="text-align:right;">

45.45

</td>

<td style="text-align:right;">

77.55

</td>

</tr>

<tr>

<td style="text-align:right;">

Bolivia

</td>

<td style="text-align:right;">

27.37

</td>

<td style="text-align:right;">

30.12

</td>

<td style="text-align:right;">

35.56

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

36.84

</td>

<td style="text-align:right;">

42.86

</td>

<td style="text-align:right;">

29.33

</td>

<td style="text-align:right;">

32.26

</td>

<td style="text-align:right;">

35.29

</td>

<td style="text-align:right;">

82.35

</td>

</tr>

<tr>

<td style="text-align:right;">

Colombia

</td>

<td style="text-align:right;">

33.33

</td>

<td style="text-align:right;">

35.07

</td>

<td style="text-align:right;">

53.95

</td>

<td style="text-align:right;">

63.16

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

61.67

</td>

<td style="text-align:right;">

48.62

</td>

<td style="text-align:right;">

67.16

</td>

<td style="text-align:right;">

49.41

</td>

<td style="text-align:right;">

78.79

</td>

</tr>

<tr>

<td style="text-align:right;">

Ecuador

</td>

<td style="text-align:right;">

33.67

</td>

<td style="text-align:right;">

33.66

</td>

<td style="text-align:right;">

40.28

</td>

<td style="text-align:right;">

57.14

</td>

<td style="text-align:right;">

38.33

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

47.13

</td>

<td style="text-align:right;">

56.86

</td>

<td style="text-align:right;">

29.81

</td>

<td style="text-align:right;">

52.63

</td>

</tr>

<tr>

<td style="text-align:right;">

Paraguay

</td>

<td style="text-align:right;">

32.89

</td>

<td style="text-align:right;">

42.50

</td>

<td style="text-align:right;">

60.63

</td>

<td style="text-align:right;">

70.67

</td>

<td style="text-align:right;">

51.38

</td>

<td style="text-align:right;">

52.87

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

65.43

</td>

<td style="text-align:right;">

44.76

</td>

<td style="text-align:right;">

75.41

</td>

</tr>

<tr>

<td style="text-align:right;">

Peru

</td>

<td style="text-align:right;">

20.41

</td>

<td style="text-align:right;">

29.06

</td>

<td style="text-align:right;">

35.62

</td>

<td style="text-align:right;">

67.74

</td>

<td style="text-align:right;">

32.84

</td>

<td style="text-align:right;">

43.14

</td>

<td style="text-align:right;">

34.57

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

34.25

</td>

<td style="text-align:right;">

54.05

</td>

</tr>

<tr>

<td style="text-align:right;">

Uruguay

</td>

<td style="text-align:right;">

32.82

</td>

<td style="text-align:right;">

37.50

</td>

<td style="text-align:right;">

54.55

</td>

<td style="text-align:right;">

64.71

</td>

<td style="text-align:right;">

50.59

</td>

<td style="text-align:right;">

70.19

</td>

<td style="text-align:right;">

55.24

</td>

<td style="text-align:right;">

65.75

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

81.40

</td>

</tr>

<tr>

<td style="text-align:right;">

Venezuela

</td>

<td style="text-align:right;">

8.45

</td>

<td style="text-align:right;">

17.91

</td>

<td style="text-align:right;">

22.45

</td>

<td style="text-align:right;">

17.65

</td>

<td style="text-align:right;">

21.21

</td>

<td style="text-align:right;">

47.37

</td>

<td style="text-align:right;">

24.59

</td>

<td style="text-align:right;">

45.95

</td>

<td style="text-align:right;">

18.60

</td>

<td style="text-align:right;">

NA

</td>

</tr>

</tbody>

</table>

##### Os dez placares que mais se repetiram considerando todos os jogos e campeonatos.

![](sistema-de-ranqueamento_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

##### Média de gols por jogo a cada ano, e linha de tendência.

![](sistema-de-ranqueamento_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

Com o modelo e a análise realizada sobre os dados podemos ter uma ótima
refêrencia sobre o desempenho dos clubes e ainda matar alguamas
curiosidades. Como foi o desempenho do seu time do coração ?
