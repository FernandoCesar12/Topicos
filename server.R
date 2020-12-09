#########################################################################################
#                         Entrando com o banco de dados para o sistema
#########################################################################################

dados <- readRDS("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Topicos\\dados_14_19.rds", refhook = NULL)

municipios <-  unique(dados$`Nome do Município`)

dados2 <- readRDS("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Topicos\\Topicos.rds", refhook = NULL)

names(dados2) <- c('Ano','Região','Sigla da UF','Código do Município','Latitude','Longitude','Tamanho da População','Nome do Município','Localização','Dependência Administrativa',
                   'Percentual de docentes 1º ao 5º Ano','Taxa  de distorcao 1º ao 5º Ano','Taxa  de distorcao 1º Ano','Taxa  de distorcao 2º Ano','Taxa  de distorcao 3º Ano',
                   'Taxa  de distorcao 4º Ano','Taxa  de distorcao 5º Ano','IDEB','1º ao 5º Ano - Taxa de Promoção','1º ao 5º Ano - Taxa de Repetência','1º ao 5º Ano - Taxa de Evasão',
                   '1º ao 5º Ano - Migração para EJA','1º Ano - Taxa de Promoção','1º Ano - Taxa de Repetência','1º Ano - Taxa de Evasão','1º Ano - Migração para EJA','2º Ano - Taxa de Promoção',
                   '2º Ano - Taxa de Repetência','2º Ano - Taxa de Evasão','2º Ano - Migração para EJA','3º Ano - Taxa de Promoção','3º Ano - Taxa de Repetência','3º Ano - Taxa de Evasão','3º Ano - Migração para EJA',
                   '4º Ano - Taxa de Promoção','4º Ano - Taxa de Repetência','4º Ano - Taxa de Evasão','4º Ano - Migração para EJA','5º Ano - Taxa de Promoção','5º Ano - Taxa de Repetência','5º Ano - Taxa de Evasão',
                   '5º Ano - Migração para EJA','Aprovação - Anos Iniciais (1º ao 5º Ano)','Aprovação no 1º Ano','Aprovação no 2º Ano','Aprovação no 3º Ano','Aprovação no 4º Ano','Aprovação no 5º Ano','Reprovação - Anos Iniciais (1º ao 5º Ano)',
                   'Reprovação no 1º Ano','Reprovação no 2º Ano','Reprovação no 3º Ano','Reprovação no 4º Ano','Reprovação no 5º Ano','Abandono - Anos Iniciais (1º ao 5º Ano)','Abandono no 1º Ano','Abandono no 2º Ano','Abandono no 3º Ano',
                   'Abandono no 4º Ano','Abandono no 5º Ano','Grupo 1','Grupo 2','Grupo 3','Grupo 4','Grupo 5','Projeções')


#############################################################################################################
#                                       Entrando com identificador de usuário
#############################################################################################################

login_details <- data.frame(user = c("sam", "pam", "ron"),     # Logins e Senhas para acesso
                            pswd = c("123", "123", "123"))
login <- box(
  title = "Login",height = 270,
  textInput("userName", "Username"),
  passwordInput("passwd", "Password"),    # Estruturando a caixa de login que fica na página principal
  br(),
  actionButton("Login", "Log in"),
  div(class="topimg",img(src = "https://raw.githubusercontent.com/FernandoCesar12/TCC/master/Slogan_removido.png", height = 350, width = 400, align = "right")) # Adicionando a foto da logo na pagina de login
)


# Entrando com o server

server <- shinyServer(function(input, output, session){ 
  
  # Comando para retornar a página de login
  
  login.page = paste(
    isolate(session$clientData$url_protocol),
    "//",
    isolate(session$clientData$url_hostname),
    ":",
    isolate(session$clientData$url_port),
    sep = ""
  )
  
  USER <- reactiveValues(Logged = F)
  observe({
    
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(login_details$user %in% Username)
          Id.password <- which(login_details$pswd %in% Password)
          if (length(Id.username) > 0 & length(Id.password) > 0){
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            }
          }
        }
      }
    }
  })
  output$sidebarpanel <- renderUI({
    if (USER$Logged == TRUE) {
      div(
        div(class='login',
            sidebarUserPanel(
              isolate(input$userName),
              subtitle = a("Logout", href = login.page), 
              image = "https://image.flaticon.com/icons/svg/892/892781.svg")), #Serve para adicionar a foto do usuário
        
        sidebarMenu(
          
          # É necessário que parte da interface pessoal (SidebarMenu) esteja no server já que ele vai ser ocultado enquanto
          # não entrarem com o login e a senha 
          
          menuItem("Descrição dos Indicadores", tabName = "aba_1",icon = icon("user-tie")),  # Criando a primeira aba 
          
          menuItem("Dashboard Municipal", icon = icon("tasks"),
                   
                   menuSubItem(icon = NULL,
                               selectInput("aba_2_1", "Selecione o indicador de interesse:", 
                                           choices=c("IDEB","Migração para EJA",
                                                     "Percentual de docentes com curso superior",
                                                     "Taxa de Abandono","Taxa de Aprovação",
                                                     "Taxa de distorção idade-série",
                                                     "Taxa de Evasão","Taxa de Promoção","Taxa de Repetência" ,
                                                     "Taxa de Reprovação")), tabName = "aba_2"),
                   
                   
                   menuSubItem(icon = NULL,
                               selectInput("aba_2_2", label = ("Selecione a série de análise:"),
                                           choices = unique(dados$serie),selected = FALSE,multiple = TRUE), tabName = "aba_2"),
                   
                   menuSubItem(icon = NULL,
                               selectInput("aba_2_3", label = ("Selecione a localização:"),
                                           choices = unique(dados$`Localização`),selected = FALSE), tabName = "aba_2"),
                   menuSubItem(icon = NULL,
                               selectInput("aba_2_6", "Selecione os estados de interesse:",
                                           choices=sort(unique(dados$`Sigla da UF`)), multiple = FALSE), tabName = "aba_2"),
                   menuSubItem(icon = NULL,
                               selectInput("aba_2_4", "Selecione os municípios de interesse:", 
                                           choices=NULL, multiple = TRUE), tabName = "aba_2"),
                   
                   menuSubItem(icon = NULL,
                               actionButton("aba_2_5", "Visualizar"))
                   
          ), # Criando a segunda aba 
          
          menuItem("Dashboard Estadual", icon = icon("chart-line"),
                   
                   menuSubItem(icon = NULL,
                               selectInput("aba_3_1", "Selecione o indicador de interesse:", 
                                           choices=c("IDEB","Migração para EJA",
                                                     "Percentual de docentes com curso superior",
                                                     "Taxa de Abandono","Taxa de Aprovação",
                                                     "Taxa de distorção idade-série",
                                                     "Taxa de Evasão","Taxa de Promoção","Taxa de Repetência" ,
                                                     "Taxa de Reprovação")), tabName = "aba_3"),
                   
                   
                   menuSubItem(icon = NULL,
                               selectInput("aba_3_2", label = ("Selecione a série de análise:"),
                                           choices = unique(dados$serie),selected = FALSE,multiple = TRUE), tabName = "aba_3"),
                   
                   menuSubItem(icon = NULL,
                               selectInput("aba_3_3", label = ("Selecione a localização:"),
                                           choices = unique(dados$`Localização`), 
                                           selected = FALSE), tabName = "aba_3"),
                   
                   menuSubItem(icon = NULL,
                               selectInput("aba_3_4", "Selecione os estados de interesse:", 
                                           choices=sort(unique(dados$`Sigla da UF`)), multiple = TRUE), tabName = "aba_3"),
                   
                   menuSubItem(icon = NULL,
                               actionButton("aba_3_5", "Visualizar"))
          ), # Criando a quarta aba
          
          menuItem("Mapa de Desempenho", icon = icon("calendar"), # Criando a quinta aba
                   
                   menuSubItem(icon = NULL,
                               selectInput("aba_5_1", "Selecione o indicador de interesse:", 
                                           choices=c("Taxa de distorção idade-série","Taxas de rendimento (abandono, aprovação, reprovação)",
                                                     "Percentual de docentes com curso superior", "Taxas de transição",
                                                     "Ideb")), tabName = "aba_5"),
                   
                   conditionalPanel(
                     condition = "input.aba_5_1 == 'Taxas de rendimento (abandono, aprovação, reprovação)'",
                     selectInput("aba_5_1_1", "Selecione a opção de interesse", choices = c('Abandono','Aprovação','Reprovação')), tabName = "aba_5"),
                   
                   
                   conditionalPanel(
                     condition = "input.aba_5_1 == 'Taxas de transição'",
                     selectInput("aba_5_1_2", "Selecione a opção de interesse", choices = c('Taxa de Promoção','Taxa de Repetência','Taxa de Evasão','Migração para EJA')), tabName = "aba_5"),
                   
                   
                   menuSubItem(icon = NULL,
                               selectInput("aba_5_4", label = ("Selecione o ano de análise:"),
                                           choices = list("2014", "2019"), 
                                           selected = FALSE), tabName = "aba_5"),
                   
                   menuSubItem(icon = NULL,
                               selectInput("aba_5_4_1", "Selecione os estados de interesse:", 
                                           choices=sort(unique(dados$`Sigla da UF`)), multiple = TRUE), tabName = "aba_5"),
                   
                   menuSubItem(icon = NULL,
                               selectInput("aba_5_2", label = ("Selecione o indice de desempenho por municipio:"),
                                           choices = list("15 melhores/piores " = 15,"10 melhores/piores" = 10, "5 melhores/piores" = 5), 
                                           selected = FALSE), tabName = "aba_5"),
                   
                   menuSubItem(icon = NULL,
                               actionButton("aba_5_3", "Visualizar")))
          
          
        )
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$Logged == TRUE) {
      
    } else {   # Final do código para login 
      login
    }
  })
  
  
  observe({  # Serve para criar um sistema de atualização automático da página (temporizador)
    
    invalidateLater(120000000000000000000000000, session) # Código programado para atualizar a cada 2 minutos
    
    observeEvent(input$aba_2_6, {
      updateSelectInput(session, "aba_2_4", 
                        choices = unique(dados$`Nome do Município`[dados$`Sigla da UF` == input$aba_2_6]))
    })
    ############################# Estruturando os Info gráficos para a Aba número 2 ###########################
    
    observeEvent(input$aba_2_5,{
      
      if(input$aba_2_1 == "IDEB"){
        
        dados_aba_2 <- dados[,c(1:11,12)]
        dados_aba_2 <- dados_aba_2[dados_aba_2$serie %in% input$aba_2_2,]
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- na.omit(info)
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Nome do Município`%in% input$aba_2_4,]
        
        dados_IDEB <- dados_aba_2 %>%
          select(Ano, `Sigla da UF`, `Nome do Município`, serie, IDEB) %>% 
          mutate(valor_z = round((IDEB-mean(IDEB, na.rm = T))/sd(IDEB, na.rm = T), 2))
        nome_indicador <- paste("IDEB")
        
        output$grafico_aba2_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_IDEB %>% 
            select(Ano,`Sigla da UF`, `Nome do Município`, serie, IDEB) %>% 
            filter(!is.na(IDEB)) %>% 
            spread(Ano, IDEB) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        output$grafico_aba2_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_IDEB%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",IDEB,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba2_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_IDEB %>% filter(Ano == 2014) %>% 
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = IDEB, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                   "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "IDEB")
          
          
        })
        
        output$grafico_aba2_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_IDEB %>% filter(Ano == 2019) %>% 
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = IDEB, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                   "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",   fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "IDEB")
        })
      }
      
      if(input$aba_2_1 == "Migração para EJA"){
        
        dados_aba_2 <- dados[,c(1:11,13)]
        dados_aba_2 <- dados_aba_2[dados_aba_2$serie %in% input$aba_2_2,]
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- na.omit(info)
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Nome do Município`%in% input$aba_2_4,]
        
        dados_aba_2 <- dados_aba_2 %>%
          select(Ano, `Sigla da UF`, `Nome do Município`, serie, `Migração para EJA`) %>% 
          rename(indicador  = `Migração para EJA`) %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Migração para EJA")
        
        output$grafico_aba2_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_2 %>% 
            select(Ano,`Sigla da UF`, `Nome do Município`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        output$grafico_aba2_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_2%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba2_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_2 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
        })
        
        output$grafico_aba2_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_2 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
        
      }
      
      if(input$aba_2_1 == "Percentual de docentes com curso superior"){
        
        dados_aba_2 <- dados[,c(1:11,14)]
        dados_aba_2 <- dados_aba_2[dados_aba_2$serie %in% input$aba_2_2,]
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- na.omit(info)
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Nome do Município`%in% input$aba_2_4,]
        
        dados_aba_2 <- dados_aba_2 %>%
          select(Ano, `Sigla da UF`, `Nome do Município`, serie, `Percentual de docentes com curso superior`) %>% 
          rename(indicador  = `Percentual de docentes com curso superior`) %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Percentual de docentes com curso superior")
        
        output$grafico_aba2_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_2 %>% 
            select(Ano,`Sigla da UF`, `Nome do Município`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        output$grafico_aba2_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_2%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba2_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_2 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",   fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
        })
        
        output$grafico_aba2_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_2 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_2_1 == "Taxa de Abandono"){
        
        dados_aba_2 <- dados[,c(1:11,15)]
        dados_aba_2 <- dados_aba_2[dados_aba_2$serie %in% input$aba_2_2,]
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- na.omit(info)
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Nome do Município`%in% input$aba_2_4,]
        
        dados_aba_2 <- dados_aba_2 %>%
          select(Ano, `Sigla da UF`, `Nome do Município`, serie, `Taxa de Abandono`) %>% 
          rename(indicador  = `Taxa de Abandono`) %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de Abandono")
        
        output$grafico_aba2_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_2 %>% 
            select(Ano,`Sigla da UF`, `Nome do Município`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        output$grafico_aba2_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_2%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba2_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_2 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
        })
        
        output$grafico_aba2_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_2 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_2_1 == "Taxa de Aprovação"){
        
        dados_aba_2 <- dados[,c(1:11,16)]
        dados_aba_2 <- dados_aba_2[dados_aba_2$serie %in% input$aba_2_2,]
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- na.omit(info)
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Nome do Município`%in% input$aba_2_4,]
        
        dados_aba_2 <- dados_aba_2 %>%
          select(Ano, `Sigla da UF`, `Nome do Município`, serie, `Taxa de Aprovação`) %>% 
          rename(indicador  = `Taxa de Aprovação`) %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de Aprovação")
        
        output$grafico_aba2_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_2 %>% 
            select(Ano,`Sigla da UF`, `Nome do Município`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        output$grafico_aba2_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_2%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba2_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_2 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
          
        })
        
        output$grafico_aba2_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_2 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_2_1 == "Taxa de distorção idade-série"){
        
        dados_aba_2 <- dados[,c(1:11,17)]
        dados_aba_2 <- dados_aba_2[dados_aba_2$serie %in% input$aba_2_2,]
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- na.omit(info)
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Nome do Município`%in% input$aba_2_4,]
        
        dados_aba_2 <- dados_aba_2 %>%
          select(Ano, `Sigla da UF`, `Nome do Município`, serie, `Taxa de distorção idade-série`) %>% 
          rename(indicador  = `Taxa de distorção idade-série`) %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de distorção idade-série")
        
        output$grafico_aba2_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_2 %>% 
            select(Ano,`Sigla da UF`, `Nome do Município`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        output$grafico_aba2_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_2%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba2_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_2 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
        })
        
        output$grafico_aba2_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_2 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_2_1 == "Taxa de Evasão"){
        
        dados_aba_2 <- dados[,c(1:11,18)]
        dados_aba_2 <- dados_aba_2[dados_aba_2$serie %in% input$aba_2_2,]
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- na.omit(info)
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Nome do Município`%in% input$aba_2_4,]
        
        dados_aba_2 <- dados_aba_2 %>%
          select(Ano, `Sigla da UF`, `Nome do Município`, serie, `Taxa de Evasão`) %>% 
          rename(indicador  = `Taxa de Evasão`) %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de Evasão")
        
        output$grafico_aba2_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_2 %>% 
            select(Ano,`Sigla da UF`, `Nome do Município`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        output$grafico_aba2_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_2%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba2_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_2 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
        })
        
        output$grafico_aba2_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_2 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_2_1 == "Taxa de Promoção"){
        
        dados_aba_2 <- dados[,c(1:11,19)]
        dados_aba_2 <- dados_aba_2[dados_aba_2$serie %in% input$aba_2_2,]
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- na.omit(info)
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Nome do Município`%in% input$aba_2_4,]
        
        dados_aba_2 <- dados_aba_2 %>%
          select(Ano, `Sigla da UF`, `Nome do Município`, serie, `Taxa de Promoção`) %>% 
          rename(indicador  = `Taxa de Promoção`) %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de Promoção")
        
        output$grafico_aba2_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_2 %>% 
            select(Ano,`Sigla da UF`, `Nome do Município`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        output$grafico_aba2_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_2%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba2_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_2 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "", fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
        })
        
        output$grafico_aba2_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_2 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_2_1 == "Taxa de Repetência"){
        
        dados_aba_2 <- dados[,c(1:11,20)]
        dados_aba_2 <- dados_aba_2[dados_aba_2$serie %in% input$aba_2_2,]
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- na.omit(info)
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Nome do Município`%in% input$aba_2_4,]
        
        dados_aba_2 <- dados_aba_2 %>%
          select(Ano, `Sigla da UF`, `Nome do Município`, serie, `Taxa de Repetência`) %>% 
          rename(indicador  = `Taxa de Repetência`) %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de Repetência")
        
        output$grafico_aba2_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_2 %>% 
            select(Ano,`Sigla da UF`, `Nome do Município`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        output$grafico_aba2_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_2%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba2_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_2 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
        })
        
        output$grafico_aba2_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_2 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_2_1 == "Taxa de Reprovação"){
        
        dados_aba_2 <- dados[,c(1:11,21)]
        dados_aba_2 <- dados_aba_2[dados_aba_2$serie %in% input$aba_2_2,]
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- dados_aba_2[dados_aba_2$`Localização` %in% input$aba_2_3,]
        info <- na.omit(info)
        dados_aba_2 <- dados_aba_2[dados_aba_2$`Nome do Município`%in% input$aba_2_4,]
        
        dados_aba_2 <- dados_aba_2 %>%
          select(Ano, `Sigla da UF`, `Nome do Município`, serie, `Taxa de Reprovação`) %>% 
          rename(indicador  = `Taxa de Reprovação`) %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de Reprovação")
        output$grafico_aba2_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_2 %>% 
            select(Ano,`Sigla da UF`, `Nome do Município`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        output$grafico_aba2_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_2%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Nome do Município`,"(",`Sigla da UF`,"),  ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba2_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_2 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "", fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
          
        })
        
        output$grafico_aba2_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_2 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`, "-", `Nome do Município`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "", fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      
      
      output$Info1 <- renderValueBox({ #Infobox 
        
        if(round(min(as.numeric(unlist(info[,12]))),2) < 2){ # Colocando condições para plotagem de diversos Info-Box
          infoBox(
            "Menor valor",subtitle = tags$a(icon("question-circle"),"Anos 2014 e 2019"),round(min(as.numeric(unlist(info[,12]))),2),icon = icon("chart-pie"),color = "purple")}
        
        else{
          infoBox(
            "Menor valor", subtitle = tags$a(icon("question-circle"),"Anos 2014 e 2019"),round(min(as.numeric(unlist(info[,12]))),2),icon = icon("chart-pie"),color = "orange")}
      })
      
      output$Info2 <- renderValueBox({
        
        if(round(max(as.numeric(unlist(info[,12]))),2) < 5){ # Colocando condições para plotagem de diversos Info-Box
          infoBox(
            "Maior valor", subtitle = tags$a(icon("question-circle"),"Anos 2014 e 2019"),round(max(as.numeric(unlist(info[,12]))),2),icon = icon("chart-bar"),color = "red")}
        
        else{
          infoBox(
            "Maior valor",subtitle = tags$a(icon("question-circle"),"Anos 2014 e 2019"), round(max(as.numeric(unlist(info[,12]))),2),icon = icon("chart-bar"),color = "green")}
      })
      
      output$Info3 <- renderValueBox({
        
        if(round(median(as.numeric(unlist(info[,12]))),2) < 5){ # Colocando condições para plotagem de diversos Info-Box
          infoBox(
            "Mediana", subtitle = tags$a(icon("question-circle"),"Anos 2014 e 2019"),round(median(as.numeric(unlist(info[,12]))),2),icon = icon("chart-area"),color = "blue")}
        
        else{
          infoBox(
            "Mediana", subtitle = tags$a(icon("question-circle"),"Anos 2014 e 2019"),round(median(as.numeric(unlist(info[,12]))),2),icon = icon("chart-area"),color = "red")}
      })
      
      output$Info4 <- renderInfoBox({
        
        if(round(mean(as.numeric(unlist(info[,12]))),2) < 5){ # Colocando condições para plotagem de diversos Info-Box
          infoBox(
            "Média", subtitle = tags$a(icon("question-circle"),"Anos 2014 e 2019"),round(mean(as.numeric(unlist(info[,12]))),2),icon = icon("calculator"),color = "green")}
        
        else{
          infoBox(
            "Média", subtitle = tags$a(icon("question-circle"),"Anos 2014 e 2019"),round(mean(as.numeric(unlist(info[,12]))),2),icon = icon("calculator"),color = "red")}
      })
      
      output$Info5 <- renderInfoBox({
        
        if(round(mean(as.numeric(unlist(info[,12])))-1.96*sqrt(var(as.numeric(unlist(info[,12])))),2) < 0){ # Colocando condições para plotagem de diversos Info-Box
          infoBox(
            "Limite inferior", subtitle = tags$a(icon("question-circle"),"Limite truncado"),0 ,icon = icon("chart-line"),color = "orange")}
        
        else{
          infoBox(
            "Limite inferior", round(mean(as.numeric(unlist(info[,12])))-1.96*sqrt(var(as.numeric(unlist(info[,12])))),2),icon = icon("chart-line"),color = "purple")}
      })
      
      output$Info6 <- renderInfoBox({
        
        if(info$serie == '1º ao 5º Ano'){ # Colocando condições para plotagem de diversos Info-Box
          infoBox(
            "Limite superior", round(mean(as.numeric(unlist(info[,12])))+1.96*sqrt(var(as.numeric(unlist(info[,12])))),2),icon = icon("project-diagram"),color = "purple")}
        
        else{
          infoBox(
            "Limite superior", round(mean(as.numeric(unlist(info[,12])))+1.96*sqrt(var(as.numeric(unlist(info[,12])))),2),icon = icon("project-diagram"),color = "blue")}
      })
      
      
      
      
      
    })
    
    
    observeEvent(input$aba_3_5,{
      
      if(input$aba_3_1 == "IDEB"){
        
        dados_aba_3 <- dados[,c(1:11,12)]
        dados_aba_3 <- dados_aba_3[dados_aba_3$serie %in% input$aba_3_2,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Localização` %in% input$aba_3_3,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Sigla da UF` %in% (input$aba_3_4),]
        
        dados_aba_3 <- dados_aba_3 %>%
          select(Ano, `Sigla da UF`,  serie, `IDEB`) %>% 
          rename(indicador  = `IDEB`) %>% 
          group_by(Ano, `Sigla da UF`, serie) %>% 
          summarise(indicador = mean(indicador, na.rm = T)) %>% ungroup() %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("IDEB")
        
        output$grafico_aba3_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_3 %>% 
            select(Ano,`Sigla da UF`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Sigla da UF`,"-", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        
        output$grafico_aba3_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_3%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Sigla da UF`,"- ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba3_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_3 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
          
        })
        
        output$grafico_aba3_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_3 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          
          ggplotly(b, tooltip = "indicador")
        })
        
      }
      
      if(input$aba_3_1 == "Migração para EJA"){
        
        dados_aba_3 <- dados[,c(1:11,13)]
        dados_aba_3 <- dados_aba_3[dados_aba_3$serie %in% input$aba_3_2,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Localização` %in% input$aba_3_3,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Sigla da UF` %in% (input$aba_3_4),]
        
        dados_aba_3 <- dados_aba_3 %>%
          select(Ano, `Sigla da UF`,  serie, `Migração para EJA`) %>% 
          rename(indicador  = `Migração para EJA`) %>% 
          group_by(Ano, `Sigla da UF`, serie) %>%
          summarise(indicador = mean(indicador, na.rm = T)) %>% ungroup() %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Migração para EJA")
        
        output$grafico_aba3_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_3 %>% 
            select(Ano,`Sigla da UF`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Sigla da UF`,"-", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        
        output$grafico_aba3_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_3%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Sigla da UF`,"- ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba3_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_3 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
          
        })
        
        output$grafico_aba3_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b<- dados_aba_3 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_3_1 == "Percentual de docentes com curso superior"){
        
        dados_aba_3 <- dados[,c(1:11,14)]
        dados_aba_3 <- dados_aba_3[dados_aba_3$serie %in% input$aba_3_2,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Localização` %in% input$aba_3_3,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Sigla da UF` %in% (input$aba_3_4),]
        
        dados_aba_3 <- dados_aba_3 %>%
          select(Ano, `Sigla da UF`,  serie, `Percentual de docentes com curso superior`) %>% 
          rename(indicador  = `Percentual de docentes com curso superior`) %>% 
          group_by(Ano, `Sigla da UF`, serie) %>% 
          summarise(indicador = mean(indicador, na.rm = T)) %>% ungroup() %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Percentual de docentes com curso superior")
        
        
        output$grafico_aba3_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_3 %>% 
            select(Ano,`Sigla da UF`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Sigla da UF`,"-", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        
        output$grafico_aba3_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_3%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Sigla da UF`,"- ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba3_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_3 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
        })
        
        output$grafico_aba3_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_3 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_3_1 == "Taxa de Abandono"){
        
        dados_aba_3 <- dados[,c(1:11,15)]
        dados_aba_3 <- dados_aba_3[dados_aba_3$serie %in% input$aba_3_2,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Localização` %in% input$aba_3_3,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Sigla da UF` %in% (input$aba_3_4),]
        
        dados_aba_3 <- dados_aba_3 %>%
          select(Ano, `Sigla da UF`,  serie, `Taxa de Abandono`) %>% 
          rename(indicador  = `Taxa de Abandono`) %>% 
          group_by(Ano, `Sigla da UF`, serie) %>% 
          summarise(indicador = mean(indicador, na.rm = T)) %>% ungroup() %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de Abandono")
        
        output$grafico_aba3_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_3 %>% 
            select(Ano,`Sigla da UF`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Sigla da UF`,"-", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        
        output$grafico_aba3_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_3%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Sigla da UF`,"- ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba3_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_3 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
          
        })
        
        output$grafico_aba3_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_3 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_3_1 == "Taxa de Aprovação"){
        
        dados_aba_3 <- dados[,c(1:11,16)]
        dados_aba_3 <- dados_aba_3[dados_aba_3$serie %in% input$aba_3_2,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Localização` %in% input$aba_3_3,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Sigla da UF` %in% (input$aba_3_4),]
        
        dados_aba_3 <- dados_aba_3 %>%
          select(Ano, `Sigla da UF`,  serie, `Taxa de Aprovação`) %>% 
          rename(indicador  = `Taxa de Aprovação`) %>% 
          group_by(Ano, `Sigla da UF`, serie) %>% 
          summarise(indicador = mean(indicador, na.rm = T)) %>% ungroup() %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de Aprovação")
        
        output$grafico_aba3_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_3 %>% 
            select(Ano,`Sigla da UF`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Sigla da UF`,"-", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        
        output$grafico_aba3_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_3%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Sigla da UF`,"- ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba3_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_3 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
          
          
        })
        
        output$grafico_aba3_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_3 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_3_1 == "Taxa de distorção idade-série"){
        
        dados_aba_3 <- dados[,c(1:11,17)]
        dados_aba_3 <- dados_aba_3[dados_aba_3$serie %in% input$aba_3_2,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Localização` %in% input$aba_3_3,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Sigla da UF` %in% (input$aba_3_4),]
        
        dados_aba_3 <- dados_aba_3 %>%
          select(Ano, `Sigla da UF`,  serie, `Taxa de distorção idade-série`) %>% 
          rename(indicador  = `Taxa de distorção idade-série`) %>% 
          group_by(Ano, `Sigla da UF`, serie) %>% 
          summarise(indicador = mean(indicador, na.rm = T)) %>% ungroup() %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de distorção idade-série")
        
        output$grafico_aba3_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_3 %>% 
            select(Ano,`Sigla da UF`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Sigla da UF`,"-", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        
        output$grafico_aba3_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_3%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Sigla da UF`,"- ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba3_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_3 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
          
        })
        
        output$grafico_aba3_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_3 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_3_1 == "Taxa de Evasão"){
        
        dados_aba_3 <- dados[,c(1:11,18)]
        dados_aba_3 <- dados_aba_3[dados_aba_3$serie %in% input$aba_3_2,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Localização` %in% input$aba_3_3,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Sigla da UF` %in% (input$aba_3_4),]
        
        dados_aba_3 <- dados_aba_3 %>%
          select(Ano, `Sigla da UF`,  serie, `Taxa de Evasão`) %>% 
          rename(indicador  = `Taxa de Evasão`) %>% 
          group_by(Ano, `Sigla da UF`, serie) %>% 
          summarise(indicador = mean(indicador, na.rm = T)) %>% ungroup() %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de Evasão")
        
        output$grafico_aba3_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_3 %>% 
            select(Ano,`Sigla da UF`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Sigla da UF`,"-", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        
        output$grafico_aba3_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_3%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Sigla da UF`,"- ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba3_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_3 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
        })
        
        output$grafico_aba3_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_3 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_3_1 == "Taxa de Promoção"){
        
        dados_aba_3 <- dados[,c(1:11,19)]
        dados_aba_3 <- dados_aba_3[dados_aba_3$serie %in% input$aba_3_2,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Localização` %in% input$aba_3_3,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Sigla da UF` %in% (input$aba_3_4),]
        
        dados_aba_3 <- dados_aba_3 %>%
          select(Ano, `Sigla da UF`,  serie, `Taxa de Promoção`) %>% 
          rename(indicador  = `Taxa de Promoção`) %>% 
          group_by(Ano, `Sigla da UF`, serie) %>% 
          summarise(indicador = mean(indicador, na.rm = T)) %>% ungroup() %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de Promoção")
        
        output$grafico_aba3_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_3 %>% 
            select(Ano,`Sigla da UF`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Sigla da UF`,"-", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        
        output$grafico_aba3_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_3%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Sigla da UF`,"- ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba3_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_3 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
          
        })
        
        output$grafico_aba3_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_3 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
          
        })
      }
      
      if(input$aba_3_1 == "Taxa de Repetência"){
        
        dados_aba_3 <- dados[,c(1:11,20)]
        dados_aba_3 <- dados_aba_3[dados_aba_3$serie %in% input$aba_3_2,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Localização` %in% input$aba_3_3,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Sigla da UF` %in% (input$aba_3_4),]
        
        dados_aba_3 <- dados_aba_3 %>%
          select(Ano, `Sigla da UF`,  serie, `Taxa de Repetência`) %>% 
          rename(indicador  = `Taxa de Repetência`) %>% 
          group_by(Ano, `Sigla da UF`, serie) %>% 
          summarise(indicador = mean(indicador, na.rm = T)) %>% ungroup() %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de Repetência")
        
        output$grafico_aba3_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_3 %>% 
            select(Ano,`Sigla da UF`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Sigla da UF`,"-", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        
        output$grafico_aba3_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_3%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Sigla da UF`,"- ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba3_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_3 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
          
        })
        
        output$grafico_aba3_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_3 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      if(input$aba_3_1 == "Taxa de Reprovação"){
        
        dados_aba_3 <- dados[,c(1:11,21)]
        dados_aba_3 <- dados_aba_3[dados_aba_3$serie %in% input$aba_3_2,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Localização` %in% input$aba_3_3,]
        dados_aba_3 <- dados_aba_3[dados_aba_3$`Sigla da UF` %in% (input$aba_3_4),]
        
        dados_aba_3 <- dados_aba_3 %>%
          select(Ano, `Sigla da UF`,  serie, `Taxa de Reprovação`) %>% 
          rename(indicador  = `Taxa de Reprovação`) %>% 
          group_by(Ano, `Sigla da UF`, serie) %>% 
          summarise(indicador = mean(indicador, na.rm = T)) %>% ungroup() %>% 
          mutate(valor_z = round((indicador-mean(indicador, na.rm = T))/sd(indicador, na.rm = T), 2))
        
        nome_indicador <- paste("Taxa de Reprovação")
        
        output$grafico_aba3_1 <- renderPlotly({ ########################## Imagem 1 
          
          dados_grafico1 <- dados_aba_3 %>% 
            select(Ano,`Sigla da UF`, serie, indicador) %>% 
            filter(!is.na(indicador)) %>% 
            spread(Ano, indicador) %>% 
            mutate(tipo = ifelse((`2019` - `2014`)< 0, "Diminuiu", "Aumentou"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var = paste(`Sigla da UF`,"-", serie))
          graf1 <- ggplot(dados_grafico1, aes(text = paste0(var, "<br> 2014: ", `2014`, "<br> 2019: ", `2019`, '<br>', tipo))) + 
            geom_segment(aes(x=1, xend=2, y=`2014`, yend=`2019`, col=tipo), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Aumentou", "Diminuiu", "Indicador faltante"), 
                               values = c("Diminuiu"="tomato", "Aumentou"="chartreuse3", "Indicador faltante" = "gray")) +
            labs(x="", y=paste(nome_indicador), color = "Decisão: ") +
            xlim(.5, 2.5) + ylim(0,(1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`))))+
            geom_text(label="2014", x=1, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=1.2, size=8)+
            geom_text(label="2019", x=2, y=1.1*(max(dados_grafico1$`2014`, dados_grafico1$`2019`)), hjust=-0.1, size=8)+
            theme_light()+
            theme(legend.position = "bottom", axis.text.x = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf1, tooltip = "text")
        })
        
        
        output$grafico_aba3_2 <- renderPlotly({ ########################## Imagem 2
          
          graf2 <- dados_aba_3%>% 
            group_by(Ano)%>% 
            arrange(valor_z) %>% 
            mutate(tipo = ifelse(valor_z < 0, "Abaixo da Média", "Acima da Média"), 
                   tipo = ifelse(is.na(tipo), "Indicador faltante", tipo),
                   var =  paste(`Sigla da UF`,"- ", serie)) %>% 
            ggplot(aes(x = var, y = valor_z, fill = tipo, label = valor_z, 
                       text = paste0(var, "<br>", nome_indicador,": ",indicador,"<br> Valor de Z: ", round(valor_z, 2))))+
            geom_bar(stat = 'identity', width = .5)+
            scale_fill_manual(labels = c("Abaixo da Média", "Acima da Média"), values = c("Abaixo da Média" = "tomato", 
                                                                                          "Acima da Média" = "chartreuse3"))+
            coord_flip()+facet_wrap(~Ano)+
            labs(y = "Indicador normalizado", x = ' ',fill = " ")+
            theme_light()+
            theme(legend.position = "bottom", axis.text.y = element_blank(),
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(graf2, tooltip = "text")
          
        })
        
        output$grafico_aba3_3 <- renderPlotly({ ########################## Imagem 3
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          a <- dados_aba_3 %>% filter(Ano == 2014) %>%  filter(!is.na(indicador)) %>%
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(a, tooltip = "indicador")
        })
        
        output$grafico_aba3_4 <- renderPlotly({ ########################## Imagem 4
          
          cores <- c("tomato", "chocolate1", "orange", "yellow", "chartreuse3", "springgreen4")
          names(cores) <- levels(factor(dados$serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                 "3º Ano", "4º Ano", "5º Ano")))
          b <- dados_aba_3 %>% filter(Ano == 2019) %>% filter(!is.na(indicador)) %>% 
            ggplot( aes(x = paste(`Sigla da UF`),
                        y = indicador, fill = factor(serie, levels  = c("1º ao 5º Ano", "1º Ano", "2º Ano", 
                                                                        "3º Ano", "4º Ano", "5º Ano")))) + 
            geom_col(position = position_dodge(width = .75), width = .7)+
            labs(y = nome_indicador, x= "",  fill = "Série: ")+
            scale_fill_manual(values = cores)+
            theme_light()+
            theme(legend.position = "bottom", 
                  plot.subtitle = element_text(color = 'gray', size = 10, face = 'bold'))
          
          ggplotly(b, tooltip = "indicador")
        })
      }
      
      
      
    })
    
    
    observeEvent(input$aba_5_3,{
      
      if(input$aba_5_1 == "Taxa de distorção idade-série"){
        
        dados_aba_5 <- dados2[,c(1:10,12:17)]
        dados_aba_5 <- dados_aba_5[dados_aba_5$Ano == c(input$aba_5_4),]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Sigla da UF` %in% (input$aba_5_4_1),]
        dados_aba_5 <- na.omit(dados_aba_5)
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Taxa  de distorcao 1º ao 5º Ano` != "",]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Taxa  de distorcao 1º ao 5º Ano` != "--",]
        dados_aba_5 <- dados_aba_5[order(dados_aba_5$`Taxa  de distorcao 1º ao 5º Ano`),]
        dados_aba_5 <- rbind(head(dados_aba_5, n = input$aba_5_2),tail(dados_aba_5, n = input$aba_5_2))
        
      }
      
      if(input$aba_5_1 == "Taxas de rendimento (abandono, aprovação, reprovação)" & input$aba_5_1_1 == 'Abandono'){
        
        dados_aba_5 <- dados2[,c(1:10,43:60)]
        dados_aba_5 <- dados_aba_5[dados_aba_5$Ano == c(input$aba_5_4),]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Sigla da UF` %in% (input$aba_5_4_1),]
        dados_aba_5 <- na.omit(dados_aba_5)
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Abandono - Anos Iniciais (1º ao 5º Ano)` != "",]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Abandono - Anos Iniciais (1º ao 5º Ano)` != "--",]
        dados_aba_5 <- dados_aba_5[order(dados_aba_5$`Abandono - Anos Iniciais (1º ao 5º Ano)`),]
        dados_aba_5 <- rbind(head(dados_aba_5, n = input$aba_5_2),tail(dados_aba_5, n = input$aba_5_2))
        dados_aba_5 <- dados_aba_5[,-c(11:22)]
      }
      
      if(input$aba_5_1 == "Taxas de rendimento (abandono, aprovação, reprovação)" & input$aba_5_1_1 == 'Aprovação'){
        
        dados_aba_5 <- dados2[,c(1:10,43:60)]
        
        dados_aba_5 <- dados_aba_5[dados_aba_5$Ano == c(input$aba_5_4),]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Sigla da UF` %in% (input$aba_5_4_1),]
        dados_aba_5 <- na.omit(dados_aba_5)
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Aprovação - Anos Iniciais (1º ao 5º Ano)` != "",]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Aprovação - Anos Iniciais (1º ao 5º Ano)` != "--",]
        dados_aba_5 <- dados_aba_5[order(dados_aba_5$`Aprovação - Anos Iniciais (1º ao 5º Ano)`),]
        dados_aba_5 <- rbind(head(dados_aba_5, n = input$aba_5_2),tail(dados_aba_5, n = input$aba_5_2))
        dados_aba_5 <- dados_aba_5[,-c(17:28)]
      }
      
      if(input$aba_5_1 == "Taxas de rendimento (abandono, aprovação, reprovação)" & input$aba_5_1_1 == 'Reprovação'){
        
        dados_aba_5 <- dados2[,c(1:10,43:60)]
        
        dados_aba_5 <- dados_aba_5[dados_aba_5$Ano == c(input$aba_5_4),]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Sigla da UF` %in% (input$aba_5_4_1),]
        dados_aba_5 <- na.omit(dados_aba_5)
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Reprovação - Anos Iniciais (1º ao 5º Ano)` != "",]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Reprovação - Anos Iniciais (1º ao 5º Ano)` != "--",]
        dados_aba_5 <- dados_aba_5[order(dados_aba_5$`Reprovação - Anos Iniciais (1º ao 5º Ano)`),]
        dados_aba_5 <- rbind(head(dados_aba_5, n = input$aba_5_2),tail(dados_aba_5, n = input$aba_5_2))
        dados_aba_5 <- dados_aba_5[,-c(11:16,23:28)]
      }
      
      
      if(input$aba_5_1 == "Percentual de docentes com curso superior"){
        
        dados_aba_5 <- dados2[,c(1:11)]
        dados_aba_5 <- dados_aba_5[dados_aba_5$Ano == c(input$aba_5_4),]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Sigla da UF` %in% (input$aba_5_4_1),]
        dados_aba_5 <- na.omit(dados_aba_5)
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Percentual de docentes 1º ao 5º Ano` != "",]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Percentual de docentes 1º ao 5º Ano` != "--",]
        dados_aba_5 <- dados_aba_5[order(dados_aba_5$`Percentual de docentes 1º ao 5º Ano`),]
        dados_aba_5 <- rbind(head(dados_aba_5, n = input$aba_5_2),tail(dados_aba_5, n = input$aba_5_2))
        
      }
      
      if(input$aba_5_1 == "Taxas de transição" & input$aba_5_1_2 == 'Taxa de Promoção'){
        
        dados_aba_5 <- dados2[,c(1:10,19:42)]
        
        dados_aba_5 <- dados_aba_5[dados_aba_5$Ano == c(input$aba_5_4),]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Sigla da UF` %in% (input$aba_5_4_1),]
        dados_aba_5 <- na.omit(dados_aba_5)
        dados_aba_5 <- dados_aba_5[dados_aba_5$`1º ao 5º Ano - Taxa de Promoção` != "",]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`1º ao 5º Ano - Taxa de Promoção` != "--",]
        dados_aba_5 <- dados_aba_5[order(dados_aba_5$`1º ao 5º Ano - Taxa de Promoção`),]
        dados_aba_5 <- rbind(head(dados_aba_5, n = input$aba_5_2),tail(dados_aba_5, n = input$aba_5_2))
        dados_aba_5 <- dados_aba_5[,-c(12:14,16:18,20:22,24:26,28:30,32:34)]
        
      }
      
      if(input$aba_5_1 == "Taxas de transição" & input$aba_5_1_2 == 'Taxa de Repetência'){
        
        dados_aba_5 <- dados2[,c(1:10,19:42)]
        
        dados_aba_5 <- dados_aba_5[dados_aba_5$Ano == c(input$aba_5_4),]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Sigla da UF` %in% (input$aba_5_4_1),]
        dados_aba_5 <- na.omit(dados_aba_5)
        dados_aba_5 <- dados_aba_5[dados_aba_5$`1º ao 5º Ano - Taxa de Repetência` != "",]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`1º ao 5º Ano - Taxa de Repetência` != "--",]
        dados_aba_5 <- dados_aba_5[order(dados_aba_5$`1º ao 5º Ano - Taxa de Repetência`),]
        dados_aba_5 <- rbind(head(dados_aba_5, n = input$aba_5_2),tail(dados_aba_5, n = input$aba_5_2))
        dados_aba_5 <- dados_aba_5[,-c(11,13:15,17:19,21:23,25:27,29:31,33:34)]
        
      }
      
      if(input$aba_5_1 == "Taxas de transição" & input$aba_5_1_2 == 'Taxa de Evasão'){
        
        dados_aba_5 <- dados2[,c(1:10,19:42)]
        
        dados_aba_5 <- dados_aba_5[dados_aba_5$Ano == c(input$aba_5_4),]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Sigla da UF` %in% (input$aba_5_4_1),]
        dados_aba_5 <- na.omit(dados_aba_5)
        dados_aba_5 <- dados_aba_5[dados_aba_5$`1º ao 5º Ano - Taxa de Evasão` != "",]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`1º ao 5º Ano - Taxa de Evasão` != "--",]
        dados_aba_5 <- dados_aba_5[order(dados_aba_5$`1º ao 5º Ano - Taxa de Evasão`),]
        dados_aba_5 <- rbind(head(dados_aba_5, n = input$aba_5_2),tail(dados_aba_5, n = input$aba_5_2))
        dados_aba_5 <- dados_aba_5[,-c(11:12,14:16,18:20,22:24,26:28,30:32,34)]
        
      }
      
      if(input$aba_5_1 == "Taxas de transição" & input$aba_5_1_2 == 'Migração para EJA'){
        
        dados_aba_5 <- dados2[,c(1:10,19:42)]
        
        dados_aba_5 <- dados_aba_5[dados_aba_5$Ano == c(input$aba_5_4),]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Sigla da UF` %in% (input$aba_5_4_1),]
        dados_aba_5 <- na.omit(dados_aba_5)
        dados_aba_5 <- dados_aba_5[dados_aba_5$`1º ao 5º Ano - Migração para EJA` != "",]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`1º ao 5º Ano - Migração para EJA` != "--",]
        dados_aba_5 <- dados_aba_5[order(dados_aba_5$`1º ao 5º Ano - Migração para EJA`),]
        dados_aba_5 <- rbind(head(dados_aba_5, n = input$aba_5_2),tail(dados_aba_5, n = input$aba_5_2))
        dados_aba_5 <- dados_aba_5[,-c(11:13,15:17,19:21,23:25,27:29,31:33)]
        
      }
      
      if(input$aba_5_1 == "Ideb"){
        
        dados_aba_5 <- dados2[,c(1:10,18)]
        dados_aba_5 <- dados_aba_5[dados_aba_5$Ano == c(input$aba_5_4),]
        dados_aba_5 <- dados_aba_5[dados_aba_5$`Sigla da UF` %in% (input$aba_5_4_1),]
        dados_aba_5 <- na.omit(dados_aba_5)
        dados_aba_5 <- dados_aba_5[dados_aba_5$IDEB != "",]
        dados_aba_5 <- dados_aba_5[dados_aba_5$IDEB != "--",]
        dados_aba_5 <- dados_aba_5[dados_aba_5$IDEB != "-",]
        dados_aba_5 <- dados_aba_5[order(dados_aba_5$IDEB),]
        dados_aba_5 <<- rbind(head(dados_aba_5, n = input$aba_5_2),tail(dados_aba_5, n = input$aba_5_2))
        
      }
      
      ############################# Estruturando o mapa para a Aba número 5 ###########################
      
      
      output$mymap <- renderLeaflet({
        
        
        df <- dados_aba_5
        df[,11] <- as.numeric(unlist(df[,11]))
        
        getColor <- function(df) {
          sapply(df[,11], function(cor) {
            if(cor <= 4) {
              "orange"
            }  else {
              "blue"
            } })
        }
        
        icons <- awesomeIcons(
          
          markerColor = getColor(df)
        )
        
        rotulo <- paste(unlist(df$`Nome do Município`),unlist(df[,11]),sep = ": ")
        
        df$Longitude <- unlist(df$Longitude)
        df$Latitude <- unlist(df$Latitude)
        
        leaflet(df) %>% addTiles() %>%
          addAwesomeMarkers(~Longitude, ~Latitude, icon=icons, label=~rotulo)
      })
      
    })
    
    
  })
  
  
  
})


