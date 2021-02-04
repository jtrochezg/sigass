
server <- function(input, output,session) {
  
  options(shiny.maxRequestSize=30*1024^2) 

  
  
  
  library(googlesheets4)
  library(googleAuthR)
  library(googledrive)
  library(RGoogleAnalytics)
  
  
  
  
   observe({
     
    # Direccion del archivo    
    inFile<-input$file
    
    if(is.null(inFile)) 
      return(NULL)
    
    # Lectura del archivo con direccion inFile$datapath y almacenamiento en data1
    

    gs4_auth_configure(api_key = "AIzaSyCl7hmA-zD6KEAw3gbPU1eZF7THobMGBUA")
    
    
    dat=read_sheet("https://docs.google.com/spreadsheets/d/e/2PACX-1vRFFQcSSbez1XK8gkN5OWZpxNgPInjnX3nQbDG9j29tgUEJQuoEsBJTdHwL5p6nQsLbOS8r5S7e_TNI/pubhtml?gid=659441384&single=true")
   
    dat <- gs_url(dat, lookup = FALSE, visibility = "public")
          })
  
  output$table <- renderDataTable({
 dat 
  
  })
  
   sliderValues2 <- reactive({
    base=data.frame(dat$`Carné o cédula`)
    colnames(base) <- c("documento")
    d=duplicated(dat$`Carné o cédula`)
    tobes <- which(d==FALSE)
    DATA <- base[tobes,]
      data.frame(
      Name = c("Total de asesorias académicas con repetición",
               "Total de asesorias académicas sin repetición"
                     ),
      Value = as.character(c(length(dat$`Carné o cédula`),
                             length(DATA$documento)
                             )),
      stringsAsFactors = FALSE)
    
  })
  output$summary4 <- renderTable({
    sliderValues2()
  })
  
   output$pred_plot21<- renderPlot({
    
    yy=data.frame(table(as.numeric(dat$`Carné o cédula`)))
    
    d <-(yy$Freq)
    
    
    haz.maz=function(x){
      ifelse(x>input$upper,"mayor que",x)}
    
    ttt=data.frame(table(sapply(d,haz.maz)))
    
    colnames(ttt)=c("Veces","Estudiantes")
    
    
    g <- ggplot(ttt, aes(x=Veces,y=Estudiantes))
    
    g + geom_col(fill=("#008081"))+scale_x_discrete("Veces")+
      geom_text(aes(label = ttt$Estudiantes), vjust = -0.2)+
    theme(axis.text.x = element_text(angle =0, hjust = 1), 
          axis.text.y = element_text(hjust = 1,size=rel(1.2)),
          axis.title.x = element_text(size = rel(1.5)),
          axis.title.y = element_text(size = rel(1.5)),
          axis.text = element_text(size = 12),legend.position = "left",
          strip.text.x = element_text(size=16),
          panel.background = element_rect(fill = "grey95", colour = "grey50"),
          strip.background = element_rect(colour = "black", fill = "grey99"))
          })
  
  
  
 
  output$pred_plot5<- renderPlot({
    
   
    ff=as.Date(dat$Timestamp,"%m/%d/%y")
    
    dias=c('Lunes','Martes','Miercoles','Jueves','Viernes','Sabado')
    y=wday(ff, week_start=1) 
    
    marco=data.frame(dias,table(y)[1:6])
    
    colnames(marco)=c("Dias","Numero","Asesorias")
    
    g <- ggplot(marco, aes(x=Numero, y=Asesorias))
    # Number of cars in each class:
    g + geom_col(fill=("#008080"))+labs(x="Dias de la semana")+
      coord_cartesian(ylim = c(min(marco$Asesorias)-100,max(marco$Asesorias)+100))+
      geom_text(aes(label = marco$Asesorias), vjust = -0.2)+
      theme(axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(hjust = 1,size=rel(1.2)),
            axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text = element_text(size = 12),legend.position = "left",
            strip.text.x = element_text(size=16),
            panel.background = element_rect(fill = "grey95", colour = "grey50"),
            strip.background = element_rect(colour = "black", fill = "grey99"))+
      scale_x_discrete(labels=c("1" = "Lunes", "2" = "Martes","3"="Miercoles",
                                "4"="Jueves","5" = "Viernes","6"="Sabado"))
    
    
  })
  
 output$pred_plot4<- renderPlot({
    
    ff=as.Date(dat$Timestamp,"%m/%d/%y")
    
    inter=seq(as.Date.character(min(ff)), as.Date.character(max(ff))+7, "weeks")

    clasi <- ifelse(ff < inter[1],'0',
              ifelse(ff < inter[2],'1',     
               ifelse(ff < inter[3],'2',
                ifelse(ff < inter[4],'3',
                 ifelse(ff < inter[5],'4',
                  ifelse(ff < inter[6],'5',
                   ifelse(ff < inter[7],'6',
                    ifelse(ff < inter[8],'7',
                     ifelse(ff < inter[9],'8',
                      ifelse(ff < inter[10],'9',
                       ifelse(ff < inter[11],'10',
                        ifelse(ff < inter[12],'11',
                         ifelse(ff < inter[13],'12',
                          ifelse(ff < inter[14],'13',
                           ifelse(ff < inter[15],'14',
                            ifelse(ff < inter[16],'15',
                             ifelse(ff < inter[17],'16',
                              ifelse(ff < inter[18],'17',
                               ifelse(ff < inter[19],'18',
                                ifelse(ff < inter[20],'19',
                                 ifelse(ff < inter[21],'20',
                                  ifelse(ff < inter[22],'21',
                                   ifelse(ff < inter[23],'22',
                                    ifelse(ff < inter[24],'23',
                                     ifelse(ff < inter[25],'24',
                                      ifelse(ff < inter[26],'25','26'
                                        ))))))))))))))))))))))))))
    
    clasi=as.numeric(clasi)
    ttt=data.frame(table(clasi))
    
    bb=round(seq(min(ttt$Freq),max(ttt$Freq),by = ((max(ttt$Freq)-min(ttt$Freq))/7)))
 colnames(ttt)=c("semana","Frecuencia")
    
    # geom_bar is designed to make it easy to create bar charts that show
    # counts (or sums of weights)
    g <- ggplot(ttt, aes(x=semana, y=Frecuencia))
    # Number of cars in each class:
    g + geom_col(fill=("#008080"))+  
      geom_text(aes(label = ttt$Frecuencia), vjust = -0.2)+
      scale_y_continuous("Asesorías",breaks = c(bb))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1), 
      axis.text.y = element_text(hjust = 1,size=rel(1.2)),
      axis.title.x = element_text(size = rel(1.5)),
       axis.title.y = element_text(size = rel(1.5)),
      axis.text = element_text(size = 12),legend.position = "left",
      strip.text.x = element_text(size=16),
      panel.background = element_rect(fill = "grey95", colour = "grey50"),
      strip.background = element_rect(colour = "black", fill = "grey99"))
    
  })
  
  
  
 output$tabless9 <- renderDataTable({
   
   
   ff=as.Date(dat$Timestamp,"%m/%d/%y")
   
   inter=seq(as.Date.character(min(ff)), as.Date.character(max(ff))+7, "weeks")
   
   dia_inicial=seq(as.Date.character(min(ff)), as.Date.character(max(ff)), "weeks")
   dia_final=dia_inicial+6
   semana=seq(1,length(dia_inicial))
   
   clasi <- ifelse(ff < inter[1],'0',
                   ifelse(ff < inter[2],'1',     
                          ifelse(ff < inter[3],'2',
                                 ifelse(ff < inter[4],'3',
                                        ifelse(ff < inter[5],'4',
                                               ifelse(ff < inter[6],'5',
                                                      ifelse(ff < inter[7],'6',
                                                             ifelse(ff < inter[8],'7',
                                                                    ifelse(ff < inter[9],'8',
                                                                           ifelse(ff < inter[10],'9',
                                                                                  ifelse(ff < inter[11],'10',
                                                                                         ifelse(ff < inter[12],'11',
                                                                                                ifelse(ff < inter[13],'12',
                                                                                                       ifelse(ff < inter[14],'13',
                                                                                                              ifelse(ff < inter[15],'14',
                                                                                                                     ifelse(ff < inter[16],'15',
                                                                                                                            ifelse(ff < inter[17],'16',
                                                                                                                                   ifelse(ff < inter[18],'17',
                                                                                                                                          ifelse(ff < inter[19],'18',
                                                                                                                                                 ifelse(ff < inter[20],'19',
                                                                                                                                                        ifelse(ff < inter[21],'20',
                                                                                                                                                               ifelse(ff < inter[22],'21',
                                                                                                                                                                      ifelse(ff < inter[23],'22',
                                                                                                                                                                             ifelse(ff < inter[24],'23',
                                                                                                                                                                                    ifelse(ff < inter[25],'24',
                                                                                                                                                                                           ifelse(ff < inter[26],'25','26'
                                                                                                                                                                                           ))))))))))))))))))))))))))
   
   
   
   clasi=as.numeric(clasi)
   table(clasi)
   
   ttt=data.frame(dia_inicial,dia_final,table(clasi))
   ttt
   
 })
 
  

  
    output$tables1 <- renderDataTable({
      
      
      
      base=data.frame(dat$Asignatura,dat$`Carné o cédula`)
      colnames(base) <- c("Asignatura","documento")
      
      ##____________________
      
      yy=data.frame(table(base$Asignatura))
      
      prop= round(as.numeric(yy$Freq)/(sum(yy$Freq))*100,1)
      
      tt= cbind(yy,prop)
      colnames(tt)=c("Asignatura","Conteo","Proporción")
      tt
    })
    
    output$tables3<- renderDataTable({
      
      ff=as.Date(dat$Timestamp,"%m/%d/%y")
      
      dias=c('lunes','martes','miercoles','jueves','viernes','sabado')
      y=wday(ff, week_start=1) 
      
      marco=data.frame(dias,table(y)[1:6])
      
      colnames(marco)=c("Dias","Numero","Asesorias")
      marco
      
    })
    
    output$tables2<- renderDataTable({
      
      
      base=data.frame(dat$Asignatura,as.numeric(dat$`Carné o cédula`))
      colnames(base) <- c("asignatura","documento")
      
      f=data.frame(ftable(base$documento,base$asignatura))
      
      haz.cero.na=function(x){
        ifelse(x==0,NA,x)}
      
      dataf.2=data.frame(sapply(f[3],haz.cero.na))
      tt=data.frame(f,dataf.2)
      ttt=na.omit(tt)
      ttt=ttt[-4]
      
      y=data.frame(table (ttt[1]))
      hh=data.frame(table(y$Freq))
      colnames(hh) <- c("Asignatura","Estudiantes")
 
      hh
      
    })
    
    output$tables4<- renderDataTable({
    
      ff=as.Date(dat$Timestamp,"%m/%d/%y")
      
      y=wday(ff, week_start=1) 
      
      w=data.frame(y,ff)
      
      dia = as.factor(y)
      
      t2=ftable(dia,ff,row.vars = 1:2)
      t2=data.frame(t2)
      
      
      haz.cero.na=function(x){
        ifelse(x==0,NA,x)}
      
      dataf.2=data.frame(sapply(t2[3],haz.cero.na))
      tt=data.frame(t2,dataf.2)
      ttt=na.omit(tt)
      ttt=ttt[-4]
      ttt
    })
    
    
 
    
    
    
    ########modifffffffffff____________________
    
    output$preee <- renderPlot({
   
      yy=data.frame(table(dat$Asignatura))
      
     
     colnames(yy)=c("Asignatura","Asesorías")
      
  g <- ggplot(yy, aes(x=(reorder(Asignatura,-Asesorías)), 
       y=Asesorías),fill=Asesorías)
      
      g + geom_col(fill=("#008081"))+
        scale_x_discrete("Asignaturas")+
        geom_text(aes(label = yy$Asesorías), vjust = -0.2)+
        theme(axis.text.x = element_text(angle = 30, hjust = 1), 
              axis.text.y = element_text(hjust = 1,size=rel(1.2)),
              axis.title.x = element_text(size = rel(1.5)),
              axis.title.y = element_text(size = rel(1.5)),
              axis.text = element_text(size = 12),legend.position = "left",
              strip.text.x = element_text(size=16),
              panel.background = element_rect(fill = "grey95", colour = "grey50"),
              strip.background = element_rect(colour = "black", fill = "grey99"))
      
      
      
        })
    
    
    
    
  
    output$preee1 <- renderPlotly({
      
      dataset <- datasetInput()
      
      base=data.frame(datasetInput()[,input$vv],datasetInput()[,input$doc])
      colnames(base) <- c("beca","documento")
      
      d=duplicated(base$documento)
      
      tobes <- which(d==FALSE)
      DATA <- base[tobes,]
      
      ##____________________
      
      yy=data.frame(table(DATA$beca))
      
      prop= round(as.numeric(yy$Freq)/(sum(yy$Freq))*100,1)
      
      tt= cbind(yy,prop)
      colnames(tt)=c("Beca","Conteo","Proporción")
      
      colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
      
      
      p <- plot_ly(tt, labels = ~ tt$Beca, values = ~ tt$Proporción,
                   type = 'pie', textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',marker = list(colors = colors,
                                                    line = list(color = '#FFFFFF', width = 1)),
                   #The 'pull' attribute can also be used to create space between the sectors
                   showlegend = FALSE) %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p
      
    })
    
    
  
  output$tabless20 <- renderPrint({
      
    
    ff=as.Date(dat$Timestamp, "%d/%m/%y")
    
    inter=seq(as.Date.character(min(ff)), as.Date.character(max(ff))+7, "weeks")
    
        clasi <- ifelse(ff < inter[2], '1',
                    ifelse(ff < inter[3],'2',
                           ifelse(ff < inter[4],'3',
                                  ifelse(ff < inter[5],'4',
                                         ifelse(ff < inter[6],'5',
                                                ifelse(ff < inter[7],'6',
                                                       ifelse(ff < inter[8],'7',
                                                              ifelse(ff < inter[9],'8',
                                                                     ifelse(ff < inter[10],'9',
                                                                            ifelse(ff < inter[11],'10',
                                                                                   ifelse(ff < inter[12],'11',
                                                                                          ifelse(ff < inter[13],'12',
                                                                                                 ifelse(ff < inter[14],'13',
                                                                                                        ifelse(ff < inter[15],'14',
                                                                                                               ifelse(ff < inter[16],'15',
                                                                                                                      ifelse(ff < inter[17],'16',
                                                                                                                             ifelse(ff < inter[18],'17',
                                                                                                                                    ifelse(ff < inter[19],'18',
                                                                                                                                           ifelse(ff < inter[20],'19','20')))))))))))))))))))
    
    
    clasi=as.numeric(clasi)
    
base=data.frame(dat$`Nombre del docente que lo atendió`,as.numeric(dat$`Carné o cédula`),clasi)
    
    
    colnames(base) <- c("ASESOR","documento","semana")
    
    tab <- addmargins(table(base$ASESOR,base$semana), 2)
    tab2=tab[as.logical(rowSums(tab != 0)), ]
    tab3=tab2[, colSums(tab2 != 0) > 0]
    tab3
    
    })
    
  output$table1 <- renderPrint({
    
    
    ff=as.Date(dat$Timestamp, "%d/%m/%y")
    
    inter=seq(as.Date.character(min(ff)), as.Date.character(max(ff))+7, "weeks")
    
    clasi <- ifelse(ff < inter[2], '1',
                    ifelse(ff < inter[3],'2',
                           ifelse(ff < inter[4],'3',
                                  ifelse(ff < inter[5],'4',
                                         ifelse(ff < inter[6],'5',
                                                ifelse(ff < inter[7],'6',
                                                       ifelse(ff < inter[8],'7',
                                                              ifelse(ff < inter[9],'8',
                                                                     ifelse(ff < inter[10],'9',
                                                                            ifelse(ff < inter[11],'10',
                                                                                   ifelse(ff < inter[12],'11',
                                                                                          ifelse(ff < inter[13],'12',
                                                                                                 ifelse(ff < inter[14],'13',
                                                                                                        ifelse(ff < inter[15],'14',
                                                                                                               ifelse(ff < inter[16],'15',
                                                                                                                      ifelse(ff < inter[17],'16',
                                                                                                                             ifelse(ff < inter[18],'17',
                                                                                                                                    ifelse(ff < inter[19],'18',
                                                                                                                                           ifelse(ff < inter[20],'19','20')))))))))))))))))))
    
    
    clasi=as.numeric(clasi)
    
    base=data.frame(dat$Asignatura,as.numeric(dat$`Carné o cédula`),clasi)
    
    
    colnames(base) <- c("Asignatura","documento","semana")
    
    tab <- addmargins(table(base$Asignatura,base$semana), 2)
    tab2=tab[as.logical(rowSums(tab != 0)), ]
    tab3=tab2[, colSums(tab2 != 0) > 0]
    tab3
    
  })
   

  output$tabless5 <- renderDataTable({
    
    yy=data.frame(table(as.numeric(dat$`Carné o cédula`)))
    
    d <-(yy$Freq)
  
    haz.maz=function(x){
      ifelse(x>input$upper,"mayor que",x)}
    
    ttt=data.frame(table(sapply(d,haz.maz)))
    
    colnames(ttt)=c("Veces","Estudiantes")
    
    ttt
    
    
  })
  
  

  
  
  
  
  
  }