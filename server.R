#WEIGHT

require(shiny);require(plotly);require(tidyr);require(lazyeval)

function(input, output) {
  ##TAG1
ntext<-eventReactive(input$goButton,{
  if (input$activity=="Sedentary"){
    AF<-1.3
  }
  if (input$activity=="Moderate"){
    AF<-1.5
  }
  if (input$activity=="Strenous"){
    AF<-2
  }

  if(input$gender=="Female"){
    (655+9.6*input$weight+1.8*input$height-4.7*input$age)*AF
  }
  else {
    (66+13.7*input$weight+5.0*input$height-6.8*input$age)*AF
  }
})

output$TEE<-renderText({
  ntext()
  })
  
  ##TAG3
data <- eventReactive(input$simulate,{
  x<-c(1:60)
  plan1<-input$starting-(((input$totale-input$eintake)*x)/7700)
  plan2<-input$starting-(((input$totale1-input$eintake1)*x)/7700)
  data<-as.data.frame(cbind(x,plan1,plan2))
  data_long<-gather(data, type, y, plan1:plan2, factor_key=TRUE)
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  data_long %>% accumulate_by(~x)
})



output$plot1<-renderPlotly({
      data()%>%
      plot_ly(x=~x,y=~y,
      split = ~type,
      frame = ~frame,
      type = 'scatter',
      mode = 'lines+markers', 
      line = list(simplyfy = F))%>%
      animation_opts(frame = 0.01,
                    transition = 0,
                    redraw = FALSE)%>%
      layout(xaxis = list(title = 'Days'),
            yaxis = list (title = 'Weight (kg)'))
})

output$IBW<-eventReactive(input$report, {
  if(input$gender1=="Female"){
    round(21.5*((input$height1/100)^2),0)
  }
  else{
    round(23*((input$height1/100)^2),0)
  }
})

output$pIBW<-eventReactive(input$report, {
  if(input$gender1=="Female"){
    round(input$weight1*100/(21.5*((input$height1/100)^2)),0)
  }
  else{
    round(input$weight1*100/(23*((input$height1/100)^2)),0)
  }
})

output$bsa<-eventReactive(input$report, {
round((input$weight1^0.425)*(input$height1^0.725)*0.007184, 1)
})

output$BMI<-eventReactive(input$report, {
  round(input$weight1/((input$height1*0.01)^2), 1)
})

CLASS<-eventReactive(input$report,{
  temp<-round(input$weight1/((input$height1*0.01)^2), 1)
  if(temp<18.5){
      "Underweight"
    }
  else if(temp>=18.5&temp<25){
    "Normal"
  }
  else if(temp>=25&temp<30){
    "Overweight"
  }
  else if(temp>=30){
    "Obesity"
  }
})

output$BMIc<-renderText({CLASS()})

output$ratio<-eventReactive(input$report,{
round(input$waist1/input$hip1,1)
})
output$check<-eventReactive(input$report,{
  temp1<-round(input$waist1/input$hip1,1)
  if(temp1>0 & temp1<=0.88){
    return(paste("<span style=\"color:green\">Normal</span>"))
  }
  else{
    return(paste("<span style=\"color:red\">Too high</span>"))
  }
})

output$hrt<-eventReactive(input$report,{
  (220-input$age1)*0.7
})

output$hrm<-eventReactive(input$report,{
  (220-input$age1)
})
}


