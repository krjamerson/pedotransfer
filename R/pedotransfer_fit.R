if ("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if ("scales" %in% rownames(installed.packages()) == FALSE) {install.packages("scales")}
if ("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl")}
if ("tcltk" %in% rownames(installed.packages()) == FALSE) {install.packages("tcltk")}
if ("grid" %in% rownames(installed.packages()) == FALSE) {install.packages("grid")}
if ("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra")}
if ("cowplot" %in% rownames(installed.packages()) == FALSE) {install.packages("cowplot")}
if ("minpack.lm" %in% rownames(installed.packages()) == FALSE) {install.packages("minpack.lm")}


#Water Retention Characterization (Author: Zach Hoylman)
library(ggplot2)
require(scales)
library(readxl)   
library(tcltk)
library(grid)
library(gridExtra)
library(cowplot)
library(minpack.lm)
library(dplyr)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

lseq <- function(from=1, to=100000, length.out=6) {
  # logarithmic spaced sequence
  # blatantly stolen from library("emdbook"), because need only this
  exp(seq(log(from), log(to), length.out = length.out))
}


fit_soils = function(data){
  file_name = basename(data)
  data = read_excel_allsheets(data)
  
  #select sheet
  pf_vwc = data$`Evaluation-Retention T(pF)`%>%
    dplyr::filter(`pF [-]` >= 1)
  
  
  #Convert pF into Pa
  MPa = ((10^(pf_vwc$`pF [-]`))/10200)
  kPa = MPa * 1000
  
  #Convert theta into ratio
  VWC = pf_vwc$`Water Content [Vol%]`/100
  
  #Fit van Genuchten model
  model = nlsLM(VWC ~ r + (s - r)/((1+a*(abs(kPa)^n))^(1-(1/n))), start = list(r = 0, s = 0.483, a = 0.1966, n = 1.241))
  
  #Create Dummy Data frame to model the curve
  dummy_kPa = data.frame(kPa = lseq(min(kPa), max(kPa), 10000))
  
  #Predict theoretical curve using dummy data
  model_predict = predict(model, newdata = dummy_kPa)
  
  #Store model coeffitients for inverse function
  model_coef = coef(model)
  
  #compile export list
  export = list()
  export[[1]] = model
  export[[2]] = data.frame(raw_kPa = kPa,
                           raw_VWC = VWC)
  export[[3]] = data.frame(kPa = dummy_kPa,
                           fit_VWC = model_predict)
  export[[4]] = file_name
  return(export)
}

plot_data = function(data){
  
  ggplot_data = data.frame(kPa = data[[2]]$raw_kPa, VWC = data[[2]]$raw_VWC)
  
  model_data = data.frame(model_predict = data[[3]]$fit_VWC, dummy_kPa = data[[3]]$kPa)
  
  Van_nuuct2 = ggplot(data = ggplot_data, aes(x =VWC , y = kPa))+
    geom_point(size = 3, color = "#BFEFFF")+
    geom_point(size = 3, color = "black", fill = NA, shape = 21, alpha = 0.3)+
    theme_bw()+
    scale_y_log10(labels = comma)+
    scale_x_continuous(breaks = pretty(ggplot_data$VWC, n = 10))+
    ylab("Water Potential (kPa)")+
    xlab(expression(paste("Volumetric Water Content (", m^3," ", m^-3,")")))+
    geom_line(data = model_data, aes(x = model_predict, y = dummy_kPa))+
    theme(text = element_text(size=18),
          plot.margin=unit(c(2,2,2,2),"cm"),
          axis.line = element_line(color='black'),
          panel.grid.major = element_line(colour="grey", size=0.25))+
    geom_hline(yintercept=c(1500), linetype = "dashed", color = "red")+
    annotate(geom = "text", x = (max(ggplot_data$VWC)*.7), y = 3000, hjust = 1.05,
             label = "Theoretical Wilting Point", size = 5, color = "red")
  
  
  Van_nuuct2
  
}

plot_time_series = function(data, model){
  #import data
  time_series = read.csv(data) %>%
    dplyr::filter(m³.m³.Water.Content > 0)
  
  #convert datetime
  time_series$Timestamp = as.POSIXct(time_series$Timestamp, format = c("%m/%d/%Y %H:%M"))
  head(time_series$Timestamp)
  #store model coef
  model_coef = coef(model[[1]])
  
  time_series$m³.m³.Water.Content[time_series$m³.m³.Water.Content > 2] =NA 
  
  
  #inverse model to predict pressure from VWC
  time_series$kPa = ((((((model_coef['s'] - model_coef['r'])/(time_series$m³.m³.Water.Content - model_coef['r']))^((model_coef['n']/(model_coef['n']-1))))-1)^(1/model_coef['n']))/model_coef['a'])
  
  vwc_time_series = ggplot(data = time_series, aes(x = Timestamp, y = m³.m³.Water.Content))+ 
    geom_line()+
    theme_bw()+
    ylab(expression(paste("VWC (", m^3," ", m^-3,")")))+
    theme(text = element_text(size=18),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ylim(0.01, max(time_series$m³.m³.Water.Content))
  
  
  kPa_time_series = ggplot(data = time_series, aes(x = Timestamp, y = kPa))+
    geom_line()+
    theme_bw()+
    scale_y_log10()+
    ylab("Water Potential (kPa)")+
    xlab("Time")+
    theme(text = element_text(size=18))+
    geom_hline(yintercept=c(1500), linetype = "dashed", color = "red")+
    annotate(geom = "text", x = time_series$Timestamp[round(length(time_series$Timestamp)*.7,0)], y = 2500, hjust = 1.5, 
             label = "Theoretical Wilting Point", size = 5, color = "red")+
    expand_limits(y = 10000)
  
  
  plot_total = plot_grid(vwc_time_series, kPa_time_series, align = "v", nrow = 2, rel_heights = c(4/9, 1/2))
  return(plot_total)
}

work.dir = tk_choose.dir(caption = "Select folder with files (Hyprop = .xlsx, Timeseries = .csv)")

data = list.files(work.dir, pattern = ".xlsx$", full.names = T)
timeseries = list.files(work.dir, pattern = ".csv$", full.names = T)

data_short = list.files(work.dir, pattern = ".xlsx$")
timeseries_short = list.files(work.dir, pattern = ".csv$")

check = cbind(data_short, timeseries_short)
print(check)

#user select write dir for plot export
write.dir = tk_choose.dir(caption = "Select Write Directory")

for(i in 1:length(data)){
  #run model fit
  model = fit_soils(data[i])
  
  #plot and export 
  plot_data(model)
  #dev.print(png,paste0(write.dir,model[[4]],"_water_retension_curve.png"),res = 500, height = 6, width =9, units = 'in')
  ggsave(filename = paste0(write.dir,'/',model[[4]],"_water_retension_curve.png"), plot = last_plot(), dpi = 500,
         units = "in", width = 10, height = 6)
  
  plot_time_series(timeseries[i],model)
  ggsave(filename = paste0(write.dir,'/',model[[4]],"_timeseries.png"), plot = last_plot(), dpi = 500,
         units = "in", width = 9, height = 6)
  
  
  capture.output(summary(model[[1]]), file = paste0(write.dir,'/', model[[4]], "_model_information.txt"))
  
}
