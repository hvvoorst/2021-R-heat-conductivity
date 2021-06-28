'

DESCRIPTION


'


#----------------------------------- RETRIEVE DATA -----------------------------------
library(dplyr)
df <- readRDS(file="df.rds")

#----------------------------------- MAKE REGRESSION -----------------------------------
dfr <- df%>%
  filter(!grepl("18",kwaliteit),
         !grepl("K[0-9][05]",kwaliteit),
         !grepl("SCC",kwaliteit))

init <- list(a=0.07,b=1.4)
model<-nls(meting815~a*(exp(ds815*b)),start=init,data=dfr,nls.control(
  maxiter = 50, tol = 1e-07, printEval = F, warnOnly = T))

#dfr2 <- (cbind(dfr,pred=predict(model)))

#----------------------------------- PLOT -----------------------------------
library(ggplot2)

p <- df %>%
  mutate(prestatie = round(meting815 / (0.07*exp(ds815*1.4)),2),
    tag = case_when(
    prestatie >= 1.3 ~ kwal,
    prestatie <= 0.75 ~ kwal,
    TRUE ~ "")
  ) %>%
  ggplot(aes(x=ds815,y=meting815,color=al2o3)) +
  geom_point(size=3) +
  geom_label_repel(aes(label=tag),label.size=NA,nudge_y=-.3)


#titles, regression and comments
subt_1 <- paste0("y = ",bquote(.(as.character(round(coefs[1],2)))))
subt_2 <- paste0(bquote(.(as.character(round(coefs[2],2)))),"*ds815")

p +
  scale_color_gradientn(colors=c("red","#23b81d","blue"),na.value = "grey50") +
  geom_function(fun = ~ coefs[1]*exp(.x*coefs[2])) + #"y = coefs[1]e",
  labs (
    title ="Heat Conductivity vs Apperent Density", 
    subtitle = bquote(.(subt_1)^.(subt_2)), 
    x = "Ds 815°C (g/ml)",
    y = expression(paste(lambda," (W/m/K)")),
    color=expression("Al"[2]*"O"[3])
  ) +
  guides(shape = FALSE) +
  annotate("text", label = "Korund, SiC and ALAG differ from regression line", x = 1.3, y = 6.5, color="red")


