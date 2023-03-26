require(tidyverse)
require(lubridate)
require(RcppRoll)
require(cowplot)

x = read_csv("https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/cases/covid-case-counts.csv") %>% 
  rename(Report_Date = `Report Date`)

nday = 140

z = x %>% 
  group_by(Report_Date) %>% 
  summarise(cases = sum(`Number of cases reported`)) %>% 
  mutate(ratio = cases/lag(cases, n = 7), 
         averatio = roll_mean(ratio, n = 7, align = "right", fill = NA),
         avecases = roll_mean(cases, n = 7, align = "right", fill = NA)) %>% 
  filter(Report_Date > today() - nday) %>% 
  print(n = 130)


doublings <- data.frame(time = c(4, 2,  10/7, 1, 5/7, 4/7, 3/7),
                        per = c("4 weeks", "2 weeks", "10 days", "1 week", "5 days", "4 days", "3 days"))
doublings$logtime <- 2^(1/doublings$time)
gooddoublings <-  doublings$time > max(z$averatio)
goodhalvings <-  doublings$time < min(z$averatio)
doublings <- doublings[1:3,]

labelxoffset <- 4

breaks <- c(0.5*10^(-10:10) ,10^(-10:10)) 
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

negdouble = doublings[1:2,]
p1 = qplot(data = z, x = Report_Date, ratio)+ 
  scale_y_log10(name = "Ratio of cases on day to cases a week earlier (log scale)") +
  geom_line(aes(y = averatio))+
  geom_hline(yintercept = doublings$logtime, colour = "darkred", alpha = .3)+
  geom_hline(yintercept = 1/negdouble$logtime, colour = "blue4", alpha = .3) +
  geom_text(data = doublings, aes(y = logtime, label = per), x = min(z$Report_Date), vjust = 0, nudge_y = 0.01)+
  geom_text(data = negdouble, aes(y = 1/logtime, label = per), x = min(z$Report_Date), vjust = 1,  nudge_y = -0.01)+
  annotate("text", label = "Cases halving in...", x = min(z$Report_Date)+labelxoffset, y= .95)+
  annotate("text", label = "Cases doubling in...", x = min(z$Report_Date)+labelxoffset, y= 1.75)+
  labs(title = paste("Covid cases week on week trend - Ao/NZ"),  x = "Report date")


  
 p2 =  qplot(data = z, x = Report_Date, cases) + 
  scale_y_log10(name = "Cases (log scale)", breaks = breaks, minor_breaks  = minor_breaks) +
  geom_line(aes(y = avecases))+
  labs(title = paste("Covid daily cases with 7-day rolling average - Ao/NZ to",today()), x = "Report date")

plot_grid(p2, p1)

ggsave(paste(today(),"aocases.jpg",sep = ""), width = 400, height = 200, units = "mm")
