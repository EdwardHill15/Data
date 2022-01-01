library(readr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(psych)
library(forcats)
library(DT)
library(kableExtra)
library(knitr)
library(tinytex)
library(AER)
library(knitLatex)
# NVM data ----------------------------------------------------------------

NVM_definitief <- read_csv("D:/DiagnostiekOnderzoek/NVMOnderzoek/NVM_definitief.csv")
NVM <- as.data.frame(NVM_definitief)
NVM <- NVM[,c(2:15)]
str(NVM)
NVM$Gender <- as.factor(NVM$Gender)
NVM$Status <- as.factor(NVM$Status)
NVM$Code <- as.factor(NVM$Code)
ggplot(NVM, aes(x=Cons)) +
  geom_bar()

mod1 <- glm(Cons~SOM, family = binomial, NVM)
summary(mod1)
summary(NVM$NEG)
NVM$Cons <- ifelse(NVM$NEG >= 22.9, "Incongruent", "Congruent")
table(NVM$Cons)

NVM$Cons2 <- ifelse(NVM$NEG >= 28, "Incongruent", 
                    ifelse(NVM$NEG < 18, "Congruent", "Neutral"))
table(NVM$Cons2)
library(RColorBrewer)

NVM$Cons2 <- factor(NVM$Cons2, c("Incongruent", "Neutral", "Congruent"))
ggplot(NVM, aes(x=Cons2, y = SOM, fill = as.factor(Cons2))) +
  #geom_point() +
  geom_jitter(size = 2, color = "grey") +
  geom_boxplot(alpha = 0.7, notch = T) +
  scale_fill_manual(values = c("#DDD6FE", "#8B5CF6", "#4C1D95"))

n_colors <- 10  # Specify number of colors

palette3_info <- brewer.pal.info[brewer.pal.info$category == "qual", ]  # Extract color info
palette3_all <- unlist(mapply(brewer.pal,                               # Create vector with all colors
                              palette3_info$maxcolors,
                              rownames(palette3_info)))
palette3_all                                                            # Print all colors

set.seed(2643598)                                                      # Set random seed
palette3 <- sample(palette3_all, n_colors)                              # Sample colors
palette3                                                                # Print hex color codes

pie(rep(1, n_colors),                                                   # Draw colors in pie chart
    col = palette3,
    radius = 1,
    main = "RColorBrewer Package")


NVM$Cons <- as.factor(NVM$Cons)
fivenum(NVM$NEG_z)

View(NVM)
NVM_whole <- which(NVM$Cons == "wholesome")
NVM$Cons <- NA
NVM[which(NVM$NEG_z >= 24.9 & NVM$NEG_z <= 34.9), "Cons"] = 'unwholesome'
NVM[which(NVM$NEG_z >= 14.9 & NVM$NEG_z < 24.9), "Cons"] = 'at risk'
NVM[which(NVM$NEG_z >= -1.1 & NVM$NEG_z < 14.9), "Cons"] = 'wholesome'


NVM$Cons <- as.factor(NVM$Cons)
str(NVM)

NVM_wide <- NVM[, c(2,10:15)]
NVM_wide_z <- NVM[, c(2,10:15)]

kable(NVM_wide_z) %>% 
  kable_styling(bootstrap_options = "striped")




# gathering columns to make long table
df_long <- gather(NVM_wide,NVM_factor,score,1:5)
head(df_long, 25)
df_long_z <- gather(NVM_wide_z,NVM_factor,score,2:6)
head(df_long_z, 25)
#grouping and summarizing data
# group by
df_group <- group_by(df_long,Cons)
head(df_group)
df_group_z <- group_by(df_long_z,Cons)
head(df_group_z)
#Summarize
df_group_z=group_by(df_long_z,Cons,NVM_factor) %>% summarise(mean=mean(score),
                                                             sd=sd(score))
head(df_group_z, 15)

df_group_z$mean <- round(df_group_z$mean,2)
df_group_z$sd <- round(df_group_z$sd,2)
table1 <- datatable(df_group_z, extensions = "Buttons",
          options = list(dom='Bfrtip',
                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

table1
df_group_z$mean <- round(df_group_z$mean, 2)
df_group_z$sd <- round(df_group_z$sd, 2)
table1 <- kable(df_group_z) %>% 
  kable_classic("basic", full_width=FALSE, font_size = 30, html_font = "sans")
table1


NVM <- as.data.frame(NVM[,c(2:15)])
NVM$Consciousness <- NVM$Cons
NVM <- NVM[,-14]
table2
table2 <- kable(NVM) %>% 
  kable_classic("basic", full_width = FALSE, font_size = 20, html_font = "times")
table2






#Plot basic barplot
p=ggplot(df_group_z,aes(x=Cons,y=mean))+
  geom_bar()
p

df_group_z$Cons

# Plot basic barplot- effect of stat= "identity"
p=ggplot(df_group_z,aes(x=Cons,y=mean))+
  geom_bar(stat="identity")
p

# fill color

p=ggplot(df_group_z,aes(x=Cons,y=mean))+
  geom_bar(stat="identity",fill="red")
p

# fill color - mapping to a variable

p=ggplot(df_group_z,aes(x=Cons,y=mean, fill=Cons))+
  geom_bar(stat="identity")
p

# fill colors- stacked bar plot
p=ggplot(df_group_z,aes(x=Cons,y=mean,fill=NVM_factor))+
  geom_bar(stat="identity")
p

# fill colors- grouped bar plot 
p=ggplot(df_group_z,aes(x=Cons,y=mean,fill=NVM_factor))+
  geom_bar(stat="identity",position="dodge")
p

#Error bars
p=ggplot(df_group_z,aes(x=Cons,y=mean,fill=NVM_factor))+
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd))


p

#Error bars width
p=ggplot(df_group_z,aes(x=Cons,y=mean,fill=NVM_factor))+
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25)


p

#Error bars width, size,position
p=ggplot(df_group_z,aes(x=Cons,y=mean,fill=NVM_factor))+
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9))


p
df_group_z$sd <- df_group_z$sd/3
#Error bars width,size, position, alpha
p=ggplot(df_group_z,aes(x=Cons,y=mean,fill=NVM_factor))+
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9),alpha=0.3)


p

#Error bars width,size, position, alpha and labels
p=ggplot(df_group_z,aes(x=Cons,y=mean,fill=NVM_factor,label=mean))+
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9), alpha=0.3)


p

#Error bars width,size, position, alpha and labels
p=ggplot(df_group,aes(x=Cons,y=mean,fill=NVM_factor,label=mean))+
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9), alpha=0.3)+
  geom_text(position=position_dodge(0.9))


p

#Error bars width,size, position, alpha and labels, label position corrected
p=ggplot(df_group,aes(x=Cons,y=mean,fill=NVM_factor,label=mean))+
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9), alpha=0.3)+
  geom_text(position=position_dodge(0.9),vjust=-1)


p


#Error bars width,size, position, alpha and labels, label position more corrected
p=ggplot(df_group,aes(x=Cons,y=mean,fill=NVM_factor,label=mean))+
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9), alpha=0.3)+
  geom_text(position=position_dodge(0.9),vjust=-0.5, hjust=1.2)


p

#theme classic
p=ggplot(df_group_z,aes(x=Cons,y=mean,fill=NVM_factor,label=mean))+
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9), alpha=0.3)+
  geom_text(position=position_dodge(0.9),vjust=-0.5, hjust=1)+
  theme_classic()


p


#theme bw
p=ggplot(df_group_z,aes(x=Cons,y=mean,fill=NVM_factor,label=mean))+
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9), alpha=0.3)+
  geom_text(position=position_dodge(0.9),vjust=-0.5, hjust=1.1)+
  theme_bw()


p

# I don't like this plot.
df_group$sd <- df_group$sd/2
p=ggplot(df_group_z,aes(x=Cons,y=mean,fill=NVM_factor))+
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9), alpha=0.3)+
  theme_bw()


p


 data_new <- df_group_z
 data_new$Cons <- factor(data_new$Cons, c("unwholesome", "at risk", "wholesome"))
 df_group_z <- data_new
# coord flip, this to when x axis labels are very long and overlap each other
df_group_z$sd <- df_group_z$sd/3
p <- ggplot(df_group_z,aes(x=Cons,y=mean,fill=NVM_factor))+
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.25,
                size=1,position= position_dodge(0.9), alpha=0.3)+
  theme_bw()+
  #coord_flip() +
  scale_fill_manual(values = c("#EDE9FE","#C4B5FD", "#7C3AED", "#6D28D9", "#4C1D95")) +
  #scale_fill_manual(values = c("#FAFAFA", "#E4E4E7","#D4D4D4", "#71717A", "#18181B")) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 35),
                     breaks = seq(0, 35, 5)) +
  #facet_wrap(~Gender) +
      theme(
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      panel.background = element_blank(),
      plot.title = element_text(size = 35, face = "bold", color = "blue", hjust = 0.5,
                                margin= margin(b = 15)),
      axis.line = element_line(color = "black"),
      axis.title = element_text(size = 30, color = "red",
                                face = "bold"),
      axis.text = element_text(size = 22, color = "black"),
      axis.text.y = element_text(size = 17),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10), size = 30, color = "red"),
      #legend.position = "none",
      legend.position = c(0.8, 0.8),
      legend.background = element_rect(color = "black"),
      legend.text = element_text(size = 15),
      legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
      legend.key = element_rect(color = NA, fill = NA)) +
  labs(
    x = "Consciousness",
    y = "Mean Score",
    fill = NULL,
    title = "Consciousness and NVM Factors"
  ) +
guides(
  fill = guide_legend(
    keywidth = 0.8,
    keyheight = 0.8,
    default.unit = "cm"
  )
)
  
p

Cons_SOM_plot <- Cons_NVM %>% 
  ggplot(aes(x = Consciousness,
             y = Mean_SOM,
             fill = Consciousness,
             ymin = lower_s,
             ymax = upper_s)) +
  geom_col(width = .5, position = position_dodge(.6)) +
  #color = "black", key_glyph = "polygon3") +
  geom_errorbar(width = .1, position = position_dodge(.6)) +
  #scale_fill_manual(values = c("#FBBF24", "#D97706", "#92400E")) +
  scale_fill_manual(values = c("#C4B5FD", "#7C3AED", "#4C1D95")) +
  #scale_fill_manual(values = c("#A7F3D0", "#059669", "#064E3B")) +
  #scale_fill_manual(values = c("#FBCFE8", "#EC4899", "#831843")) +
  #scale_fill_manual(values = c("#D4D4D8", "#71717A", "#27272A")) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 20),
                     breaks = seq(0, 20, 5)) +
  labs(
    x = "Consciousness",
    y = "Physical Complaints",
    fill = NULL,
    title = "Consciousness and Physical Stress"
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5,
                              margin= margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 17),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none",
    #legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 0.8,
      keyheight = 0.8,
      default.unit = "cm"
    )
  )












df1_N <- NVM %>% 
  select(c(2,10,15)) %>% 
  filter(NVM$Cons == "unwholesome") %>% 
  summarise(mean(df1_N$NEG_z)) 

df2_N <- NVM %>%
  select(c(2,10,15)) %>%
  filter(NVM$Cons == "at risk") %>% 
  summarise(mean(df2_N$NEG_z)) 

df3_N <- NVM %>% 
  select(c(2,10,15)) %>%
  filter(NVM$Cons == "wholesome") %>% 
  summarise(mean(df3_N$NEG_z))

df1_p <- NVM %>% 
  select(c(2,13,15)) %>% 
  filter(NVM$Cons == "unwholesome") %>% 
  summarise(mean(df1_p$PSY_z)) 

df2_p <- NVM %>%
  select(c(2,13,15)) %>%
  filter(NVM$Cons == "at risk") %>% 
  summarise(mean(df2_p$PSY_z)) 

df3_p <- NVM %>% 
  select(c(2,13,15)) %>%
  filter(NVM$Cons == "wholesome") %>% 
  summarise(mean(df3_p$PSY_z))

df1_e <- NVM %>% 
  select(c(2,14,15)) %>% 
  filter(NVM$Cons == "unwholesome") %>% 
  summarise(mean(df1_e$EX_z)) 

df2_e <- NVM %>%
  select(c(2,14,15)) %>%
  filter(NVM$Cons == "at risk") %>% 
  summarise(mean(df2_e$EX_z)) 

df3_e <- NVM %>% 
  select(c(2,14,15)) %>%
  filter(NVM$Cons == "wholesome") %>% 
  summarise(mean(df3_e$EX_z))

df1_v <- NVM %>% 
  select(c(2,12,15)) %>% 
  filter(NVM$Cons == "unwholesome") %>% 
  summarise(mean(df1_v$VERL_z)) 

df2_v <- NVM %>%
  select(c(2,12,15)) %>%
  filter(NVM$Cons == "at risk") %>% 
  summarise(mean(df2_v$VERL_z)) 

df3_v<- NVM %>% 
  select(c(2,12,15)) %>%
  filter(NVM$Cons == "wholesome") %>% 
  summarise(mean(df3_v$VERL_z))






describe(NVM$SOM_z)
describe(NVM$NEG_z)
describe(NVM$EX_z)

# NVM tibble definitie ----------------------------------------------------

Cons_NVM <- tibble(
  Consciousness = c("Unwholesome", "At risk", "Wholesome"), 
  Mean_SOM = c(19,14,11),
  upper_s = Mean_SOM + .72,
  lower_s = Mean_SOM - .72
  ) %>% 
  mutate(
    Consciousness = as_factor(Consciousness) %>% fct_relevel("Unwholesome",
                                               "At risk",
                                               "Wholesome")
  )


# NVM Bar plots Consciousness ---------------------------------------------

Cons_SOM_plot <- Cons_NVM %>% 
  ggplot(aes(x = Consciousness,
             y = Mean_SOM,
             fill = Consciousness,
             ymin = lower_s,
             ymax = upper_s)) +
  geom_col(width = .5, position = position_dodge(.6)) +
           #color = "black", key_glyph = "polygon3") +
  geom_errorbar(width = .1, position = position_dodge(.6)) +
  #scale_fill_manual(values = c("#FBBF24", "#D97706", "#92400E")) +
  scale_fill_manual(values = c("#C4B5FD", "#7C3AED", "#4C1D95")) +
  #scale_fill_manual(values = c("#A7F3D0", "#059669", "#064E3B")) +
  #scale_fill_manual(values = c("#FBCFE8", "#EC4899", "#831843")) +
  #scale_fill_manual(values = c("#D4D4D8", "#71717A", "#27272A")) +
    scale_y_continuous(expand = expansion(0),
                     limits = c(0, 20),
                     breaks = seq(0, 20, 5)) +
  labs(
    x = "Consciousness",
    y = "Physical Complaints",
    fill = NULL,
    title = "Consciousness and Physical Stress"
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5,
                              margin= margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 17),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none",
    #legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 0.8,
      keyheight = 0.8,
      default.unit = "cm"
    )
  )

####### Consciousness vs Negativisme

Cons_NVM_n <- tibble(
  Consciousness = c("Unwholesome", "At risk", "Wholesome"), 
  Mean_NEG = c(20,22,13),
  upper_n = Mean_NEG + .58,
  lower_n = Mean_NEG - .58
) %>% 
  mutate(
    Consciousness = as_factor(Consciousness) %>% fct_relevel("Unwholesome",
                                                             "At risk",
                                                             "Wholesome")
  )

############ Consciousness - Negativisme plot definitie

Cons_NEG_plot <- Cons_NVM_n %>% 
  ggplot(aes(x = Consciousness,
             y = Mean_NEG,
             fill = Consciousness,
             ymin = lower_n,
             ymax = upper_n)) +
  geom_col(width = .5, position = position_dodge(.6)) +
  #color = "black", key_glyph = "polygon3") +
  geom_errorbar(width = .1, position = position_dodge(.6)) +
  #scale_fill_manual(values = c("#FBBF24", "#D97706", "#92400E")) +
  scale_fill_manual(values = c("#C4B5FD", "#7C3AED", "#4C1D95")) +
  #scale_fill_manual(values = c("#A7F3D0", "#059669", "#064E3B")) +
  #scale_fill_manual(values = c("#FBCFE8", "#EC4899", "#831843")) +
  #scale_fill_manual(values = c("#D4D4D8", "#71717A", "#27272A")) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 25),
                     breaks = seq(0, 25, 5)) +
  labs(
    x = "Consciousness",
    y = "Negative Emotions",
    fill = NULL,
    title = "Consciousness and Negative Emotions"
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5,
                              margin= margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 17),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none",
    #legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 0.8,
      keyheight = 0.8,
      default.unit = "cm"
    )
  )

########## Consciousness versus Psy definitie


 describe(NVM$PSY_z)
Cons_NVM_p <- tibble(
  Consciousness = c("Unwholesome", "At risk", "Wholesome"), 
  Mean_PSY = c(6,3,2),
  upper_p = Mean_PSY + .33,
  lower_p = Mean_PSY - .33
) %>% 
  mutate(
    Consciousness = as_factor(Consciousness) %>% fct_relevel("Unwholesome",
                                                             "At risk",
                                                             "Wholesome")
  )

Cons_PSY_plot <- Cons_NVM_p %>% 
  ggplot(aes(x = Consciousness,
             y = Mean_PSY,
             fill = Consciousness,
             ymin = lower_p,
             ymax = upper_p)) +
  geom_col(width = .5, position = position_dodge(.6)) +
  #color = "black", key_glyph = "polygon3") +
  geom_errorbar(width = .1, position = position_dodge(.6)) +
  #scale_fill_manual(values = c("#FBBF24", "#D97706", "#92400E")) +
  scale_fill_manual(values = c("#C4B5FD", "#7C3AED", "#4C1D95")) +
  #scale_fill_manual(values = c("#A7F3D0", "#059669", "#064E3B")) +
  #scale_fill_manual(values = c("#FBCFE8", "#EC4899", "#831843")) +
  #scale_fill_manual(values = c("#D4D4D8", "#71717A", "#27272A")) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 8),
                     breaks = seq(0, 8, 2)) +
  labs(
    x = "Consciousness",
    y = "Psychopathology",
    fill = NULL,
    title = "Consciousness and Psychopathology"
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5,
                              margin= margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 17),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none",
    #legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 0.8,
      keyheight = 0.8,
      default.unit = "cm"
    )
  )

################# Consciousness and Extraversion
describe(NVM$EX_z)
Cons_NVM_e <- tibble(
  Consciousness = c("Unwholesome", "At risk", "Wholesome"), 
  Mean_EX = c(12,11,12),
  upper_e = Mean_EX + .4,
  lower_e = Mean_EX - .4
) %>% 
  mutate(
    Consciousness = as_factor(Consciousness) %>% fct_relevel("Unwholesome",
                                                             "At risk",
                                                             "Wholesome")
  )


Cons_EX_plot <- Cons_NVM_e %>% 
  ggplot(aes(x = Consciousness,
             y = Mean_EX,
             fill = Consciousness,
             ymin = lower_e,
             ymax = upper_e)) +
  geom_col(width = .5, position = position_dodge(.6)) +
  #color = "black", key_glyph = "polygon3") +
  geom_errorbar(width = .1, position = position_dodge(.6)) +
  #scale_fill_manual(values = c("#FBBF24", "#D97706", "#92400E")) +
  scale_fill_manual(values = c("#C4B5FD", "#7C3AED", "#4C1D95")) +
  #scale_fill_manual(values = c("#A7F3D0", "#059669", "#064E3B")) +
  #scale_fill_manual(values = c("#FBCFE8", "#EC4899", "#831843")) +
  #scale_fill_manual(values = c("#D4D4D8", "#71717A", "#27272A")) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 14),
                     breaks = seq(0, 14, 2)) +
  labs(
    x = "Consciousness",
    y = "Extraversion",
    fill = NULL,
    title = "Consciousness and Extraversion"
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5,
                              margin= margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 17),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none",
    #legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 0.8,
      keyheight = 0.8,
      default.unit = "cm"
    )
  )

################# Consciousness versus Verlegenheid

describe(NVM$VERL_z)
Cons_NVM_e <- tibble(
  Consciousness = c("Unwholesome", "At risk", "Wholesome"), 
  Mean_VERL = c(16,12,10),
  upper_v = Mean_VERL + .71,
  lower_v = Mean_VERL - .71
) %>% 
  mutate(
    Consciousness = as_factor(Consciousness) %>% fct_relevel("Unwholesome",
                                                             "At risk",
                                                             "Wholesome")
  )

Cons_VERL_plot <- Cons_NVM_e %>% 
  ggplot(aes(x = Consciousness,
             y = Mean_VERL,
             fill = Consciousness,
             ymin = lower_v,
             ymax = upper_v)) +
  geom_col(width = .5, position = position_dodge(.6)) +
  #color = "black", key_glyph = "polygon3") +
  geom_errorbar(width = .1, position = position_dodge(.6)) +
  #scale_fill_manual(values = c("#FBBF24", "#D97706", "#92400E")) +
  scale_fill_manual(values = c("#C4B5FD", "#7C3AED", "#4C1D95")) +
  #scale_fill_manual(values = c("#A7F3D0", "#059669", "#064E3B")) +
  #scale_fill_manual(values = c("#FBCFE8", "#EC4899", "#831843")) +
  #scale_fill_manual(values = c("#D4D4D8", "#71717A", "#27272A")) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 18),
                     breaks = seq(0, 18, 2)) +
  labs(
    x = "Consciousness",
    y = "Introversion",
    fill = NULL,
    title = "Consciousness and Introversion"
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5,
                              margin= margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 17),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none", 
    #legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 0.8,
      keyheight = 0.8,
      default.unit = "cm"
    ) 
  )





# Save NVM plots ----------------------------------------------------------
ggsave("barchart_cons_SOM.png", width = 10, height = 7, dpi = 300)
ggsave("barchart_cons_NEG.png", width = 10, height = 7, dpi = 300)
ggsave("barchart_cons_psy.png", width = 10, height = 7, dpi = 300)
ggsave("barchart_cons_verl.png", width = 10, height = 7, dpi = 300)
ggsave("barchart_cons_ex.png", width = 10, height = 7, dpi = 300)


getwd()
setwd("D:/DiagnostiekOnderzoek/NVMOnderzoek/Plots")
ggsave("Cons_EX_plot.png", width = 10, height = 7, dpi = 300)
Cons_PSY_plot
ggsave("Cons_PSY_plot.png", width = 10, height = 7, dpi = 300)
Cons_NEG_plot
ggsave("Cons_NEG_plot.png", width = 10, height = 7, dpi = 300)
Cons_SOM_plot
ggsave("Cons_SOM_plot.png", width = 10, height = 7, dpi = 300)
Cons_VERL_plot
ggsave("Cons_VERL_plot.png", width = 10, height = 7, dpi = 300)
ggsave("Violin_SOM_plot.png", width = 10, height = 7, dpi = 300)
ggsave("table1.png", width = 10, height = 7, dpi = 300)



library(cowplot)
plot_grid(Cons_EX_plot, Cons_PSY_plot, Cons_NEG_plot, Cons_SOM_plot, 
          nrow=2, ncol=2)

getwd()
setwd("D:/DiagnostiekOnderzoek/NVMOnderzoek/Plots")
ggsave("Cons_multiple-plot2.png", width = 15, height = 10, dpi = 300)
ggsave("Cons_SOM_facet_Gender.png", width = 15, height = 10, dpi = 300)
ggsave("Consciousness_group.png", width = 15, height = 10, dpi = 300)
ggsave("Consciousness_group2.png", width = 15, height = 10, dpi = 300)
ggsave("Consciousness_group3.png", width = 15, height = 10, dpi = 300)
ggsave("Consciousness_group4.png", width = 15, height = 10, dpi = 300)

p <- NVM %>%
  ggplot(aes(x=Cons, y=SOM_z, fill=Cons, color=Cons)) +
  geom_jitter(color = "grey", size = 2.0) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width=0.3, color="black", alpha=0.7) +
  facet_wrap(~Gender) + 
  #scale_fill_viridis(discrete = TRUE) + 
  scale_fill_manual(values = c("#FBBF24", "#D97706", "#92400E", "#B45309")) +
  #scale_fill_manual(values = c("#C4B5FD", "#7C3AED", "#4C1D95",)) +
  #scale_fill_manual(values = c("#A7F3D0", "#059669", "#064E3B")) +
  #scale_fill_manual(values = c("#FBCFE8", "#EC4899", "#831843")) +
  #scale_fill_manual(values = c("#FAFAFA", "#E4E4E7", "#D4D4D4", "#737373")) +
  #theme_ipsum() +
  theme(
    legend.position="none") +
    ggtitle("Consciousness versus SOM") +
  #geom_col(width = .5, position = position_dodge(.6)) +
  #color = "black", key_glyph = "polygon3") +
  #geom_errorbar(width = .1, position = position_dodge(.6)) +
  #scale_fill_manual(values = c("#FBBF24", "#D97706", "#92400E")) +
  #scale_fill_manual(values = c("#C4B5FD", "#7C3AED", "#4C1D95")) +
  #scale_fill_manual(values = c("#A7F3D0", "#059669", "#064E3B")) +
  #scale_fill_manual(values = c("#FBCFE8", "#EC4899", "#831843")) +
  #scale_fill_manual(values = c("#D4D4D8", "#71717A", "#27272A")) +
  p + scale_y_continuous(expand = expansion(0),
                     limits = c(0, 40),
                     breaks = seq(0, 40, 10)) +
  labs(
    x = "Consciousness",
    y = "Physical Complaints",
    fill = NULL,
    title = "Consciousness and Physical Stress"
  ) 
p <- p +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5,
                              margin= margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 17, color = "black"),
    axis.text.y = element_text(size = 17),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.position = "none",
    #legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 0.8,
      keyheight = 0.8,
      default.unit = "cm"
    )
  )
  
 p <- p +
  xlab("Consciousness") + 
  ylab("Somatisering") +
  theme(
    #plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5,
    margin= margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 17, color = "black"),
    axis.text.y = element_text(size = 17),
    axis.text.x.bottom = element_text(size = 17),
    axis.ticks.x = element_blank(),
    axis.title.x = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 22, face = "bold", vjust = 0.5),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none",
    #legend.position = c(0.8, 0.8),
    #legend.background = element_rect(color = "black"),
    #legend.text = element_text(size = 15),
    #legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    #legend.key = element_rect(color = NA, fill = NA)
    )
p
