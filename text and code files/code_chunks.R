#code chunks

#EDA
#summary data from get_nCov2019
covid <- get_nCov2019(lang = "en")
updateTime <- covid[["lastUpdateTime"]]
lubridate::as_datetime(updateTime)
confirmedCases <- format(covid$chinaTotal$confirm, big.mark = ",")

#get the total number of cases world-wide
areaTree <- covid[["areaTree"]]
totalcases <- sum(areaTree$total$confirm)


#mapping
install.packages("maps")
plot(covid, main = "")


#load historical data
hist_ls <- load_nCov2019(lang = "en")
hist_df <- hist_ls$data


#trend analyses - CHINA
#by province

by_province <- hist_df %>% 
  group_by(province) %>% 
  summarise(cases = max(cum_confirm),
            healed = max(cum_heal),
            dead = max(cum_dead)) %>% 
  arrange(desc(cases)))

p <- by_province %>% 
  mutate(province = fct_reorder(province, cases)) %>% 
  ggplot(aes(x = province, y = cases))

p +
  geom_col(fill = "#9DA368") +
  geom_text(aes(label= cases), 
            position=position_dodge(width=.20), hjust= -.4, size= 2.5) +
  coord_flip() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank()) +
  labs(x = "Province", y = "Number of Confirmed Cases")



#hubei province has the highest numbers!

hubei <- hist_df %>% 
  filter(province == "Hubei") %>% 
  group_by(time) %>% 
  summarise(cases = sum(cum_confirm),
            heal = sum(cum_heal),
            dead = sum(cum_dead))


t <- ggplot(hubei, aes(x = as.Date(time, "%m.%d")))

t +
  geom_col(aes(y = cases), fill = "#58718A") +
  geom_point(aes(y = heal), color = "#DDBCBA") +
  geom_line(aes(y = heal), color = "#DDBCBA") +
  #scale_y_continuous(sec.axis = sec_axis(~. /5)) +
  geom_text(aes(label = heal, y = heal), 
            position=position_dodge(width=.9), size = 3, vjust = -2.5,
            color = "#DDBCBA") +
  theme_minimal() +
  labs(x = "", y = "Number of Confirmed Cases")

provinces <- unique(hist_df$province)

#running parameterized rmarkdown files
purrr::map(provinces, function(x){
  rmarkdown::render("./solution/covid_param.rmd", 
                    params = list(province = x),
                    output_file = paste0(x, '.html'))
})

