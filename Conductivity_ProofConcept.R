#### conductivity proof of concept figure

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/MurphyCreek")

SC<-read.csv("SC_Murphy.csv")
colnames(SC)[1]<-"datetime"
SC$datetime<-as.POSIXct(SC$datetime, format = "%m/%d/%y %H:%M")
SC_long<-SC %>%
  pivot_longer(cols=c(2:5), values_to = "cond", names_to = "sensor")

SC_long %>%
  #filter(datetime < "2019-08-19" & datetime > "2019-08-05") %>%
  ggplot(aes(datetime, cond))+geom_line()+facet_wrap(~sensor, nrow = 4)

SC_233<-SC_long %>%
  filter(sensor=="SPC_M233")

RC_q<-read.delim("reynolds-creek-043-hourly-streamflow.dat", skip = 18, sep = ",")
RC_q$datetime<-as.POSIXct(RC_q$datetime, format = "%Y-%m-%d %H:%M")
RC_q$qcms[RC_q$qcms==-999]<-NA

binary_df<-read.csv("PA_Murphy.csv")
binary_df$datetime<-as.POSIXct(binary_df$datetime, format = "%m/%d/%y %H:%M")
names(binary_df)[-1] <- sub("^M", "", names(binary_df)[-1])
binary_df<-binary_df %>%
  filter(!is.na(datetime))

binary_flow<-left_join(binary_df, RC_q) %>%
  filter(!is.na(qcms))

sensor_names <- names(binary_flow)[grepl("^[0-9]+$", names(binary_flow))]  # after removing 'M'

df_events <- binary_flow %>%
  pivot_longer(cols = all_of(sensor_names),
               names_to = "sensor", values_to = "flow_state") %>%
  group_by(sensor) %>%
  arrange(datetime) %>%
  mutate(change = flow_state - lag(flow_state),
         event_type = case_when(
           change == 1  ~ "rewet",
           change == -1 ~ "drydown",
           TRUE ~ NA_character_
         )) %>%
  filter(!is.na(event_type))

events_233<-df_events %>%
  filter(sensor==233)

SC_233<-SC_233 %>%
  mutate(event_tag=case_when(
    datetime %in% events_233$datetime~"event",
    .default = "no event"))

SC_233_min_max <- SC_233 %>%
  filter(cond > 0) %>%
  group_by(day = date(datetime)) %>%
  mutate(
    daily_event_tag = any(event_tag == "event", na.rm = TRUE),
    min_SC = min(cond),
    max_SC = max(cond),
    amp    = max_SC - min_SC
  ) %>%
  ungroup() %>%
  filter(!duplicated(date(datetime)))


shade_data <- data.frame(
  start_date = as.POSIXct("2019-08-05 01:00:01"),
  end_date = as.POSIXct("2019-08-19 01:00:01")
)

shade_data2 <- data.frame(
  start_date = as.POSIXct("2019-09-07 00:00:01"),
  end_date = as.POSIXct("2019-09-07 23:59:00")
)

p1<-ggplot()+
  geom_rect(data = shade_data, mapping=aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = "skyblue", alpha = 0.4)+
  geom_rect(data = shade_data2, mapping=aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = "skyblue", alpha = 0.4)+
  geom_line(SC_233, mapping=aes(datetime, cond_no_zero))+theme_classic(base_size = 15)+
  labs(x="Date", y="Specific Conductivity (uS/cm)")

p2<-ggplot(SC_233_min_max, aes(daily_event_tag, amp, fill=daily_event_tag))+geom_boxplot()+
  theme_classic(base_size = 15)+
  theme(legend.position = "null")+
  labs(x="", y="Max Daily SC - Min Daily SC")+scale_x_discrete(labels=c("no rewetting", "rewetting"))+
  scale_fill_manual(values = c("black", "skyblue"))

ggarrange(p1, p2, nrow = 1, widths = c(0.6, 0.4))

SC_233 %>%
  filter(datetime < "2019-08-19" & datetime > "2019-08-05") %>%
  ggplot(aes(datetime, cond_no_zero))+geom_line()+theme_classic(base_size = 15)+
  labs(x="Date", y="Specific Conductivity (uS/cm)")
