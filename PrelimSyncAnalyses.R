require(ggplot2)
require(dplyr)
require(tidyr)
require(ggpubr)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/MurphyCreek")

RC_q<-read.delim("reynolds-creek-043-hourly-streamflow.dat", skip = 18, sep = ",")
RC_q$datetime<-as.POSIXct(RC_q$datetime, format = "%Y-%m-%d %H:%M")
RC_q$qcms[RC_q$qcms==-999]<-NA


RC_q %>%
  filter(datetime < "2019-10-01" & datetime > "2019-07-01") %>%
  ggplot(aes(datetime, qcms))+geom_line()

binary_df<-read.csv("PA_Murphy.csv")
binary_df$datetime<-as.POSIXct(binary_df$datetime, format = "%m/%d/%y %H:%M")
names(binary_df)[-1] <- sub("^M", "", names(binary_df)[-1])
binary_df<-binary_df %>%
  filter(!is.na(datetime))

binary_flow<-left_join(binary_df, RC_q) %>%
  filter(!is.na(qcms))

sensor_names <- names(binary_flow)[grepl("^[0-9]+$", names(binary_flow))]  # after removing 'M'

network_state <- binary_flow %>%
  mutate(n_on = rowSums(select(., all_of(sensor_names)) == 1, na.rm = TRUE),
         frac_on = n_on / length(sensor_names))

p1<-ggplot(network_state, aes(frac_on, qcms)) +
  geom_boxplot(alpha = 0.4, aes(group=frac_on)) +
  geom_jitter(alpha=0.1)+
  geom_smooth(method = "loess",se=F, color = "blue") +
  theme_classic(base_size = 14) +
  labs(x = "Fraction of sensors flowing",
       y = "Outlet discharge")

p1

binary_df_long <- binary_df %>%
  pivot_longer(cols = -datetime, names_to = "distance", values_to = "flow_tag") %>%
  mutate(distance=as.numeric(distance), 
         flow_tag=case_when(
           is.na(flow_tag)~1,
           .default = flow_tag)) %>%
  arrange(datetime, distance)

last_flow <- binary_df_long %>%
  filter(flow_tag==0) %>%
  group_by(datetime) %>%
  summarise(flow_dist=min(distance))

last_flow_df<-left_join(network_state, last_flow) %>%
  mutate(flow_dist=case_when(
    is.na(flow_dist)~1993,
    .default = flow_dist
  ))

p2<-ggplot(last_flow_df, aes(flow_dist, qcms))+
  geom_boxplot(aes(group=flow_dist), alpha = 0.4)+
  geom_jitter(alpha=0.1)+
  theme_classic(base_size = 14) +
  geom_smooth(method = "loess", se=F, col="red")+
  labs(x = "Continuous Flowing Distance",
       y = "Outlet discharge")

p3<-ggplot()+
  geom_smooth(last_flow_df, mapping = aes(flow_dist, qcms), method = "loess", se=F, col="red")+
  geom_smooth(network_state, mapping=aes(frac_on*2000, qcms), method = "loess", se=F, col="blue")+
  scale_x_continuous(
    name = "Continuous Flowing Distance",
    sec.axis = sec_axis(~ . /2000, name = "Fraction of Sensors Flowing")
  ) +
  theme_classic(base_size = 14)+labs(y="Outlet discharge")
  
p3

pdf("Outlet_Response_panelPlot.pdf", width = 6, height = 10)

ggarrange(p1, p2, p3, nrow = 3, align = "v")

dev.off()

p1<-ggplot(last_flow_df, aes(flow_dist, frac_on, col=datetime))+
  geom_smooth(method = "loess", col="black", se=F)+
  geom_jitter(width = 20, alpha=0.4)+
  theme_classic(base_size = 14) +
  labs(x = "Continuous Flowing Distance",
       y = "Fraction flowing", col="Date")+
  scale_color_datetime(low = "dodgerblue", high = "salmon")

p2<-ggplot(last_flow_df, aes(flow_dist, frac_on, col=qcms))+
  geom_smooth(method = "loess", col="black", se=F)+
  geom_jitter(width = 20, alpha=0.4)+
  theme_classic(base_size = 14) +
  labs(x = "Continuous Flowing Distance",
       y = "Fraction flowing", col="Discharge")+
  scale_color_gradientn(colors = c("skyblue", "dodgerblue", "dodgerblue4"))

ggarrange(p1, p2)

cor_df <- tibble(
  sensor = sensor_names,
  cor_with_Q = sapply(sensor_names, function(s) {
    cor(binary_flow[[s]], binary_flow$qcms, use = "pairwise.complete.obs")
  })
)

cor_df <- cor_df %>%
  mutate(category = case_when(
    abs(cor_with_Q) >= 0.5 ~ "Strong",
    abs(cor_with_Q) >= 0.3 ~ "Moderate",
    TRUE ~ "Weak"
  ))

ggplot(cor_df, aes(x = as.numeric(sensor), y = cor_with_Q, fill = category)) +
  geom_col() +
  scale_fill_manual(values = c("Weak" = "grey70", "Moderate" = "skyblue", "Strong" = "navy")) +
  theme_classic(base_size = 14) +
  labs(x = "Distance from Outlet", y = "Correlation with Outlet Q", fill = "Strength",
       title = "Sensor–Outlet Correlation Categories")


attributes<-read.csv("murphy_topo_attributes.csv")

attributes$Sensor.ID <- sub("^M", "", attributes$Sensor.ID)

colnames(attributes)<-c("sensor", "Flow_perm", "Slope", "UCA", "Curvature", "TWI", "SubStorage")

attributes<-attributes %>%
  mutate(sensor=case_when(
    sensor=="91"~"93",
    .default = sensor
  ))


cor_df_attributes<-left_join(cor_df, attributes)

ggplot(cor_df_attributes, aes(Flow_perm, cor_with_Q))+
  theme_bw()+geom_smooth(method = "lm", se=F, col="black", lwd=0.5)+
  geom_point(size=3, aes(col=as.numeric(sensor)))+
  labs(x="Flow Permenance", y="Correlation with Outlet Q", col="Stream Meter")+
  theme(text = element_text(size = 20))+
  scale_color_gradient(low = "dodgerblue3", high="grey")


#### now identify wet/dry events ####

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

df_events_last_drawdown<-df_events %>%
  filter(event_type=="drydown" & datetime < as.POSIXct("2019-09-06")) %>%
  group_by(sensor) %>%
  slice_max(datetime)

df_events_last_rewet<-df_events %>%
  filter(event_type=="rewet" & datetime > as.POSIXct("2019-09-04")) %>%
  group_by(sensor) %>%
  slice_max(datetime) %>%
  filter(!sensor=="1166")

df_events_rewet_sept6 <-df_events %>%
  filter(datetime > as.POSIXct("2019-09-06") & datetime < as.POSIXct("2019-09-08") & 
           event_type=="rewet") %>%
  filter(sensor=="1166")

final_events<-bind_rows(df_events_last_drawdown, df_events_last_rewet, df_events_rewet_sept6)

final_events_paired<-final_events %>%
  group_by(sensor) %>%
  mutate(num_events=n()) %>%
  ungroup() %>%
  filter(num_events==2) %>%
  mutate(sensor=as.numeric(sensor))

df_events_last<-df_events %>%
  group_by(sensor, event_type) %>%
  slice_max(datetime) %>%
  mutate(sensor=as.numeric(sensor))

df_events_neighbor<-binary_flow %>%
  pivot_longer(
    cols = all_of(sensor_names),
    names_to = "sensor",
    values_to = "flow_state"
  ) %>%
  group_by(sensor) %>%
  arrange(datetime) %>%
  mutate(
    change = flow_state - lag(flow_state),
    event_type = case_when(
      change == 1  ~ "rewet",
      change == -1 ~ "drydown",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(event_type)) %>%             # keep only real events
  mutate(
    time_next = lead(datetime) - datetime,
    has_neighbor_2days =
      (!is.na(time_next) & time_next <= days(2))
  ) %>%
  filter(has_neighbor_2days)             # keep only events near another event


df_events_neighbor <- df_events_neighbor %>%
  group_by(sensor, event_type) %>%
  mutate(
    n_events = n(),
  ) %>%
  filter(n_events >= 5)  

df_events_neighbor %>%
  filter(event_type=="rewet") %>%
  ggplot(aes(hour(datetime)))+geom_histogram()

df_events_neighbor %>%
  filter(event_type=="drydown") %>%
  ggplot(aes(hour(datetime)))+geom_histogram()


df_events <- final_events_paired %>%
  rowwise() %>%
  mutate(
    Q_before = mean(binary_flow$qcms[
      binary_flow$datetime >= datetime - hours(24) & binary_flow$datetime < datetime
    ], na.rm = TRUE),
    Q_after = mean(binary_flow$qcms[
      binary_flow$datetime <= datetime + hours(24) & binary_flow$datetime > datetime
    ], na.rm = TRUE),
    delta_Q = Q_after - Q_before,
    delta_Q_norm = delta_Q/Q_before
  ) %>%
  ungroup()

# event_summary <- df_events %>%
#   group_by(sensor, event_type) %>%
#   summarise(
#     mean_delta_Q = median(delta_Q_norm[is.finite(delta_Q_norm)], na.rm = TRUE),
#   )

p5<-ggplot(event_summary, aes(x = as.numeric(sensor), y = mean_delta_Q, fill = event_type)) +
  geom_col(position = "dodge") +
  theme_classic(base_size = 14) +
  labs(
    x = "Sensor ID",
    y = expression("Percent Change in"~Delta~Q[outlet]),
    fill = "Transition Type",
    title = "Outlet Response to Local Flow Transitions (window = 24 hours)"
  )+ylim(-1, 3)

pdf("OutletDischargeResponse.pdf", width = 15, height = 10)

ggarrange(p0, p1, p2, p3, p4, p5, nrow=3, ncol = 2)

dev.off()

attributes<-read.csv("murphy_topo_attributes.csv")

attributes$Sensor.ID <- sub("^M", "", attributes$Sensor.ID)

colnames(attributes)<-c("sensor", "Flow_perm", "Slope", "UCA", "Curvature", "TWI", "SubStorage")

attributes<-attributes %>%
  mutate(sensor=case_when(
    sensor=="91"~"93",
    .default = sensor
  ))

event_attributes <- left_join(event_summary, attributes)

ggplot(event_attributes, aes(Flow_perm, abs(mean_delta_Q)))+geom_point(aes(col=event_type))+
  geom_smooth(aes(col=event_type), se=F)+theme_bw()+
  theme(text = element_text(size = 20))+labs(x="Flow Permanence", y="Outlet Q Response",
                                             col="Event Type")


