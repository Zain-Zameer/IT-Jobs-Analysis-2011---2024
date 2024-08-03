library("readr")
library("dplyr")
library("ggplot2")

df <- read_csv("jobs_01.csv")

job_df <- df %>%
  group_by(Year,`Job Type`) %>%
  summarise(Total=sum(`Estimated Number`))

year_dataAnalyst <- job_df %>%
  filter(`Job Type`=="Data Analyst")

ggplot(data=year_dataAnalyst,aes(x=Year,y=Total)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks=seq(2011,2024,by=1)) +
  theme_minimal() +
  labs(title="Data Analyst Analysis",subtitle="Analysis from 2011 - 2024",x="Years",y="Estimated Numbers")



growth_rate_data <- df %>%
  group_by(Region,`Job Type`) %>%
  mutate(Growth_Rate=(`Estimated Number`-lag(`Estimated Number`))/lag(`Estimated Number`))

regions_wise_DES <- growth_rate_data %>%
  filter(Region=="Europe",`Job Type`=="Data Entry Specialist")

ggplot(data=regions_wise_DES,aes(x=Year,y=Growth_Rate))+ 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(2011,2024,by=1)) +
  theme_minimal() +
  scale_y_continuous(labels=scales::percent)+
  labs(title="Data Entry Specialist Analysis",subtitle="Analysis from 2011-2024 in Europe",x="Years",y="Growth Rate Percentage")


regional <- growth_rate_data %>%
  filter(Year==max(Year)) %>%
  group_by(Region) %>%
  summarise(Total=sum(`Estimated Number`))

ggplot(data=regional,aes(x=reorder(Region,-Total),y=Total)) +
  geom_bar(stat="identity",aes(fill=Region)) +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x=element_blank()) + 
  theme(axis.text.y=element_blank()) +
  labs(title="Job Analysis 2024",subtitle="Analysis on total number of estimated jobs in each region",x="Regions",y="Total Estimated Jobs")


role_popular <- df %>%
  group_by(`Job Type`) %>%
  summarise(Total=sum(`Estimated Number`))

ggplot(data=role_popular,aes(x=reorder(`Job Type`,-Total),y=Total)) +
  geom_bar(stat="identity",aes(fill=`Job Type`)) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) + 
  theme(axis.text.y=element_blank()) + 
  coord_flip() +
  labs(title="IT Jobs Analysis",subtitle="Analysis on total number of estimated jobs for each role.",x="Job Roles",y="Total Estimated Jobs")
  

yearly_comparison <- growth_rate_data %>%
  group_by(Year) %>%
  summarise(Total=sum(`Estimated Number`))

ggplot(data=yearly_comparison,aes(x=reorder(Year,Total),y=Total)) +
  geom_bar(stat="identity",fill="lightgreen") +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x=element_blank()) +
  labs(title="IT Jobs Analysis",subtitle="Yearly comparison of Jobs 2011-2024",x="Years",y="Total IT Jobs")







