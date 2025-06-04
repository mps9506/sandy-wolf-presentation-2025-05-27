library(dataRetrieval)
library(dplyr)
library(EnvStats)
library(ggplot2)
library(mpsTemplates)
library(runner)
library(ggrepel)

## download data
site_numbers <- c("TCEQMAIN-10484", "TCEQMAIN-15344")
bac_data <- dataRetrieval::readWQPdata(siteid = site_numbers,
                                       startDateLo = "2000-01-01",
                                       startDateHi = "2025-01-01")

bac_data |>
  distinct(CharacteristicName) |> 
  arrange(CharacteristicName)

bac_data |> 
  filter(CharacteristicName == "Escherichia coli") |>
  filter(ResultMeasure.MeasureUnitCode != "hours") |> 
  ggplot() +
  geom_point(aes(ActivityStartDate, ResultMeasureValue)) +
  scale_y_log10() +
  geom_hline(yintercept = 126) +
  facet_wrap(~MonitoringLocationIdentifier)



df <- bac_data |> 
  filter(CharacteristicName == "Escherichia coli") |>
  filter(ResultMeasure.MeasureUnitCode != "hours") |> 
  mutate(station = case_when(
    MonitoringLocationIdentifier == "TCEQMAIN-10484" ~ "Sandy Creek",
    MonitoringLocationIdentifier == "TCEQMAIN-15344" ~ "Wolf Creek"
  )) |> 
  group_by(station) |> 
  run_by(idx = "ActivityStartDate", k = "7 years") |> 
  mutate(rga = runner(x = ResultMeasureValue,
                      f = function(x) {
                        EnvStats::geoMean(x)
                        }))


df |> 
  filter(ActivityStartDate == max(ActivityStartDate, na.rm = TRUE))

##10484 is Sandy Creek
## 15344 is Wolf
ggplot(df) +
  geom_point(aes(ActivityStartDate, ResultMeasureValue), alpha = 0.5) +
  geom_step(aes(ActivityStartDate, rga, 
                color = "7-year Average",
                linetype = "7-year Average")) +
  geom_label_repel(data = df |> 
                    filter(ActivityStartDate == max(ActivityStartDate, na.rm = TRUE)),
                  aes(ActivityStartDate, rga, label = round(rga,0))) +
  scale_y_log10("E. coli Concentration") +
  scale_x_date("Date") +
  geom_hline(aes(yintercept = 126, 
                 color = "Primary Recreation 1 Criteria (126 MPN/100mL)",
                 linetype ="Primary Recreation 1 Criteria (126 MPN/100mL)")) +
  facet_wrap(~station) +
  guides(colour = guide_legend(""), 
         linetype = guide_legend("")) +
  theme_mps_noto()
