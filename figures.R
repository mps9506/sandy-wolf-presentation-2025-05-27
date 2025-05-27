library(dataRetrieval)
library(dplyr)
library(EnvStats)
library(ggplot2)
library(mpsTemplates)
library(runner)


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



bac_data |> 
  filter(CharacteristicName == "Escherichia coli") |>
  filter(ResultMeasure.MeasureUnitCode != "hours") |> 
  group_by(MonitoringLocationIdentifier) |> 
  run_by(idx = "ActivityStartDate", k = "7 years") |> 
  mutate(rga = runner(x = ResultMeasureValue,
                      f = function(x) {
                        EnvStats::geoMean(x)
                        })) |>
  ggplot() +
  geom_point(aes(ActivityStartDate, ResultMeasureValue)) +
  geom_step(aes(ActivityStartDate, rga)) +
  scale_y_log10() +
  geom_hline(yintercept = 126) +
  facet_wrap(~MonitoringLocationIdentifier)
