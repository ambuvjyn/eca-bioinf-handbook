

library(tidyverse)


times <- read_csv("figure-creation/haplotype-caller-run-times/hc_times.csv")

times2 <- times %>%
  extract(
    col = file,
    into = c("number_of_samples", "rep"),
    regex = "/([0-9]+)_([0-9]+)\\.bmk",
    convert = TRUE
  ) %>%
  arrange(number_of_samples, rep)


centrals <- times2 %>%
  group_by(number_of_samples) %>%
  summarise(
    mean_cpu = mean(cpu_time),
    median_cpu = median(cpu_time)
  )

# we want a linear line from size 48's mean
rise = centrals$mean_cpu[centrals$number_of_samples == "48"]
run = 48

g <- ggplot() +
  geom_point(data = times2, aes(x = number_of_samples, y = cpu_time), colour = "blue", alpha = 0.5) +
  #geom_point(data = centrals, aes(x = number_of_samples, y = mean_cpu)) +
  geom_line(data = centrals, aes(x = number_of_samples, y = mean_cpu), colour = "blue") +
  geom_abline(slope = rise/run, intercept = 0, linetype = "dashed") +
  xlab("Number of samples") +
  ylab("CPU time (in seconds)")


ggsave(g, filename = "figs/haplotype-caller-run-times.svg", width = 6, height = 3)

  
