# Moving Average Example

library(tidyverse)
library(timetk)

1:10

1:10 %>% slidify_vec(.f = min, .period = 3, .align = "center", .partial = TRUE)
