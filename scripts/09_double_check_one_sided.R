# double checking one sided t-test
check <- xdata |> 
  filter(decision_error == TRUE, 
         test_type %in% c("t","F")) |> 
  mutate(real_p = ifelse(test_type == "t", 
                            2 * (1 - pt(q=test_value, df=df2)),
                            pf(test_value, df1, df2)),
         real_p_prop = real_p / computed_p)

## all of them are along the [1,1] axis meaning they are not halfing the p-value for one-sided tests
ggplot(check, aes(x = real_p,
                  y = computed_p,
                  color = one_tailed_in_txt)) + 
  geom_point() +
  facet_grid(one_tailed_in_txt ~ test_type)
