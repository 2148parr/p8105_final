preliminary\_data\_cleaning
================

## Importing NHATS data and selecting only relevant variables

Using only round 5 data, included ID variable, the two variables needed
for the depression risk scale (PHQ-2), gender, age, race, whether
participant uses mobility devices, size of social network, whether or
not they have no one to talk to, self reported memory issues causing
problems, if dementia is reason for a proxy (note this variable is only
relevant for people who have a proxy, so we can’t omit missing/NA for
this variable), self-rated health, and whether they are happy with their
living situation.

``` r
nhats = read_sas("./final_data/nhats_r5.sas7bdat")  %>%
  select(spid, hc5depresan1, hc5depresan2, r5dgender, r5d2intvrage, rl5dracehisp, md5canewlker, 
         sn5dnumsn, fl5noonetalk, cg5ofmemprob, is5reasnprx1, hc5health, wb5truestme4) %>%
  janitor::clean_names() %>%   
  filter(hc5depresan1 > 0, hc5depresan2 > 0)

nhats %>% 
  select(hc5depresan1, hc5depresan2)  %>% 
  rowSums(na.rm = TRUE) -> nhats$phq.total
```

Preliminary exploratory visualizations (need proper labels): \# for
gender - 1 male, 2 female \# for age categories - 1 65-69, 2 70-74, 5
75-79, 4 80 - 84, 5 85-89, 6 90+ \# for racehisp - 1 white non-hispanic,
2 black non-hispanic, 3 other non-hispanic, 4, hispanic, 5 more than
one/dkrf primary

``` r
gender_bar = 
  nhats  %>%   
  count(r5dgender) %>% 
  mutate(r5dgender = as.factor(r5dgender),
         r5dgender = fct_reorder(r5dgender, n)) %>% 
  plot_ly(x = ~r5dgender, y = ~n, color = ~r5dgender, type = "bar", colors = "viridis") %>% 
  layout(plot_bgcolor = "f8f8f8",
         xaxis = list(title = "participant gender", tickfont = list(size = 11)), 
         yaxis = list(title = "frequency"), 
         title = "Gender",  
         titlefont = list(size = 16),
         showlegend = FALSE)

age_bar = 
  nhats  %>%   
  filter(r5d2intvrage > 0) %>% 
  count(r5d2intvrage) %>% 
  mutate(r5d2intvrage = as.factor(r5d2intvrage),
         r5d2intvrage = fct_reorder(r5d2intvrage, n)) %>% 
  plot_ly(x = ~r5d2intvrage, y = ~n, color = ~r5d2intvrage, type = "bar", colors = "viridis") %>% 
  layout(plot_bgcolor = "f8f8f8",
         xaxis = list(title = "participant age", tickfont = list(size = 11)), 
         yaxis = list(title = "frequency"), 
         title = "Age",  
         titlefont = list(size = 16),
         showlegend = FALSE)

race_bar = 
  nhats  %>%   
  filter(rl5dracehisp > 6) %>% 
  count(rl5dracehisp) %>% 
  mutate(rl5dracehisp = as.factor(rl5dracehisp),
         rl5dracehisp = fct_reorder(rl5dracehisp, n)) %>% 
  plot_ly(x = ~rl5dracehisp, y = ~n, color = ~rl5dracehisp, type = "bar", colors = "viridis") %>% 
  layout(plot_bgcolor = "f8f8f8",
         xaxis = list(title = "participant race/ethnicity", tickfont = list(size = 11)), 
         yaxis = list(title = "frequency"), 
         title = "Race/Ethnicity",  
         titlefont = list(size = 16),
         showlegend = FALSE)

demo_bar = subplot(gender_bar, age_bar, race_bar, nrows = 3) %>%
  layout(xaxis3 = list(title = "categories"), 
         yaxis2 = list(title = "frequency"),
         title = list(text = "Demographic frequencies"), 
         titlefont = list(size = 14),
         legend = list(font = list(size = 10)))
```

    ## Warning: The titlefont attribute is deprecated. Use title = list(font = ...)
    ## instead.

    ## Warning: The titlefont attribute is deprecated. Use title = list(font = ...)
    ## instead.

    ## Warning: The titlefont attribute is deprecated. Use title = list(font = ...)
    ## instead.

``` r
demo_bar 
```

    ## Warning: The titlefont attribute is deprecated. Use title = list(font = ...)
    ## instead.

<div id="htmlwidget-22e9fb2a5015cf58beb2" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-22e9fb2a5015cf58beb2">{"x":{"data":[{"x":["1"],"y":[3140],"type":"bar","name":"1","marker":{"color":"rgba(68,1,84,1)","line":{"color":"rgba(68,1,84,1)"}},"textfont":{"color":"rgba(68,1,84,1)"},"error_y":{"color":"rgba(68,1,84,1)"},"error_x":{"color":"rgba(68,1,84,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2"],"y":[4344],"type":"bar","name":"2","marker":{"color":"rgba(253,231,37,1)","line":{"color":"rgba(253,231,37,1)"}},"textfont":{"color":"rgba(253,231,37,1)"},"error_y":{"color":"rgba(253,231,37,1)"},"error_x":{"color":"rgba(253,231,37,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["6"],"y":[725],"type":"bar","name":"6","marker":{"color":"rgba(68,1,84,1)","line":{"color":"rgba(68,1,84,1)"}},"textfont":{"color":"rgba(68,1,84,1)"},"error_y":{"color":"rgba(68,1,84,1)"},"error_x":{"color":"rgba(68,1,84,1)"},"xaxis":"x2","yaxis":"y2","frame":null},{"x":["5"],"y":[1017],"type":"bar","name":"5","marker":{"color":"rgba(65,68,135,1)","line":{"color":"rgba(65,68,135,1)"}},"textfont":{"color":"rgba(65,68,135,1)"},"error_y":{"color":"rgba(65,68,135,1)"},"error_x":{"color":"rgba(65,68,135,1)"},"xaxis":"x2","yaxis":"y2","frame":null},{"x":["1"],"y":[1039],"type":"bar","name":"1","marker":{"color":"rgba(42,120,142,1)","line":{"color":"rgba(42,120,142,1)"}},"textfont":{"color":"rgba(42,120,142,1)"},"error_y":{"color":"rgba(42,120,142,1)"},"error_x":{"color":"rgba(42,120,142,1)"},"xaxis":"x2","yaxis":"y2","frame":null},{"x":["4"],"y":[1395],"type":"bar","name":"4","marker":{"color":"rgba(34,168,132,1)","line":{"color":"rgba(34,168,132,1)"}},"textfont":{"color":"rgba(34,168,132,1)"},"error_y":{"color":"rgba(34,168,132,1)"},"error_x":{"color":"rgba(34,168,132,1)"},"xaxis":"x2","yaxis":"y2","frame":null},{"x":["3"],"y":[1561],"type":"bar","name":"3","marker":{"color":"rgba(122,209,81,1)","line":{"color":"rgba(122,209,81,1)"}},"textfont":{"color":"rgba(122,209,81,1)"},"error_y":{"color":"rgba(122,209,81,1)"},"error_x":{"color":"rgba(122,209,81,1)"},"xaxis":"x2","yaxis":"y2","frame":null},{"x":["2"],"y":[1747],"type":"bar","name":"2","marker":{"color":"rgba(253,231,37,1)","line":{"color":"rgba(253,231,37,1)"}},"textfont":{"color":"rgba(253,231,37,1)"},"error_y":{"color":"rgba(253,231,37,1)"},"error_x":{"color":"rgba(253,231,37,1)"},"xaxis":"x2","yaxis":"y2","frame":null},{"type":"bar","marker":{"color":"rgba(188,189,34,1)","line":{"color":"rgba(188,189,34,1)"}},"error_y":{"color":"rgba(188,189,34,1)"},"error_x":{"color":"rgba(188,189,34,1)"},"xaxis":"x3","yaxis":"y3","frame":null}],"layout":{"xaxis":{"domain":[0,1],"automargin":true,"tickfont":{"size":11},"type":"category","categoryorder":"array","categoryarray":["1","2"],"anchor":"y"},"xaxis2":{"domain":[0,1],"automargin":true,"tickfont":{"size":11},"type":"category","categoryorder":"array","categoryarray":["6","5","1","4","3","2"],"anchor":"y2"},"xaxis3":{"domain":[0,1],"automargin":true,"tickfont":{"size":11},"anchor":"y3","title":"categories"},"yaxis3":{"domain":[0,0.313333333333333],"automargin":true,"anchor":"x3"},"yaxis2":{"domain":[0.353333333333333,0.646666666666667],"automargin":true,"anchor":"x2","title":"frequency"},"yaxis":{"domain":[0.686666666666667,1],"automargin":true,"anchor":"x"},"annotations":[],"shapes":[],"images":[],"margin":{"b":40,"l":60,"t":25,"r":10},"plot_bgcolor":"f8f8f8","title":{"text":"Demographic frequencies"},"titlefont":{"size":14},"showlegend":false,"hovermode":"closest","legend":{"font":{"size":10}}},"attrs":{"126e538c4e59":{"x":{},"y":{},"color":{},"colors":"viridis","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"},"126e5706bb6e0":{"x":{},"y":{},"color":{},"colors":"viridis","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"},"126e52fb365c9":{"x":{},"y":{},"color":{},"colors":"viridis","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"subplot":true,"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
