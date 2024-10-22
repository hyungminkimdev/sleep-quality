# sleep-quality
Design, analysis, and evaluation of a sleep quality using Bayesian Network

### How to Run the Code
The code is written in order.
If you run the code in order from top to bottom, the data will be stored sequentially.

### How to use model to infer Sleep Quality
cpquery(model, event, evidence)

1. In model, put model.
2. In event, put the variable you want to know with condition
3. In evidence, put the evidence that you already know

(e.g.) predicted_1 <- cpquery(bn_8_model, event = (Sleep.Quality.Rating=="1"), evidence =(Age.group == "20-29" & Gender == "m" & Dietary.Habits == "unhealthy" & Bedtime == "01:15" & Sleep.Disorders == "yes")) 
