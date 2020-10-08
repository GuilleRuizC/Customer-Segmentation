

function(input, output, session){
  observe({
#--- location ---------------------------------------------------------------------------------------------------    
    output$location <- renderPlot(
      df %>%  # PUT TITLE IN THE MIDDLE
        group_by(.,city) %>%
        ggplot(aes(x = city, y = spending)) +
        theme_minimal() +
        geom_boxplot(aes(color = city)) +   
        ylab('Total sales') +
        ggtitle('Sales per city') +
        theme(axis.text=element_text(size=20),
              axis.title=element_text(size=20),
              axis.title.x = element_blank(),
              plot.title = element_text(size = 20)) +
        theme(legend.position = "none") +
        theme(plot.title = element_text(hjust = 0.5))
        
    )
#--- Time ----------------------------------------------------------------------------------------------------- 
    output$time <- renderPlot(
      if (input$time == 1) {
      df %>% # Fails order in which days of week are displayed. # Not able to do a line plot
        mutate(., `date` = as.Date(`date`, format = "%m/%d/%y")) %>% 
        mutate(., `day` = weekdays(`date`)) %>%  
        group_by(., `day`) %>%  
        summarise(., `day`, `sales` = mean(`spending`)) %>% 
        distinct(., `day`, `sales`) %>% 
        mutate(., `num` = ifelse(`day` == 'Monday',1,ifelse((`day` == 'Tuesday'),2,ifelse((`day` == 'Wednesday'),3,ifelse((`day` == 'Thursday'),4,ifelse((`day` == 'Friday'),5,ifelse((`day` == 'Saturday'),6,7))))))) %>% 
        arrange(., `num`) %>% 
        as.data.frame() %>%
        ggplot(.,aes(x = factor(`day`, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')), y = `sales`)) +
        geom_line(aes(group = 1), color = 'darkblue') +
        ylab('Total sales') +
        xlab('Weekdays') +
        ggtitle('Average sales per weekday') +
        theme_minimal() + 
        theme(axis.text=element_text(size=20),
              axis.title=element_text(size=20),
              axis.title.x = element_blank(),
              plot.title = element_text(size = 20)) +
        theme(plot.title = element_text(hjust = 0.5))  
           
      
      }else {
        df %>%
        summarise(spending, time.intervals = ifelse(time < 11, 10, ifelse((time >= 11 & time < 12), 11, ifelse((time >=12 & time < 13),12, ifelse((time >= 13 & time < 14),13,ifelse((time >= 14 & time < 15),14,ifelse((time >= 15 & time < 16), 15, ifelse((time >= 16 & time < 17),16,ifelse((time >= 16 & time < 17),16,ifelse((time >= 17 & time < 18), 17,ifelse((time >= 18 & time < 19), 18,ifelse((time >= 19 & time <20),19,20)))))))))))) %>% 
        group_by(time.intervals) %>% 
        summarise(time.intervals, sales = mean(spending)) %>% 
        distinct(.,time.intervals,sales) %>%
        ggplot(aes(x = time.intervals, y = sales)) +
        geom_line(stat = 'identity', color = 'darkblue') +
        theme_minimal() +  
        ylab('Total sales') +
        xlab('Daily hours') +
        ggtitle('Average sales per hour') +
        theme(axis.text=element_text(size=20),
              axis.title=element_text(size=20),
              plot.title = element_text(size = 20)) +
        theme(plot.title = element_text(hjust = 0.5))}
    )    
#--- Sales overview ----------------------------------------------------------------------------------------------------- 
    output$salesoverview <- renderPlot(
      if (input$salesoverview == 1) {
        df %>% 
          group_by(gender) %>% 
          summarise(gender, unit.price = mean(unit.price), quantity = mean(quantity), spending) %>% 
          distinct(gender, quantity, unit.price, spending) %>% 
          ggplot(aes(x = gender, y = spending)) +
          geom_bar(aes(fill = gender), stat = 'identity', width = 0.35) +
          ylab('Total sales') +
          xlab('Gender') + 
          ggtitle('Total sales per gender') +
          theme_minimal() +
          theme(axis.text=element_text(size=20),
                axis.title=element_text(size=20),
                plot.title = element_text(size = 20)) +
          theme(plot.title = element_text(hjust = 0.5)) +
          theme(legend.position = "none") 
        
      }else if(input$salesoverview == 2){
        df %>%
          group_by(product.type) %>%
          summarise(spending = sum(spending), product.type) %>% 
          distinct(product.type, spending) %>%
          mutate_if(is.numeric, round) %>% 
          ggplot(aes(y = product.type, x = spending, label = spending)) +
          theme_minimal() +
          geom_segment(aes(x = 0, y = product.type, xend = spending, yend = product.type), color = 'darkblue') +
          geom_point(size = 20, color = 'darkblue') + 
          geom_text(color = 'white', size = 3.5) +
          ylab('Type of product') +
          xlab('Total sales') +
          ggtitle('Total sales per type of product') +
          theme_minimal() +
          theme(axis.text=element_text(size=20),
                axis.title=element_text(size=20),
                plot.title = element_text(size = 20)) +
          theme(plot.title = element_text(hjust = 0.5))
        
      }else if (input$salesoverview == 3){
        df %>% 
          group_by(., rating) %>% 
          summarise(rating, spending = mean(spending)) %>% 
          distinct(rating, spending) %>% 
          ggplot(aes(x = rating, y = spending )) +
          theme_minimal() +
          geom_point(color = 'darkblue') +
          geom_smooth(method = 'lm') +
          ylab('Total sales') +
          xlab('Rating') +
          ggtitle('Correlation between total sales and rating') +
          theme(axis.text=element_text(size=20),
                axis.title=element_text(size=20),
                plot.title = element_text(size = 20)) +
          theme(plot.title = element_text(hjust = 0.5))
        
      }else {
        df %>% 
          ggplot(aes(x = customer.type, y = spending)) +
          theme_minimal() +
          geom_boxplot(color = 'darkblue') +
          ylab('Total Sales') +
          xlab('Membership') +
          ggtitle('Total sales per membership') +
          theme(axis.text=element_text(size=20),
                axis.title=element_text(size=20),
                plot.title = element_text(size = 20)) +
          theme(plot.title = element_text(hjust = 0.5))}
    )  
#--- Gender ----------------------------------------------------------------------------------------------------- 
    output$gender <- renderPlot(
      if (input$gender == 1) {
        df %>%
          group_by(gender, product.type) %>%
          summarise(spending = sum(spending), gender, product.type) %>% 
          distinct(product.type, spending, gender) %>%
          mutate_if(is.numeric, round) %>% 
          ggplot(aes(y = product.type, x = spending, label = spending, color = gender)) +
          theme_minimal() +
          geom_segment(aes(x = 0, y = product.type, xend = spending, yend = product.type)) +
          geom_point(size = 20) + 
          geom_text(color = 'white', size = 3.5) +
          ylab('Type of product') +
          xlab('Total sales') +
          ggtitle('Total sales per type of product and gender') +
          theme(axis.text=element_text(size=20),
                axis.title=element_text(size=20),
                plot.title = element_text(size = 20)) + 
          theme(plot.title = element_text(hjust = 0.5))
        
      }else{
        df %>% 
          group_by(gender, customer.type) %>% 
          ggplot(aes(x = customer.type, y = spending)) +
          theme_minimal() +
          geom_bar(stat = 'identity', position = 'dodge', aes(fill = gender)) +
          xlab('Membership') +
          ylab('Total sales') +
          ggtitle('Membership by gender') +
          theme(axis.text=element_text(size=20),
                axis.title=element_text(size=20),
                plot.title = element_text(size = 20)) +
          theme(plot.title = element_text(hjust = 0.5))
      }
    )
    
#--- Membership ----------------------------------------------------------------------------------------------------- 
    output$membership <- renderPlot(
      df %>%
        group_by(customer.type, product.type) %>%
        summarise(spending = sum(spending), customer.type, product.type) %>% 
        distinct(product.type, spending, customer.type) %>%
        mutate_if(is.numeric, round) %>% 
        ggplot(aes(y = product.type, x = spending, label = spending, color = customer.type)) +
        theme_minimal() +
        geom_segment(aes(x = 0, y = product.type, xend = spending, yend = product.type)) +
        geom_point(size = 20) + 
        geom_text(color = 'white', size = 3.5) +
        ylab('Type of product') +
        xlab('Total sales') +
        ggtitle('Total sales per type of product and membership') +
        theme(axis.text=element_text(size=20),
              axis.title=element_text(size=20),
              plot.title = element_text(size = 20)) +
        theme(plot.title = element_text(hjust = 0.5))
    )
#--- Rating -----------------------------------------------------------------------------------------------------
    output$rating <- renderPlot(
      df %>% 
        ggplot(aes(x = customer.type, y = rating)) +
        theme_minimal() +
        geom_boxplot(color = 'darkblue') +
        ylab('Rating') +
        xlab('Membership') +
        ggtitle('Membership by rating') +
        theme(axis.text=element_text(size=20),
              axis.title=element_text(size=20),
              plot.title = element_text(size = 20)) +
        theme(plot.title = element_text(hjust = 0.5)))
  })}
