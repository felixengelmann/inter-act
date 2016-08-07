
############
library(shiny)
runApp()

# install.packages('rsconnect')
# library(rsconnect)
# rsconnect::setAccountInfo(name="engelmann", token="93FC7E948A3738BD9F6A8FFF78F0DFB1", secret="XlRlj2vIxdHnMKEFvMvbk9G0bMZ84CxUK5uk9p20")

library(rsconnect)
deployApp(account="engelmann", appName="inter-act")
