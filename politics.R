##########################
### R-script for 
###  
### Decision trees and random forests: 
### Machine learning techniques to classify rare events
###
### European Policy Analysis
### Simon Hegelich
###
### Contact: 
### Prof. Dr. Simon Hegelich
### Political Data Science
### Technical University of Munich TUM
### Bavarian School of Public Policy
### Email: hegelich@hfpm.de
### http://politicaldatascience.blogspot.de
##########################
 
### Dowlnoading data from the Policy Agendas Project
### This can take some time and requires a good internet connection
budget = read.csv("http://www.utexas.edu/cola/_webservices/policyagendas/budget/instances.csv?from=1945&to=2014", as.is=T)
congress = read.csv("http://www.utexas.edu/cola/_webservices/policyagendas/ch/instances.csv?from=1945&to=2014", as.is=T)
laws = read.csv("http://www.utexas.edu/cola/_webservices/policyagendas/pl/instances.csv?from=1945&to=2014", as.is=T)
rowCall = read.csv("http://www.utexas.edu/cola/_webservices/policyagendas/rc/instances.csv?from=1945&to=2014", as.is=T)
eo = read.csv("http://www.utexas.edu/cola/_webservices/policyagendas/eo/instances.csv?from=1945&to=2014", as.is=T)
sou = read.csv("http://www.utexas.edu/cola/_webservices/policyagendas/sou/instances.csv?from=1945&to=2014", as.is=T)
supCourt = read.csv("http://www.utexas.edu/cola/_webservices/policyagendas/sc/instances.csv?from=1945&to=2014", as.is=T)
gallup = read.csv("http://www.utexas.edu/cola/_webservices/policyagendas/mip/instances.csv?from=1945&to=2014", as.is=T)
 
### The next table was constructed by the author.
### It show matches in topics and budgets
TopicsCross = read.csv("TopicsCross.csv", as.is=T)
