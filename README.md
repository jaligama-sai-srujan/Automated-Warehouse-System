# Automated Warehouse System

# Description

Automated Warehouse System Scenario is a simplified version of automated planning done in warehouses. The objective of this project is to make robots find the optimal path to bring a shelf which is at a particular location to a specified picking station to deliver an order of a product that is present on the shelf by dividing whole warehouse into the Grid layout. The logical rules for the whole process is built using Clingo.

# Files Description

AW_final.asp :- File Contains Rules Used to automate the whole delivery process.
inst*.asp :- These are the file used to create the instance of the warehouse. All the entities initial position are given in this files.

# Steps to Run the Application
1) Install Clingo
2) Create instance file or use the existing instance file.
3) Open command prompt or terminal
4) Redirect to the project folder.
5) execute the command "clingo aw_final.asp $InstanceFile -c m=13". To provide the number of maximum number of steps to process all orders change the value of m. If no stable models are produced increase the value of m as number of steps are not sufficient to process all orders.  
