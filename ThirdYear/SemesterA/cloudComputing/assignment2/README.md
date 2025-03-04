# A REST Application to Manage Stocks Portfolio

## Stocks service:
* manage stocks in a given portfolio
* retrieve the price of stocks on a given day
* compute the value of the portfolio based on the lastest recorded prices
* Uses a 3rd party API (Ninjas) to retrieve stock prices

## Using Docker Compose to create the application that is built from 5 services:
* 2 instances of the stocks service
* an additional capital-gains service
* a database service with MongoDB to persist data (downloaded from DockerHub)
* a reverse-proxy service to route requests to the right server (downloaded from DockerHub)

***Docker Compose will restart stocks services after a failure (and process
requests as if it never failed)

## Full Application Architecture:

<img width="599" alt="image" src="https://github.com/user-attachments/assets/a622ae02-9733-4065-a636-9247782da8d9" />
