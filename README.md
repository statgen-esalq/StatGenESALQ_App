---
title: "StatGen- ESALQ App"
output: html_document
---

This tools are in constant development by members of ESALQ/USP Statistical Genetics Laboratory. Feel free to contribute sending pull requests or contacting one of us.

This shiny app performs statistical genetics analysis for diverse purposes. Please, check the left menu items for each feature detailed description.

Features implemented by now:

* Single environment analysis

* Multi environment analysis

* Assumptions tests

* Selection indices

## Quick start

There are three ways of using this app:

* (Exclusive for courses) Access: [https://statgen.esalq.usp.br/epagriApp/](https://statgen.esalq.usp.br/epagriApp/)

We use this App for didactic purposes. In our server version, we store real data as examples for our courses. If you are participating of some of or courses, please send us an e-mail requesting your login and password (chtaniguti in usp.br).

* (Public access) Run directly from github:

```{r, eval=FALSE}
runGitHub("statgen-esalq/StatGenESALQ_App")
```

Because this shiny App is made using [Golem](https://golemverse.org/), it works like a package and the dependencies will be automatically installed. 

* (Public access) Install [Docker](https://www.docker.com/get-started) and access the app via container:

```{bash, eval=FALSE}
docker run cristaniguti/statgenapp
```

The App will be available in the port `localhost:80` or `127.0.0.1:80`. Using container you will not require any installation but Docker (you don't even need to have R installed). It works in any operational system.
