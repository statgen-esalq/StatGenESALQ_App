These tools are in constant development by members of ESALQ/USP Statistical Genetics Laboratory. Feel free to contribute sending pull requests or contacting one of us.

This shiny app performs statistical genetics analysis for diverse purposes. Please, check the left menu items for each feature detailed description.

Features implemented by now:

* Single environment analysis

* Multi environment analysis

* Assumptions tests

* Selection indices

## Quick start

There are four ways of using this app:

* (Exclusive for courses) Access: [https://statgen.esalq.usp.br/epagriApp/](https://statgen.esalq.usp.br/epagriApp/)

We use this App for didactic purposes. In our server version, we store real data as examples for our courses. If you are participating of some of or courses, please send us an e-mail requesting your login and password (chtaniguti in usp.br).

* (Public access) Install the package locally and run the app:

```{r, eval=FALSE}
devtools::install_github("statgen-esalq/StatGenESALQ_App")
library(StatGenESALQ)
run_app()
```

Because this shiny App is made using [Golem](https://golemverse.org/), it works like a package and the dependencies will be automatically installed. 

* (Public access) Run directly from GitHub:

```{r, eval=FALSE}
runGitHub("statgen-esalq/StatGenESALQ_App", ref="main")
```

**PS:** [Why "main" and not "master"](https://www.zdnet.com/article/github-to-replace-master-with-main-starting-next-month/#:~:text=GitHub%20repositories%20are%20where%20users,of%20a%20source%20code%20repository.&text=1%2C%202020%2C%20any%20new%20repositories,master%2C%22%20the%20company%20said.).

It will require that the dependencies packages are already installed.

* (Public access) Install [Docker](https://www.docker.com/get-started) and access the app via container:

```{bash, eval=FALSE}
docker run --rm -e USERID=$(id -u) -e GROUPID=$(id -g) -p 80:80 -e DISABLE_AUTH=true cristaniguti/statgenapp
```

The App will be available in the port `localhost:80` or `127.0.0.1:80`. Using container you will not need any installation but Docker (you don't even need to have R installed). It works in any operational system.

