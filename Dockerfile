FROM rocker/shiny-verse

RUN apt-get update && apt-get install -y  \
    	                libxml2-dev \
  		                imagemagick \
		                  libcurl4-openssl-dev \
		                  libgl1-mesa-dev \
		                  libglu1-mesa-dev \
		                  libicu-dev \
		                  libpng-dev \
		                  libssl-dev \
		                  make \
		                  pandoc \
		                  pandoc-citeproc \
		                  zlib1g-dev

RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.4")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.1.1")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.16")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("ggfortify",upgrade="never", version = "0.4.11")'
RUN Rscript -e 'remotes::install_version("lmtest",upgrade="never", version = "0.9-38")'
RUN Rscript -e 'remotes::install_version("car",upgrade="never", version = "3.0-10")'
RUN Rscript -e 'remotes::install_version("psych",upgrade="never", version = "2.0.12")'
RUN Rscript -e 'remotes::install_version("BiocManager",upgrade="never", version = "1.30.10")'
RUN Rscript -e 'remotes::install_version("sommer",upgrade="never", version = "4.1.2")'
RUN Rscript -e 'remotes::install_version("AGHmatrix",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'BiocManager::install("multtest")'

COPY ./ /tmp/app/
RUN R -e 'remotes::install_local("/tmp/app")'
EXPOSE 80/tcp
RUN rm /srv/shiny-server/index.html
COPY ./inst/app /srv/shiny-server/
COPY ./inst/app/shiny-server.conf /etc/shiny-server/shiny-server.conf

CMD ["/usr/bin/shiny-server"]

