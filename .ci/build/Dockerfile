FROM ghcr.io/opcruz/r-base:sha-842f822
LABEL maintainer="opcruz <orestesperez1995@gmail.com>"

# RUN sudo echo "nameserver 172.17.16.13"> /etc/resolv.conf \
#         && R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"
RUN sudo echo "nameserver 8.8.8.8"> /etc/resolv.conf \
        && R -e "install.packages(c('devtools'), repos='https://cloud.r-project.org/')"
#         && apt-get update
#         R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"
# RUN apt-get update && apt-get install -y --no-install-recommends \
#     sudo \
#     libcurl4-gnutls-dev \
#     libcairo2-dev \
#     libxt-dev \
#     libssl-dev \
#     libssh2-1-dev \
#     && rm -rf /var/lib/apt/lists/*
# RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"
# RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
RUN addgroup --system app \
    && adduser --system --ingroup app app
WORKDIR /home/app
# COPY script.R .
# RUN chown app:app -R /home/app
# USER app
# EXPOSE 3838
CMD ["R"]