FROM ghcr.io/opcruz/r-base:sha-842f822

RUN sudo echo "nameserver 8.8.8.8"> /etc/resolv.conf \
        && R -e "install.packages(c('shiny'), repos='https://cloud.r-project.org/')"

RUN addgroup --system app \
    && adduser --system --ingroup app app

WORKDIR /home/app

COPY app .

RUN chown app:app -R /home/app

USER app

CMD ["Rscript", "script.R"]
