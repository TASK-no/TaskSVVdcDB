FROM rocker/shiny-verse
RUN install2.r rsconnect golem htmltools config writexl shiny.semantic gargoyle
RUN install2.r reactable magrittr plotly auth0 hunspell spelling
RUN installGithub.r TASK-no/TaskAnalyticsTB \
&& rm -rf /tmp/downloaded_packages/
WORKDIR /home/TaskSVVdcDB
COPY app.R app.R
COPY deploy.R deploy.R
COPY DESCRIPTION DESCRIPTION
COPY NAMESPACE NAMESPACE
ADD R R
ADD data data
CMD Rscript deploy.R