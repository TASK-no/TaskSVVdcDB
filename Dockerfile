FROM rocker/shiny-verse
RUN install2.r rsconnect golem htmltools config writexl shiny.semantic gargoyle
RUN install2.r reactable magrittr plotly auth0 hunspell spelling
RUN installGithub.r TASK-no/TaskAnalyticsTB selva86/InformationValue \
&& rm -rf /tmp/downloaded_packages/
WORKDIR /home/TaskSVVdcDB
COPY app.R app.R
COPY deploy.R deploy.R
COPY DESCRIPTION DESCRIPTION
COPY NAMESPACE NAMESPACE
ADD data data
ADD inst inst
ADD R R
CMD Rscript deploy.R
