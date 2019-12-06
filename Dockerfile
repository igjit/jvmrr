FROM rocker/tidyverse:3.6.1

RUN install2.r --error \
    dequer \
 && rm -rf /tmp/downloaded_packages/
