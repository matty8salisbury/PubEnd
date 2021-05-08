#VenueEnd Dockerfile

FROM matty8salisbury/shinymenu_order_basis_v2

MAINTAINER Matt Salisbury "matty8salisbury@gmail.com"

RUN apt-get update

RUN mkdir /root/PubEnd
COPY OrderApp /root/PubEnd
COPY venueinfo.R /root/venueinfo.R

EXPOSE 3838

CMD ["R", "-e", "source('/root/venueinfo.R'); shiny::runApp('/root/PubEnd')"]