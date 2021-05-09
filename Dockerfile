#VenueEnd Dockerfile

FROM matty8salisbury/shinymenu_order_basis_v2

MAINTAINER Matt Salisbury "matty8salisbury@gmail.com"

RUN apt-get update

RUN mkdir /root/PubEnd
COPY PubEnd /root/PubEnd
RUN mkdir /root/CheckClosedOrders
COPY CheckClosedOrders /root/CheckClosedOrders
COPY PubEnd/Rprofile.site usr/local/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "source('/root/PubEnd/venueinfo.R'); print(Sys.getenv('SQL_ENDPOINT'); shiny::runApp('/root/PubEnd')"]
#CMD ["R", "-e", "source('/root/PubEnd/venueinfo.R'); print(Sys.getenv('SQL_ENDPOINT'); shiny::runApp('/root/CheckClosedOrders')"]
