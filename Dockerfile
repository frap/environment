FROM agasson/environment:archlinux
MAINTAINER Andres Gasson <gas@tuatara.red>
WORKDIR /opt/eru
COPY ./eru-new.sh /opt/eru/eru-new.sh
RUN ./eru-new.sh
