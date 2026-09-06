# Serves the generated site (_site/) with nginx. Build the site first:
#   stack run -- build && docker build -t haspot .
FROM nginx:stable-alpine
COPY default.conf /etc/nginx/conf.d/default.conf
COPY _site/ /usr/share/nginx/html/
EXPOSE 80
