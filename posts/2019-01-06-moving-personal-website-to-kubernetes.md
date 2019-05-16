---
layout: post
title: "Moving Personal Website to Kubernetes"
date: 2019-01-06 17:30
comments: true
categories: 
---

I've read people talking about Kubernetes for some time, and it does sound appealing to me when I know that it incorporates most of the design from Google's Borg. Due to the needs from my job, I started reading the materials from Kubernetes and was intrigued to study the underlying implementation, on how it designs its container network and orchestration. It's very complicated but also rewarding to get the knowledge of that. In order to get my hands dirty on exercise, I used DigitalOcean's managed Kubernetes and migrate my personal website from a Chef managed instance to Docker. It's simple enough for me to understand the basic concept and make me able to dabble it and to understand how complicated it is to have a TLS static website setup.

It's quite easy to launch a managed Kubernetes on DigitalOcean, a few clickthrough then I have two instances running my dockers. I created a base docker image just to host my personal website's static html. And a custom setting with virtual hosts on nginx so that I could forward the domain managed by me to redirect to the right places.

```
FROM nginx
EXPOSE 80
COPY _site/ /usr/share/nginx/html
COPY default.conf /etc/nginx/conf.d/default.conf
```

For the running service, it is not hard. By copying the standard Service and Deployment object from Kubernetes would suffice to get it done.

```
apiVersion: v1
kind: Service
metadata:
  name: homepage 
spec:
  ports:
  - port: 80
    targetPort: 80 
  selector:
    app: homepage 
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: homepage 
spec:
  selector:
    matchLabels:
      app: homepage 
  replicas: 2
  template:
    metadata:
      labels:
        app: homepage
    spec:
      containers:
      - name: homepage
        image: paulmeng/homepage:v1.0.1
        imagePullPolicy: Always
        ports:
        - containerPort: 5678
```

It is more difficult to understand the Ingress, since I would like to have the letsencrypt to terminate at the load balancer layer. Fortunately, DigitalOcean have a tutorial setup on how to decorate the Ingress spec with cert manager, and install the necessary component by helm.

How to Set Up an Nginx Ingress with Cert-Manager on DigitalOcean Kubernetes | DigitalOcean
Kubernetes Ingresses allow you to flexibly route traffic from outside your Kubernetes cluster to Services inside of…www.digitalocean.com

By referencing the tutorial, I managed to change the templates to suit my needs.

```
apiVersion: certmanager.k8s.io/v1alpha1
kind: ClusterIssuer
metadata:
  name: letsencrypt-prod
spec:
  acme:
    # The ACME server URL
    server: https://acme-v02.api.letsencrypt.org/directory
    # Email address used for ACME registration
    email: me@paulme.ng 
    # Name of a secret used to store the ACME account private key
    privateKeySecretRef:
      name: letsencrypt-prod
    # Enable the HTTP-01 challenge provider
    http01: {}

apiVersion: extensions/v1beta1
kind: Ingress
metadata:
  name: homepage-ingress
  annotations:
    kubernetes.io/ingress.class: nginx
    certmanager.k8s.io/cluster-issuer: letsencrypt-prod
spec:
  tls:
  - hosts:
    - paulme.ng
    - haskell.tw
    - haskell.sg
    secretName: letsencrypt-prod
  rules:
  - host: paulme.ng
    http:
      paths:
      - backend:
          serviceName: homepage
          servicePort: 80
  - host: haskell.sg 
    http:
      paths:
      - backend:
          serviceName: homepage
          servicePort: 80
  - host: haskell.tw 
    http:
      paths:
      - backend:
          serviceName: homepage
          servicePort: 80
```

Overall it works, with HTTP port auto-redirected to TLS, but somehow the Certmanager kept failing on the ACME challenge required by Letsencrypt. I did make sure I updated my DNS record to the correct External IP address exposed by DigitalOcean load balancer. When I run

```
kubectl describe ingress
```

It kept saying the ACME test01 failed, but I couldn't manage to find the more detailed log by either running kubectl log or others. It made the troubleshooting extremely hard and you have no idea why it failed. Surprisingly, when I just left it there for one day due to I need to do the work at day time. It is just automatically fixed, and the same kind of issue was reported on certmanager's github issue tracker. I am not sure it is due to the implementation for certmanager is bad or where there are critical informations that I left out.

Kubernetes is an interesting technology to learn, especially when you would like to manage your app by container. It is a proven container orchestration technology. However, the underlying system is quite complicated and you have to bend your head understand how the internals work, otherwise you would left blind out when the systems go unexpected output.
