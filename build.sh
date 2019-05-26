VERSION=v1.0.22

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
